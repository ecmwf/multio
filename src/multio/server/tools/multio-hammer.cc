
#include <algorithm>

#include "eckit/mpi/Comm.h"

#include "eckit/config/YAMLConfiguration.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/log/Log.h"
#include "eckit/option/CmdArgs.h"
#include "eckit/option/SimpleOption.h"

#include "multio/server/Listener.h"
#include "multio/server/LocalIndices.h"
#include "multio/server/Message.h"
#include "multio/server/MultioServerTool.h"
#include "multio/server/PlanConfigurations.h"
#include "multio/server/Peer.h"
#include "multio/server/print_buffer.h"
#include "multio/server/TestData.h"
#include "multio/server/Transport.h"

using eckit::Log;
using namespace multio::server;

//----------------------------------------------------------------------------------------------------------------------

class MultioHammer final : public multio::server::MultioServerTool {
public:  // methods

    MultioHammer(int argc, char** argv);

private:
    void usage(const std::string& tool) const override {
        Log::info() << std::endl
                    << "Usage: " << tool << " [options]" << std::endl
                    << std::endl
                    << std::endl
                    << "Examples:" << std::endl
                    << "=========" << std::endl
                    << std::endl
                    << tool << " --transport=mpi --nbclients=10 --nbservers=4" << std::endl
                    << std::endl;
    }

    void init(const eckit::option::CmdArgs& args) override;

    void execute(const eckit::option::CmdArgs& args) override;

    std::tuple<std::vector<Peer>, std::vector<Peer>> createPeerLists();

    void spawnServers(const std::vector<Peer>& serverPeers, std::shared_ptr<Transport> transport);

    void spawnClients(const std::vector<Peer>& clientPeers, const std::vector<Peer>& serverPeers,
                      std::shared_ptr<Transport> transport);

    std::string trType_ = "none";
    size_t nbClients_ = 1;
    int port_ = 7777;

    eckit::LocalConfiguration config_;
};

//----------------------------------------------------------------------------------------------------------------------

MultioHammer::MultioHammer(int argc, char** argv) : multio::server::MultioServerTool(argc, argv) {
    options_.push_back(
        new eckit::option::SimpleOption<std::string>("transport", "Type of transport layer"));
    options_.push_back(new eckit::option::SimpleOption<size_t>("nbclients", "Number of clients"));
    options_.push_back(new eckit::option::SimpleOption<size_t>("port", "TCP port"));
}

void MultioHammer::init(const eckit::option::CmdArgs& args) {
    MultioServerTool::init(args);
    args.get("transport", trType_);
    args.get("nbclients", nbClients_);
    args.get("port", port_);

    config_ = eckit::LocalConfiguration{eckit::YAMLConfiguration{plan_configurations(trType_)}};

    if (trType_ == "mpi") {
        auto domain_size = eckit::mpi::comm(config_.getString("domain").c_str()).size();
        if (domain_size != nbClients_ + nbServers_) {
            throw eckit::SeriousBug(
                "Number of MPI ranks does not match the number of clients and servers");
        }
    }
}

//----------------------------------------------------------------------------------------------------------------------

std::tuple<std::vector<Peer>, std::vector<Peer>> MultioHammer::createPeerLists() {
    std::vector<Peer> clientPeers;
    std::vector<Peer> serverPeers;

    if (trType_ == "mpi") {
        auto domain = config_.getString("domain");

        auto domain_size = nbClients_ + nbServers_;
        auto ii = 0u;
        while (ii != nbClients_) {
            clientPeers.push_back(Peer{domain.c_str(), ii++});
        }
        while (ii != domain_size) {
            serverPeers.push_back(Peer{domain.c_str(), ii++});
        }

        return std::make_tuple(clientPeers, serverPeers);
    }

    if (trType_ == "tcp") {
        for (auto cfg : config_.getSubConfigurations("servers")) {
            auto host = cfg.getString("host");
            for (auto port : cfg.getUnsignedVector("ports")) {
                serverPeers.push_back(Peer{host, port});
            }
        }

        for (auto cfg : config_.getSubConfigurations("clients")) {
            auto host = cfg.getString("host");
            for (auto port : cfg.getUnsignedVector("ports")) {
                clientPeers.push_back(Peer{host, port});
            }
        }

        nbClients_ = clientPeers.size();
        nbServers_ = serverPeers.size();

        return std::make_tuple(clientPeers, serverPeers);
    }

    ASSERT(trType_ == "thread"); // nothing else is supported

    return std::make_tuple(clientPeers, serverPeers);
}

void MultioHammer::spawnServers(const std::vector<Peer>& serverPeers,
                                std::shared_ptr<Transport> transport) {
    // Do nothing if current process is not in the list of servers
    if (find(begin(serverPeers), end(serverPeers), transport->localPeer()) == end(serverPeers)) {
        return;
    }

    Listener listener(config_, *transport);
    listener.listen();
}

void MultioHammer::spawnClients(const std::vector<Peer>& clientPeers,
                                const std::vector<Peer>& serverPeers,
                                std::shared_ptr<Transport> transport) {
    // Do nothing if current process is not in the list of clients
    auto it = find(begin(clientPeers), end(clientPeers), transport->localPeer());
    if (it == end(clientPeers)) {
        return;
    }

    Peer client = transport->localPeer();

    // open all servers
    for (auto& server : serverPeers) {
        Message open{{Message::Tag::Open, client, server}, std::string("open")};
        transport->send(open);
    }

    auto client_list_id = std::distance(begin(clientPeers), it);
    auto idxm = generate_index_map(client_list_id, nbClients_);
    eckit::Buffer buffer(reinterpret_cast<const char*>(idxm.data()), idxm.size() * sizeof(size_t));
    LocalIndices index_map{std::move(idxm)};

    // send partial mapping
    for (auto& server : serverPeers) {
        Message msg{{Message::Tag::Mapping, client, server, "unstructured", nbClients_}, buffer};

        transport->send(msg);
    }

    // send N messages
    const int nfields = 13;
    for (int ii = 0; ii < nfields; ++ii) {
        auto field_id = std::string("temperature::step::") + std::to_string(ii);
        std::vector<double> field;

        auto& global_field = global_test_field(field_id, field_size(), trType_, client_list_id);
        index_map.to_local(global_field, field);

        if (root() == client_list_id) {
            eckit::Log::info() << "   ---   Field: " << field_id << ", values: " << std::flush;
            print_buffer(global_field, eckit::Log::info(), " ");
            eckit::Log::info() << std::endl;
        }

        // Choose server
        auto id = std::hash<std::string>{}(field_id) % nbServers_;
        ASSERT(id < serverPeers.size());

        eckit::Buffer buffer(reinterpret_cast<const char*>(field.data()),
                             field.size() * sizeof(double));

        Message msg{{Message::Tag::Field, client, serverPeers[id], "unstructured", nbClients_,
                     "prognostic", field_id, field_size()},
                    buffer};

        transport->send(msg);
    }

    // close all servers
    for (auto& server : serverPeers) {
        Message close{{Message::Tag::Close, client, server}, std::string("close")};
        transport->send(close);
    }
}

//----------------------------------------------------------------------------------------------------------------------

void MultioHammer::execute(const eckit::option::CmdArgs&) {

    config_.set("local_port", port_);
    std::shared_ptr<Transport> transport{TransportFactory::instance().build(trType_, config_)};

    std::cout << *transport << std::endl;

    field_size() = 29;

    std::vector<Peer> clientPeers;
    std::vector<Peer> serverPeers;
    std::tie(clientPeers, serverPeers) = createPeerLists();

    spawnServers(serverPeers, transport);
    spawnClients(clientPeers, serverPeers, transport);
}

//----------------------------------------------------------------------------------------------------------------------

int main(int argc, char** argv) {
    MultioHammer tool(argc, argv);
    return tool.start();
}

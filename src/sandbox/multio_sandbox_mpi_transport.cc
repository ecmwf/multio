
#include <algorithm>

#include "eckit/mpi/Comm.h"

#include "eckit/config/YAMLConfiguration.h"
#include "eckit/log/Log.h"
#include "eckit/option/CmdArgs.h"
#include "eckit/option/SimpleOption.h"

#include "sandbox/TestData.h"
#include "sandbox/Listener.h"
#include "sandbox/Message.h"
#include "sandbox/MultioServerTool.h"
#include "sandbox/PlanConfigurations.h"
#include "sandbox/Peer.h"
#include "sandbox/print_buffer.h"
#include "sandbox/Transport.h"

using eckit::Log;
using namespace multio::sandbox;

//----------------------------------------------------------------------------------------------------------------------

class MpiExample final : public multio::sandbox::MultioServerTool {
public:  // methods

    MpiExample(int argc, char** argv);

private:
    void usage(const std::string& tool) const override {
        Log::info() << std::endl
                    << "Usage: " << tool << " [options]" << std::endl
                    << std::endl
                    << std::endl
                    << "Examples:" << std::endl
                    << "=========" << std::endl
                    << std::endl
                    << tool << " --nbservers 4" << std::endl
                    << std::endl;
    }

    std::vector<Peer> spawnServers(const eckit::Configuration& config,
                                   std::shared_ptr<Transport> transport);

    void spawnClients(std::shared_ptr<Transport> transport, const std::vector<Peer>& serverPeers);

    void init(const eckit::option::CmdArgs& args) override;

    void execute(const eckit::option::CmdArgs& args) override;

    size_t nbClients_ = 1;

    eckit::YAMLConfiguration config_;
};

//----------------------------------------------------------------------------------------------------------------------

MpiExample::MpiExample(int argc, char** argv) :
    multio::sandbox::MultioServerTool(argc, argv),
    config_(plan_configurations()) {}

void MpiExample::init(const eckit::option::CmdArgs& args) {
    MultioServerTool::init(args);
    auto domain_size = eckit::mpi::comm(config_.getString("domain").c_str()).size();
    ASSERT(nbServers_ < domain_size);
    nbClients_ = domain_size - nbServers_;
}

//----------------------------------------------------------------------------------------------------------------------

std::vector<Peer> MpiExample::spawnServers(const eckit::Configuration& config,
                                           std::shared_ptr<Transport> transport) {
    std::vector<Peer> serverPeers;

    auto domain = config.getString("domain");

    auto size = eckit::mpi::comm(domain.c_str()).size();
    for (auto ii = size - nbServers_; ii != size; ++ii) {
        serverPeers.push_back(Peer{domain.c_str(), ii});
    }

    auto it = std::find(begin(serverPeers), end(serverPeers), transport->localPeer());
    if (it != end(serverPeers)) {
        Listener listener(config, *transport);

        listener.listen();
    }

    return serverPeers;
}

namespace {

std::vector<size_t> generate_index_map(Peer peer, size_t nbclients) {
    auto id = peer.id_;  // OK for mpi; otherwise create a clientPeer list
    auto chunk_size = field_size() / nbclients + ((id < field_size() % nbclients) ? 1 : 0);

    auto maps = std::vector<size_t>(chunk_size);
    for (auto jj = 0u; jj != chunk_size; ++jj) {
        maps[jj] = static_cast<size_t>(id) + jj * nbclients;
    }
    return maps;
}

}  // namespace

void MpiExample::spawnClients(std::shared_ptr<Transport> transport,
                              const std::vector<Peer>& serverPeers) {
    // Do nothing if current rank is a server rank
    if (find(begin(serverPeers), end(serverPeers), transport->localPeer()) != end(serverPeers)) {
        return;
    }

    Peer client = transport->localPeer();

    // open all servers
    for (auto& server : serverPeers) {
        Message open{Message::Tag::Open, client, server, std::string("open")};
        transport->send(open);
    }

    auto idx = generate_index_map(client, nbClients_);

    // send partial mapping
    for (auto& server : serverPeers) {
        eckit::Buffer buffer(reinterpret_cast<const char*>(idx.data()), idx.size() * sizeof(size_t));

        Message msg{Message::Tag::Mapping, client, server, buffer, "scattered", nbClients_};

        transport->send(msg);
    }

    // send N messages
    const int nfields = 13;
    for (int ii = 0; ii < nfields; ++ii) {
        auto field_id = std::string("temperature step ") + std::to_string(ii);
        std::vector<double> field =
            create_local_field(global_test_field(field_id, field_size()), idx);

        if (root() == eckit::mpi::comm(client.domain_.c_str()).rank()) {
            eckit::Log::info() << "   ---   Field: " << field_id << ", values: " << std::flush;
            print_buffer(global_test_field(field_id, field_size()), eckit::Log::info(), " ");
            eckit::Log::info() << std::endl;
        }

        // Chose server
        auto idx = std::hash<std::string>{}(field_id) % nbServers_;
        ASSERT(idx < serverPeers.size());

        eckit::Buffer buffer(reinterpret_cast<const char*>(field.data()),
                             field.size() * sizeof(double));

        Message msg{Message::Tag::Field, client, serverPeers[idx], buffer, "scattered", nbClients_,
            "prognostic", field_id, field_size()};

        transport->send(msg);
    }

    // close all servers
    for (auto& server : serverPeers) {
        Message close{Message::Tag::Close, client, server, std::string("close")};
        transport->send(close);
    }
}

//----------------------------------------------------------------------------------------------------------------------

void MpiExample::execute(const eckit::option::CmdArgs&) {
    std::cout << "Clients: " << nbClients_ << std::endl << "Servers: " << nbServers_ << std::endl;

    std::shared_ptr<Transport> transport{TransportFactory::instance().build("Mpi", config_)};

    std::cout << *transport << std::endl;

    field_size() = 29;

    auto serverPeers = spawnServers(config_, transport);

    spawnClients(transport, serverPeers);
}

//----------------------------------------------------------------------------------------------------------------------

int main(int argc, char** argv) {
    MpiExample tool(argc, argv);
    return tool.start();
}

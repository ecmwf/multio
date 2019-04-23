
#include <algorithm>
#include <chrono>
#include <functional>
#include <iostream>
#include <mutex>
#include <string>
#include <thread>

#include "eckit/config/YAMLConfiguration.h"
#include "eckit/log/Log.h"
#include "eckit/option/CmdArgs.h"
#include "eckit/option/SimpleOption.h"
#include "eckit/net/TCPClient.h"
#include "eckit/net/TCPServer.h"
#include "eckit/runtime/Tool.h"

#include "multio/server/Listener.h"
#include "multio/server/LocalIndices.h"
#include "multio/server/Message.h"
#include "multio/server/MultioServerTool.h"
#include "multio/server/Peer.h"
#include "multio/server/PlanConfigurations.h"
#include "multio/server/print_buffer.h"
#include "multio/server/TestData.h"
#include "multio/server/Transport.h"

using namespace eckit;
using namespace multio;
using namespace multio::server;

//----------------------------------------------------------------------------------------------------------------------

class TcpExample final : public multio::server::MultioServerTool {
public:  // methods

    TcpExample(int argc, char** argv);

private:
    void usage(const std::string &tool) const override {
        Log::info() << std::endl
                    << "Usage: " << tool << " [options]" << std::endl
                    << std::endl
                    << std::endl
                    << "Examples:" << std::endl
                    << "=========" << std::endl
                    << std::endl
                    << tool << " --nbclients=5 --port=9771" << std::endl
                    << std::endl;
    }

    void init(const eckit::option::CmdArgs& args) override;

    void execute(const eckit::option::CmdArgs& args) override;

    std::tuple<std::vector<Peer>, std::vector<Peer>> createPeerLists(
        const eckit::Configuration& config);

    void spawnServers(const std::vector<Peer>& serverPeers, const eckit::Configuration& config,
                      std::shared_ptr<Transport> transport);

    void spawnClients(const std::vector<Peer>& clientPeers, const std::vector<Peer>& serverPeers,
                      std::shared_ptr<Transport> transport);

    size_t nbClients_ = 1;
    int port_ = 7777;

    eckit::YAMLConfiguration config_;
};

//----------------------------------------------------------------------------------------------------------------------

TcpExample::TcpExample(int argc, char** argv) :
    multio::server::MultioServerTool(argc, argv),
    config_(tcp_plan_configurations()) {
    options_.push_back(new eckit::option::SimpleOption<size_t>("port", "TCP port"));
}

void TcpExample::init(const option::CmdArgs& args) {
    args.get("port", port_);
}

//----------------------------------------------------------------------------------------------------------------------

std::tuple<std::vector<Peer>, std::vector<Peer>> TcpExample::createPeerLists(
    const eckit::Configuration& config) {
    std::vector<Peer> clientPeers;
    std::vector<Peer> serverPeers;

    for (auto cfg : config.getSubConfigurations("servers")) {
        auto host = cfg.getString("host");
        for (auto port : cfg.getUnsignedVector("ports")) {
            serverPeers.push_back(Peer{host, port});
        }
    }

    for (auto cfg : config.getSubConfigurations("clients")) {
        auto host = cfg.getString("host");
        for (auto port : cfg.getUnsignedVector("ports")) {
            clientPeers.push_back(Peer{host, port});
        }
    }

    nbClients_ = clientPeers.size();
    nbServers_ = serverPeers.size();

    return std::make_tuple(clientPeers, serverPeers);
}

void TcpExample::spawnServers(const std::vector<Peer>& serverPeers,
                              const eckit::Configuration& config,
                              std::shared_ptr<Transport> transport) {
    // Do nothing if current process is not in the list of servers
    if (find(begin(serverPeers), end(serverPeers), transport->localPeer()) == end(serverPeers)) {
        return;
    }

    Listener listener(config, *transport);
    listener.listen();
}

void TcpExample::spawnClients(const std::vector<Peer>& clientPeers,
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
        auto& global_field =
            global_test_field(field_id, field_size(), client.domain_, client_list_id);
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

void TcpExample::execute(const eckit::option::CmdArgs&) {

    eckit::LocalConfiguration config{config_};

    config.set("local_port", port_);

    std::shared_ptr<Transport> transport{TransportFactory::instance().build("Tcp", config)};

    eckit::Log::info() << *transport << std::endl;

    field_size() = 29;

    std::vector<Peer> clientPeers;
    std::vector<Peer> serverPeers;
    std::tie(clientPeers, serverPeers) = createPeerLists(config);

    spawnServers(serverPeers, config, transport);
    spawnClients(clientPeers, serverPeers, transport);
}

//----------------------------------------------------------------------------------------------------------------------

int main(int argc, char** argv) {
    TcpExample tool(argc, argv);
    return tool.start();
}

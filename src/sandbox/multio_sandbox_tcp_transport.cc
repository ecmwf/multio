
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

#include "sandbox/MultioServerTool.h"
#include "sandbox/Listener.h"
#include "sandbox/Message.h"
#include "sandbox/Peer.h"
#include "sandbox/Transport.h"

using namespace eckit;
using namespace multio;
using namespace multio::sandbox;

//----------------------------------------------------------------------------------------------------------------------

class TcpExample final : public multio::sandbox::MultioServerTool {
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
                    << tool << " --port=9771" << std::endl
                    << std::endl;
    }

    void init(const eckit::option::CmdArgs& args) override;

    void execute(const eckit::option::CmdArgs& args) override;

    std::vector<Peer> spawnServers(const eckit::Configuration& config,
                                   std::shared_ptr<Transport> transport);

    void spawnClients(std::shared_ptr<Transport> transport, const std::vector<Peer>& serverPeers);

    size_t nbClients_ = 1;
    int port_ = 7777;

    eckit::YAMLConfiguration config_;
};

//----------------------------------------------------------------------------------------------------------------------

std::string plan_configurations() {
    return R"json(
        {
           "transport" : "tcp",
           "servers" : [
              {
                 "host" : "skadi",
                 "ports" : [9771, 9772, 9773]
              }
           ],
           "plans" : [
              {
                 "name" : "ocean",
                 "actions" : {
                    "root" : {
                       "type" : "Print",
                       "stream" : "error",
                       "next" : {
                          "type" : "AppendToFile",
                          "path" : "messages.txt",
                          "next" : {
                             "type" : "Null"
                          }
                       }
                    }
                 }
              }
           ]
        }
    )json";
}

//----------------------------------------------------------------------------------------------------------------------

TcpExample::TcpExample(int argc, char** argv) :
    multio::sandbox::MultioServerTool(argc, argv),
    config_(plan_configurations()) {
    options_.push_back(new eckit::option::SimpleOption<size_t>("port", "TCP port"));
}

void TcpExample::init(const option::CmdArgs& args) {
    args.get("port", port_);
}

//----------------------------------------------------------------------------------------------------------------------

std::vector<Peer> TcpExample::spawnServers(const eckit::Configuration& config,
                                           std::shared_ptr<Transport> transport) {
    std::vector<Peer> serverPeers;

    auto serverConfigs = config.getSubConfigurations("servers");

    for (auto cfg : serverConfigs) {
        auto host = cfg.getString("host");
        for (auto port : cfg.getUnsignedVector("ports")) {
            serverPeers.push_back(Peer{host, port});
        }
    }

    auto it = std::find(begin(serverPeers), end(serverPeers), transport->localPeer());
    if (it != end(serverPeers)) {
        Listener listener(config, *transport);

        listener.listen();
    }

    return serverPeers;
}

void TcpExample::spawnClients(std::shared_ptr<Transport> transport,
                              const std::vector<Peer>& serverPeers) {
    // Do nothing if current rank is a server rank
    if (find(begin(serverPeers), end(serverPeers), transport->localPeer()) != end(serverPeers)) {
        return;
    }

    Peer client = transport->localPeer();

    for (auto& server : serverPeers) {
        Message msg{{Message::Tag::Open, client, server}, std::string("open")};
        transport->send(msg);

        std::string str = "Once upon a midnight dreary + " + std::to_string(server.id_);
        msg = Message{{Message::Tag::Field, client, server}, str};
        transport->send(msg);

        msg = Message{{Message::Tag::Close, client, server}, std::string("close")};
        transport->send(msg);
    }
}

//----------------------------------------------------------------------------------------------------------------------

void TcpExample::execute(const eckit::option::CmdArgs&) {

    eckit::LocalConfiguration config{config_};

    config.set("local_port", port_);

    std::shared_ptr<Transport> transport{TransportFactory::instance().build("Tcp", config)};

    eckit::Log::info() << *transport << std::endl;

    auto serverPeers = spawnServers(config_, transport);

    spawnClients(transport, serverPeers);
}

//----------------------------------------------------------------------------------------------------------------------

int main(int argc, char** argv) {
    TcpExample tool(argc, argv);
    return tool.start();
}

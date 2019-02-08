
#include "MultioTool.h"

#include <algorithm>

#include "eckit/config/YAMLConfiguration.h"
#include "eckit/option/SimpleOption.h"

#include "sandbox/Listener.h"
#include "sandbox/ThreadTransport.h"

namespace multio {
namespace sandbox {

MultioTool::MultioTool(int argc, char** argv) : eckit::Tool(argc, argv, "MULTIO_HOME") {
    options_.push_back(new eckit::option::SimpleOption<size_t>("nbclients", "Number of clients"));
    options_.push_back(new eckit::option::SimpleOption<size_t>("nbservers", "Number of servers"));
}

void MultioTool::usage(const std::string& tool) {
    eckit::Log::info() << std::endl
                       << "Usage: " << tool << " [options]" << std::endl
                       << std::endl
                       << std::endl
                       << "Examples:" << std::endl
                       << "=========" << std::endl
                       << std::endl
                       << tool << " --nbclients=10 --nbservers=4" << std::endl
                       << std::endl;
}

void MultioTool::init(const eckit::option::CmdArgs& args) {
    args.get("nbclients", nbClients_);
    args.get("nbservers", nbServers_);
}

void MultioTool::run() {
    eckit::option::CmdArgs args(&MultioTool::usage, options_, numberOfPositionalArguments(),
                                minimumPositionalArguments());

    init(args);
    execute(args);
    finish(args);
}

//--------------------------------------------------------------------------------------------------

MultioThreadTool::MultioThreadTool(int argc, char** argv) : MultioTool(argc, argv) {}

std::tuple<std::vector<Peer>, std::vector<std::thread>> MultioThreadTool::spawnServers(
    const eckit::Configuration& config, std::shared_ptr<Transport> transport, size_t nbServers) {

    auto listen = [&config, transport]() {
        Listener listener(config, *transport);

        listener.listen();
    };

    std::vector<Peer> serverPeers;
    std::vector<std::thread> servers;
    for (size_t i = 0; i != nbServers; ++i) {
        eckit::Log::info() << "starting server " << i << std::endl;

        std::thread t(listen);

        serverPeers.push_back(Peer("thread", std::hash<std::thread::id>{}(t.get_id())));

        servers.push_back(std::move(t));
    }

    return std::make_tuple(serverPeers, std::move(servers));
}

std::vector<std::thread> MultioThreadTool::spawnClients(std::shared_ptr<Transport> transport,
                                                          size_t nbClients,
                                                          const std::vector<Peer>& serverPeers) {
    auto sendMsg = [transport, serverPeers]() {
        Peer client = transport->localPeer();

        // open all servers
        for (auto& server : serverPeers) {
            Message open{Message::Tag::Open, client, server, std::string("open")};
            transport->send(open);
        }

        // send N messages
        const int nmessages = 10;
        for (int ii = 0; ii < nmessages; ++ii) {
            for (auto& server : serverPeers) {
                std::ostringstream oss;
                oss << "Once upon a midnight dreary " << " + " << client;

                Message msg{Message::Tag::Field, client, server, oss.str()};

                transport->send(msg);
            }
        }

        // close all servers
        for (auto& server : serverPeers) {
            Message close{Message::Tag::Close, client, server, std::string("close")};
            transport->send(close);
        }
    };

    std::vector<std::thread> clients;

    for (size_t ii = 0; ii != nbClients; ++ii) {
        clients.emplace_back(sendMsg);
    }

    return clients;
}


std::string local_plan() {
    return R"json(
    {
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

void MultioThreadTool::execute(const eckit::option::CmdArgs&) {

    eckit::YAMLConfiguration config{local_plan()};

    std::shared_ptr<Transport> transport{TransportFactory::instance().build("Thread", config)};

    std::vector<Peer> serverPeers;
    std::vector<std::thread> servers;
    std::tie(serverPeers, servers) = spawnServers(config, transport, nbServers_);

    std::vector<std::thread> clients = spawnClients(transport, nbClients_, serverPeers);

    std::for_each(begin(clients), end(clients), [](std::thread& t) { t.join(); });
    std::for_each(begin(servers), end(servers), [](std::thread& t) { t.join(); });
}


}  // namespace sandbox
}  // namespace multio

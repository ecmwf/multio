
#include <algorithm>
#include <iostream>
#include <thread>

#include "eckit/config/YAMLConfiguration.h"
#include "eckit/log/Log.h"
#include "eckit/option/CmdArgs.h"
#include "eckit/option/SimpleOption.h"

#include "sandbox/MultioServerTool.h"
#include "sandbox/Listener.h"
#include "sandbox/Message.h"
#include "sandbox/Peer.h"
#include "sandbox/Transport.h"

using eckit::Log;
using namespace multio::sandbox;

//----------------------------------------------------------------------------------------------------------------------

class ThreadExample final : public multio::sandbox::MultioServerTool {
public:  // methods

    ThreadExample(int argc, char** argv);

private:
    void usage(const std::string &tool) const override {
        Log::info() << std::endl
                    << "Usage: " << tool << " [options]" << std::endl
                    << std::endl
                    << std::endl
                    << "Examples:" << std::endl
                    << "=========" << std::endl
                    << std::endl
                    << tool << " --nbclients 10 --nbservers 4" << std::endl
                    << std::endl;
    }

    void init(const eckit::option::CmdArgs& args) override;

    void execute(const eckit::option::CmdArgs& args) override;

    std::tuple<std::vector<Peer>, std::vector<std::thread>> spawnServers(
        const eckit::Configuration& config, std::shared_ptr<Transport> transport);

    std::vector<std::thread> spawnClients(std::shared_ptr<Transport> transport,
                                          const std::vector<Peer>& serverPeers);

    size_t nbClients_ = 1;
};

ThreadExample::ThreadExample(int argc, char** argv) : MultioServerTool(argc, argv) {
    options_.push_back(new eckit::option::SimpleOption<size_t>("nbclients", "Number of clients"));
}

void ThreadExample::init(const eckit::option::CmdArgs& args) {
    MultioServerTool::init(args);
    args.get("nbclients", nbClients_);
}

//----------------------------------------------------------------------------------------------------------------------

std::tuple<std::vector<Peer>, std::vector<std::thread>> ThreadExample::spawnServers(
    const eckit::Configuration& config, std::shared_ptr<Transport> transport) {
    auto listen = [&config, transport]() {
        Listener listener(config, *transport);

        listener.listen();
    };

    std::vector<Peer> serverPeers;
    std::vector<std::thread> servers;
    for (size_t i = 0; i != nbServers_; ++i) {
        Log::info() << "starting server " << i << std::endl;

        std::thread t(listen);

        serverPeers.push_back(Peer("thread", std::hash<std::thread::id>{}(t.get_id())));

        servers.push_back(std::move(t));
    }

    return std::make_tuple(serverPeers, std::move(servers));
}

std::vector<std::thread> ThreadExample::spawnClients(std::shared_ptr<Transport> transport,
                                                     const std::vector<Peer>& serverPeers) {
    auto sendMsg = [transport, serverPeers]() {
        Peer client = transport->localPeer();

        // open all servers
        for (auto& server : serverPeers) {
            Message open{{Message::Tag::Open, client, server}, std::string("open")};
            transport->send(open);
        }

        // send N messages
        const int nmessages = 10;
        for (int ii = 0; ii < nmessages; ++ii) {
            for (auto& server : serverPeers) {
                std::ostringstream oss;
                oss << "Once upon a midnight dreary" << " + " << client;

                Message msg{{Message::Tag::Field, client, server}, oss.str()};

                transport->send(msg);
            }
        }

        // close all servers
        for (auto& server : serverPeers) {
            Message close{{Message::Tag::Close, client, server}, std::string("close")};
            transport->send(close);
        }
    };

    std::vector<std::thread> clients;

    for (size_t ii = 0; ii != nbClients_; ++ii) {
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

void ThreadExample::execute(const eckit::option::CmdArgs&) {
    eckit::YAMLConfiguration config{local_plan()};

    std::shared_ptr<Transport> transport{TransportFactory::instance().build("Thread", config)};

    std::vector<Peer> serverPeers;
    std::vector<std::thread> servers;
    std::tie(serverPeers, servers) = spawnServers(config, transport);

    std::vector<std::thread> clients = spawnClients(transport, serverPeers);

    std::for_each(begin(clients), end(clients), [](std::thread& t) { t.join(); });
    std::for_each(begin(servers), end(servers), [](std::thread& t) { t.join(); });
}

//----------------------------------------------------------------------------------------------------------------------

int main(int argc, char** argv) {
    ThreadExample tool(argc, argv);
    return tool.start();
}

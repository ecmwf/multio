
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

#include "sandbox/MultioServerTool.h"
#include "sandbox/Listener.h"
#include "sandbox/Message.h"
#include "sandbox/Peer.h"
#include "sandbox/Transport.h"

using namespace eckit;
using namespace multio;
using namespace multio::sandbox;

//----------------------------------------------------------------------------------------------------------------------

class ThreadExample final : public multio::sandbox::MultioServerTool {
public:  // methods

    ThreadExample(int argc, char** argv);

    virtual void usage(const std::string &tool) const {
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

protected:  // methods
    virtual void init(const eckit::option::CmdArgs& args);

private:  // methods

    virtual void execute(const eckit::option::CmdArgs& args);

    std::vector<std::thread> spawnServers(const Configuration& config,
                                          std::shared_ptr<Transport> transport, size_t nbServers,
                                          std::vector<Peer>& peerServers);
    std::vector<std::thread> spawnClients(std::shared_ptr<Transport> transport, size_t nbClients,
                                          const std::vector<Peer>& servers);

private:  // members
    size_t nbClients_ = 1;
};

ThreadExample::ThreadExample(int argc, char** argv) : MultioServerTool(argc, argv) {
    options_.push_back(new eckit::option::SimpleOption<size_t>("nbclients", "Number of clients"));
}

void ThreadExample::init(const option::CmdArgs& args) {
    MultioServerTool::init(args);
    args.get("nbclients", nbClients_);
}

//----------------------------------------------------------------------------------------------------------------------

std::vector<std::thread> ThreadExample::spawnServers(const eckit::Configuration& config,
                                                   std::shared_ptr<Transport> transport,
                                                   size_t nbServers,
                                                   std::vector<Peer>& peerServers) {
    auto listen = [&config, transport]() {
        Listener listener(config, *transport);

        listener.listen();
    };

    std::vector<std::thread> servers;

    for (size_t i = 0; i != nbServers; ++i) {
        eckit::Log::info() << "starting server " << i << std::endl;

        std::thread t(listen);

        peerServers.push_back(Peer("thread", std::hash<std::thread::id>{}(t.get_id())));

        servers.emplace_back(std::move(t));
    }

    return servers;
}

std::vector<std::thread> ThreadExample::spawnClients(std::shared_ptr<Transport> transport,
                                                   size_t nbClients,
                                                   const std::vector<Peer>& servers) {
    const int nmessages = 10;

    std::vector<std::thread> clients;

    for (size_t i = 0; i != nbClients; ++i) {
        auto sendMsg = [transport, servers]() {
            Peer client = transport->localPeer();

            // open all servers
            for (auto& server : servers) {
                Message open{Message::Tag::Open, client, server, std::string("open")};
                transport->send(open);
            }

            // send N messages
            for (int i = 0; i < nmessages; ++i) {
                for (auto& server : servers) {
                    std::ostringstream oss;
                    oss << "Once upon a midnight dreary "
                        << " + " << client << "\n";

                    Message msg{Message::Tag::Field, client, server, oss.str()};

                    transport->send(msg);
                }
            }

            // close all servers
            for (auto& server : servers) {
                Message close{Message::Tag::Close, client, server, std::string("close")};
                transport->send(close);
            }
        };

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

//----------------------------------------------------------------------------------------------------------------------

void ThreadExample::execute(const eckit::option::CmdArgs&) {

    eckit::YAMLConfiguration config{local_plan()};

    std::shared_ptr<Transport> transport(TransportFactory::instance().build("Thread", config));

    // spawn servers

    std::vector<Peer> peerServers;

    std::vector<std::thread> servers = spawnServers(config, transport, nbServers_, peerServers);

    // spawn clients

    std::vector<std::thread> clients = spawnClients(transport, nbClients_, peerServers);

    std::for_each(begin(clients), end(clients), [](std::thread& t) { t.join(); });
    std::for_each(begin(servers), end(servers), [](std::thread& t) { t.join(); });
}

//----------------------------------------------------------------------------------------------------------------------

int main(int argc, char** argv) {
    ThreadExample tool(argc, argv);
    return tool.start();
}


#include <algorithm>

#include "eckit/mpi/Comm.h"

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

class MpiExample final : public multio::sandbox::MultioServerTool {
public:  // methods

    MpiExample(int argc, char** argv);

private:
    void usage(const std::string &tool) const override {
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
};

//----------------------------------------------------------------------------------------------------------------------

MpiExample::MpiExample(int argc, char** argv) : multio::sandbox::MultioServerTool(argc, argv) {}

void MpiExample::init(const eckit::option::CmdArgs& args) {
    MultioServerTool::init(args);
    nbClients_ = eckit::mpi::comm("world").size() - nbServers_;
}

//----------------------------------------------------------------------------------------------------------------------

std::vector<Peer> MpiExample::spawnServers(const eckit::Configuration& config,
                                           std::shared_ptr<Transport> transport) {
    std::vector<Peer> serverPeers;

    long size = eckit::mpi::comm("world").size();
    for (long ii = size - nbServers_; ii != size; ++ii) {
        serverPeers.push_back(Peer{"world", static_cast<size_t>(ii)});
    }

    auto it = std::find(begin(serverPeers), end(serverPeers), transport->localPeer());
    if (it != end(serverPeers)) {
        Listener listener(config, *transport);

        listener.listen();
    }

    return serverPeers;
}

void MpiExample::spawnClients(std::shared_ptr<Transport> transport,
                              const std::vector<Peer>& serverPeers) {
    auto it = std::find(begin(serverPeers), end(serverPeers), transport->localPeer());

    if (it != end(serverPeers)) {
        return;
    }

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

            oss << "Once upon a midnight dreary + " << client;

            Message msg{Message::Tag::Field, client, server, oss.str()};

            transport->send(msg);
        }
    }

    // close all servers
    for (auto& server : serverPeers) {
        Message close{Message::Tag::Close, client, server, std::string("close")};
        transport->send(close);
    }
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

void MpiExample::execute(const eckit::option::CmdArgs&) {
    eckit::YAMLConfiguration config{local_plan()};

    std::cout << "Clients: " << nbClients_ << std::endl << "Servers: " << nbServers_ << std::endl;

    std::shared_ptr<Transport> transport{TransportFactory::instance().build("Mpi", config)};

    std::cout << *transport << std::endl;

    auto serverPeers = spawnServers(config, transport);

    spawnClients(transport, serverPeers);
}

//----------------------------------------------------------------------------------------------------------------------

int main(int argc, char** argv) {
    MpiExample tool(argc, argv);
    return tool.start();
}

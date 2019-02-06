
#include <algorithm>
#include <chrono>
#include <functional>
#include <iostream>
#include <mutex>
#include <string>
#include <thread>

#include "eckit/log/Log.h"
#include "eckit/option/CmdArgs.h"
#include "eckit/option/SimpleOption.h"
#include "eckit/runtime/Tool.h"

#include "sandbox/Message.h"
#include "sandbox/Peer.h"
#include "sandbox/ThreadTransport.h"

using namespace eckit;
using namespace multio;
using namespace multio::sandbox;

namespace {
std::mutex mut;
template <typename Printable>
void print(Printable& val) {
    std::lock_guard<std::mutex> lock{mut};
    std::cout << val << std::endl;
}
}  // namespace

//----------------------------------------------------------------------------------------------------------------------

class SandboxTool final : public eckit::Tool {

public:  // methods

    SandboxTool(int argc, char** argv);

    static void usage(const std::string& tool) {
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

protected:  // members
    std::vector<eckit::option::Option*> options_;

protected:  // methods
    virtual void init(const eckit::option::CmdArgs& args);
    virtual void finish(const eckit::option::CmdArgs& args) {}

private:  // methods

    virtual void execute(const eckit::option::CmdArgs& args);

    virtual int numberOfPositionalArguments() const { return -1; }
    virtual int minimumPositionalArguments() const { return -1; }

    void run() override;

    std::vector<std::thread> spawnServers(std::shared_ptr<Transport> transport, int nbServers, int nbClients /* to be removed */, std::vector<Peer>& peerServers );
    std::vector<std::thread> spawnClients(std::shared_ptr<Transport> transport, int nbClients, const std::vector<Peer>& servers);

private:  // members

    size_t nbClients_ = 1;
    size_t nbServers_ = 1;
};

//static SandboxTool* instance_ = nullptr;

SandboxTool::SandboxTool(int argc, char** argv) : eckit::Tool(argc, argv, "MULTIO_HOME")
{
//    ASSERT(instance_ == nullptr);
//    instance_ = this;

    options_.push_back(new eckit::option::SimpleOption<size_t>("nbclients", "Number of clients"));
    options_.push_back(new eckit::option::SimpleOption<size_t>("nbservers", "Number of servers"));
}

//static void usage(const std::string& tool) {
//    ASSERT(instance_);
//    instance_->usage(tool);
//}

void SandboxTool::init(const option::CmdArgs& args) {
    args.get("nbclients", nbClients_);
    args.get("nbservers", nbServers_);
}

//----------------------------------------------------------------------------------------------------------------------

void SandboxTool::run() {
    eckit::option::CmdArgs args(&SandboxTool::usage,
                                options_,
                                numberOfPositionalArguments(),
                                minimumPositionalArguments());

    init(args);
    execute(args);
    finish(args);
}

std::vector<std::thread> SandboxTool::spawnServers(std::shared_ptr<Transport> transport, int nbServers, int nbClients, std::vector<Peer>& peerServers)
{
    auto listen = [transport, nbClients]() {
        Peer server = transport->localPeer();

        auto counter = 0;
        do {
            Message msg = transport->receive();

            eckit::Log::info() << msg << std::endl;

            if (msg.tag() == Message::Tag::close)
                ++counter;

        } while (counter < nbClients);
    };

    std::vector<std::thread> servers;

    for (auto i = 0; i != nbServers; ++i) {

        std::thread t(listen);

        peerServers.push_back(Peer("thread", std::hash<std::thread::id>{}(t.get_id())));

        servers.emplace_back(std::move(t));
    }

    return servers;
}

std::vector<std::thread> SandboxTool::spawnClients(std::shared_ptr<Transport> transport, int nbClients, const std::vector<Peer>& servers)
{
    const int nmessages = 10;

    std::vector<std::thread> clients;

    for (auto i = 0; i != nbClients; ++i) {

        auto sendMsg = [transport, servers]() {

            Peer client = transport->localPeer();

            // open all servers
            for (auto& server: servers) {
                Message open {Message::Tag::close, client, server, std::string("open")};
                transport->send(open);
            }

            // send N messages
            for(int i = 0; i < nmessages; ++i) {
                for (auto& server: servers) {

                    std::ostringstream oss;
                    oss << "Once upon a midnight dreary " << " + " << std::this_thread::get_id();

                    Message msg {Message::Tag::field_data, client, server, oss.str()};

                    transport->send(msg);
                }
            }

            // close all servers
            for (auto& server: servers) {
                Message close {Message::Tag::close, client, server, std::string("close")};
                transport->send(close);
            }
        };

        clients.emplace_back(sendMsg);
    }

    return clients;
}


void SandboxTool::execute(const eckit::option::CmdArgs&) {

    eckit::LocalConfiguration config;
    config.set("name", "test");
    config.set("nbClients", nbClients_);
    config.set("nbServers", nbServers_);

    std::shared_ptr<Transport> transport {std::make_shared<ThreadTransport>(config)};

    // spawn servers

    std::vector<Peer> peerServers;

    std::vector<std::thread> servers = spawnServers(transport, nbServers_, nbClients_, peerServers);


    // spawn clients

    std::vector<std::thread> clients = spawnClients(transport, nbClients_, peerServers);


    std::for_each(begin(clients), end(clients), [](std::thread& t) { t.join(); });
    std::for_each(begin(servers), end(servers), [](std::thread& t) { t.join(); });
}

//----------------------------------------------------------------------------------------------------------------------

int main(int argc, char** argv) {
    SandboxTool tool(argc, argv);
    return tool.start();
}


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
    virtual void finish(const eckit::option::CmdArgs& args);

private:  // methods

    virtual void execute(const eckit::option::CmdArgs& args);

    virtual int numberOfPositionalArguments() const { return -1; }
    virtual int minimumPositionalArguments() const { return -1; }

    void run() override;

    std::map<Peer, std::thread> spawnServers(std::shared_ptr<Transport> transport, int nbServers, int nbClients /* to be removed */ );
    std::map<Peer, std::thread> spawnClients(std::shared_ptr<Transport> transport, int nbClients, const std::map<Peer, std::thread>& servers);



private:  // members
    int nbClients_ = 1;
    int nbServers_ = 1;
};

static SandboxTool* instance_ = nullptr;

SandboxTool::SandboxTool(int argc, char** argv) : eckit::Tool(argc, argv, "MULTIO_HOME")
{
    ASSERT(instance_ == nullptr);
    instance_ = this;

    options_.push_back(new eckit::option::SimpleOption<int>("nbclients", "Number of clients"));
    options_.push_back(new eckit::option::SimpleOption<int>("nbservers", "Number of servers"));
}

static void usage(const std::string& tool) {
    ASSERT(instance_);
    instance_->usage(tool);
}

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

std::map<Peer, std::thread> SandboxTool::spawnServers(std::shared_ptr<Transport> transport, int nbServers, int nbClients)
{
    auto listen = [transport, nbClients]() {
        auto counter = 0;
        do {
            Message msg = transport->receive();

            eckit::Log::info() << msg << std::endl;

            if (msg.tag() == Message::Tag::close)
                ++counter;

        } while (counter < nbClients);
    };

    std::map<Peer, std::thread> servers;

    for (auto i = 0; i != nbServers; ++i) {

        Peer server {"thread", i};

        servers.emplace(server, listen);
    }
}

std::map<Peer, std::thread> SandboxTool::spawnClients(std::shared_ptr<Transport> transport, int nbClients, const std::map<Peer, std::thread>& servers)
{
    int nbServers = servers.size();


    std::map<Peer, std::thread> clients;

    for (auto i = 0; i != nbClients; ++i) {

        Peer client {"thread", i};

        auto sendMsg = [transport, servers](Peer& from) {

            for (auto& server: servers) {
                Peer to = server.first;
                Message msg {Message::Tag::open, from, to, std::string("opening")};
                transport->send(msg);
            }


            std::ostringstream oss;
            oss << "Once upon a midnight dreary " << " + " << std::this_thread::get_id();

            int peer = std::hash<std::string>{}(oss.str()) % nbServers;

            Message msg {Message::Tag::mapping_data, from, to, oss.str()};

            transport->send(msg);

            msg = Message{0, peer, MsgTag::close};
            print(msg);
            transport->send(msg);
        };



        clients.emplace(client, sendMsg(i));
    }
}


void SandboxTool::execute(const eckit::option::CmdArgs& args) {

    eckit::LocalConfiguration config;
    config.set("name", "test");
    config.set("nbClients", nbClients_);
    config.set("nbServers", nbServers_);

    std::shared_ptr<Transport> transport{
        std::make_shared<ThreadTransport>(config)};


    // spawn servers

    std::map<Peer, std::thread> servers = spawnServers(transport, nbServers_, nbClients_);


    // spawn clients

    std::map<Peer, std::thread> clients = spawnClients(transport, nbClients_, servers);


    std::for_each(begin(clients), end(clients), [](std::pair<Peer, std::thread>& t) { t.second.join(); });
    std::for_each(begin(servers), end(servers), [](std::pair<Peer, std::thread>& t) { t.second.join(); });

 # if 0


    std::vector<Peer> peerClients;
    std::vector<Peer> peerServers;

    // Spawn clients with sendString function
    std::vector<std::thread> clients;
    for (auto ii = 0; ii != nbClients_; ++ii) {
        clients.emplace_back(sendString);
    }

    auto listen = [transport, nbClients_]() {
        auto counter = 0;
        do {
            Message msg{0, nbClients_, Message::Tag::mapping_data};
            transport->receive(msg);
            print(msg);
            if (msg.tag() == MsgTag::close)
                ++counter;
        } while (counter < nbClients_);
    };

    // Spawn servers with listen function
    std::vector<std::thread> servers;
    for (auto ii = 0; ii != nbServers_; ++ii) {
        servers.emplace_back(listen);
    }

    std::for_each(begin(clients), end(clients), [](std::thread& t) { t.join(); });
    std::for_each(begin(servers), end(servers), [](std::thread& t) { t.join(); });

#endif

}

//----------------------------------------------------------------------------------------------------------------------

int main(int argc, char** argv) {
    SandboxTool tool(argc, argv);
    return tool.start();
}


#include <algorithm>
#include <chrono>
#include <iostream>
#include <functional>
#include <mutex>
#include <string>
#include <thread>

#include "eckit/log/Log.h"
#include "eckit/runtime/Tool.h"
#include "eckit/option/SimpleOption.h"
#include "eckit/option/CmdArgs.h"

#include "sandbox/Message.h"
#include "sandbox/Peer.h"
#include "sandbox/ThreadTransport.h"

using namespace eckit;
using namespace multio;

namespace {
std::mutex mut;
template <typename Printable>
void print(Printable& val) {
    std::lock_guard<std::mutex> lock{mut};
    std::cout << val << std::endl;
}
}  // namespace

//----------------------------------------------------------------------------------------------------------------------

class SandboxTool : public eckit::Tool {

protected: // methods

    SandboxTool(int argc, char **argv) :
        eckit::Tool(argc, argv, "MULTIO_HOME")
    {
        options_.push_back(new eckit::option::SimpleOption<int>("nbclients", "Number of clients"));
        options_.push_back(new eckit::option::SimpleOption<int>("nbservers", "Number of servers"));
    }

public: // methods

    virtual void usage(const std::string &tool) const {
        Log::info() << std::endl
                    << "Usage: " << tool << " [options]" << std::endl
                    << std::endl
                    << std::endl
                    << "Examples:" << std::endl
                    << "=========" << std::endl << std::endl
                    << tool << " --all" << std::endl
                    << tool << " --version" << std::endl
                    << tool << " --home" << std::endl
                    << tool << " --schema" << std::endl
                    << std::endl;

    }

protected: // members

    std::vector<eckit::option::Option *> options_;

protected: // methods

    virtual void init(const eckit::option::CmdArgs& args);
    virtual void finish(const eckit::option::CmdArgs& args);

private: // methods

    virtual int numberOfPositionalArguments() const { return -1; }
    virtual int minimumPositionalArguments() const { return -1; }

    virtual void run() override final;

    virtual void execute() override;

private: // members

    int nbClients_;
    int nbServers_;

};


//----------------------------------------------------------------------------------------------------------------------

void SandboxTool::init(const option::CmdArgs& args)
{
    eckit::option::CmdArgs args(usage,
        options_,
        numberOfPositionalArguments(),
        minimumPositionalArguments());

    args.get("nbclients", nbClients_);
    args.get("nbservers", nbServers_);
}

void SandboxTool::execute()  {

    eckit::LocalConfiguration config;
    config.set("name", "test");
    auto no_clients = 7;
    auto no_servers = 1;
    config.set("no_clients", no_clients);
    config.set("no_servers", no_servers);

    std::shared_ptr<sandbox::Transport> transport{
        std::make_shared<sandbox::ThreadTransport>(config)};

    auto sendString = [transport, no_clients, no_servers]() {
        std::string str = "Once upon a midnight dreary ";
        std::ostringstream os;
        os << str << std::this_thread::get_id();

        int peer = std::hash<std::string>{}(os.str()) % no_servers + no_clients;
        sandbox::Message msg{0, peer, sandbox::MsgTag::mapping_data};
        msg.write(str.data(), str.size());
        print(msg);
        transport->send(msg);

        msg = sandbox::Message{0, peer, sandbox::MsgTag::close};
        print(msg);
        transport->send(msg);
    };


    std::vector<Peer> peerClients;
    std::vector<Peer> peerServers;

    // Spawn clients with sendString function
    std::vector<std::thread> clients;
    for (auto ii = 0; ii != no_clients; ++ii) {
        clients.emplace_back(sendString);
    }

    auto listen = [transport, no_clients]() {
        auto counter = 0;
        do {
            sandbox::Message msg{0, no_clients, sandbox::MsgTag::mapping_data};
            transport->receive(msg);
            print(msg);
            if (msg.tag() == sandbox::MsgTag::close)
                ++counter;
        } while (counter < no_clients);
    };

    // Spawn servers with listen function
    std::vector<std::thread> servers;
    for (auto ii = 0; ii != no_servers; ++ii) {
        servers.emplace_back(listen);
    }

    std::for_each(begin(clients), end(clients), [](std::thread& t) { t.join(); });
    std::for_each(begin(servers), end(servers), [](std::thread& t) { t.join(); });
}

//----------------------------------------------------------------------------------------------------------------------

int main(int argc, char** argv) {
    SandboxTool tool(argc, argv);
    return tool.start();

}

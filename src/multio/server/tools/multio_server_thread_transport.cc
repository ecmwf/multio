
#include <algorithm>
#include <iostream>
#include <mutex>
#include <thread>

#include "eckit/config/YAMLConfiguration.h"
#include "eckit/log/Log.h"
#include "eckit/option/CmdArgs.h"
#include "eckit/option/SimpleOption.h"

#include "multio/server/MultioServerTool.h"
#include "multio/server/Listener.h"
#include "multio/server/LocalIndices.h"
#include "multio/server/Message.h"
#include "multio/server/Peer.h"
#include "multio/server/PlanConfigurations.h"
#include "multio/server/print_buffer.h"
#include "multio/server/TestData.h"
#include "multio/server/Transport.h"

using eckit::Log;
using namespace multio::server;

//----------------------------------------------------------------------------------------------------------------------

class ThreadExample final : public multio::server::MultioServerTool {
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
                    << tool << " --nbclients=10 --nbservers=4" << std::endl
                    << std::endl;
    }

    void init(const eckit::option::CmdArgs& args) override;

    void execute(const eckit::option::CmdArgs& args) override;

    std::tuple<std::vector<Peer>, std::vector<std::thread>> spawnServers(
        const eckit::Configuration& config, std::shared_ptr<Transport> transport);

    std::vector<std::thread> spawnClients(std::shared_ptr<Transport> transport,
                                          const std::vector<Peer>& serverPeers);

    size_t nbClients_ = 1;

    std::mutex mut_;
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

        serverPeers.push_back(Peer{"thread", std::hash<std::thread::id>{}(t.get_id())});

        servers.push_back(std::move(t));
    }

    return std::make_tuple(serverPeers, std::move(servers));
}

std::vector<std::thread> ThreadExample::spawnClients(std::shared_ptr<Transport> transport,
                                                     const std::vector<Peer>& serverPeers) {
    auto sendMsg = [this, transport, &serverPeers](const size_t client_list_id) {
        Peer client = transport->localPeer();

        // open all servers
        for (auto& server : serverPeers) {
            Message open{{Message::Tag::Open, client, server}, std::string("open")};
            transport->send(open);
        }

        auto idxm = generate_index_map(client_list_id, nbClients_);
        eckit::Buffer buffer{reinterpret_cast<const char*>(idxm.data()),
                             idxm.size() * sizeof(size_t)};
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

            {
                std::lock_guard<std::mutex> lock{mut_};
                auto& global_field =
                    global_test_field(field_id, field_size(), "thread", client_list_id);
                index_map.to_local(global_field, field);

                if (root() == client_list_id) {
                    eckit::Log::info()
                        << "   ---   Field: " << field_id << ", values: " << std::flush;
                    print_buffer(global_field, eckit::Log::info(), " ");
                    eckit::Log::info() << std::endl;
                }
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
    };

    std::vector<std::thread> clients;

    for (size_t ii = 0; ii != nbClients_; ++ii) {
        clients.emplace_back(sendMsg, ii);
    }

    return clients;
}

void ThreadExample::execute(const eckit::option::CmdArgs&) {
    eckit::YAMLConfiguration config{thread_plan_configurations()};

    std::shared_ptr<Transport> transport{TransportFactory::instance().build("thread", config)};

    eckit::Log::info() << *transport << std::endl;

    field_size() = 29;
    new_random_data_each_run() = true;

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

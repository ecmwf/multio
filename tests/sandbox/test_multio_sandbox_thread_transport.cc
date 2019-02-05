
#include <algorithm>
#include <chrono>
#include <iostream>
#include <functional>
#include <mutex>
#include <string>
#include <thread>

#include "multio/sandbox/Message.h"
#include "multio/sandbox/ThreadTransport.h"

using namespace multio;

namespace {
std::mutex mut;
template <typename Printable>
void print(Printable& val) {
    std::lock_guard<std::mutex> lock{mut};
    std::cout << "thread: " << std::this_thread::get_id() << ", output: " << val << std::endl;
}
}  // namespace

int main(int argc, char** argv) {
    eckit::LocalConfiguration config;
    config.set("name", "thread");
    auto no_clients = 7;
    auto no_servers = 3;
    config.set("no_clients", no_clients);
    config.set("no_servers", no_servers);

    std::shared_ptr<sandbox::Transport> transport{
        std::make_shared<sandbox::ThreadTransport>(config)};

    std::ostringstream oss;
    oss << *transport;

    ASSERT(oss.str() == "Transport[thread]");

    auto sendString = [transport, no_clients, no_servers]() {
        std::string str = "Once upon a midnight dreary ";
        std::ostringstream os;
        os << str << std::this_thread::get_id();
        auto server_id = std::hash<std::string>{}(os.str()) % no_servers + no_clients;
        sandbox::Message msg{0, server_id, sandbox::MsgTag::mapping_data};
        msg.write(str.data(), str.size());
        print(msg);
        transport->send(msg);
    };

    std::vector<std::thread> clients;
    for (auto ii = 0; ii != no_clients; ++ii) {
        clients.emplace_back(std::thread{sendString});
    }

    // auto listen = [transport]() {
    //     sandbox::Message msg{};
    //     transport->receive(msg);
    //     std::cout << msg << std::endl;
    // };

    // std::vector<std::thread> servers;
    // for (auto ii = 0; ii != no_servers; ++ii) {
    //     servers.emplace_back(std::thread{listen});
    // }

    std::for_each(begin(clients), end(clients), [](std::thread& t) { t.join(); });
    // std::for_each(begin(servers), end(servers), [](std::thread& t) { t.join(); });
}

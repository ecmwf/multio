
#include "ThreadTransport.h"

namespace multio {
namespace sandbox {

ThreadTransport::ThreadTransport(const eckit::LocalConfiguration& config) :
    Transport{config},
    no_servers_(get_config_value<int>(config_, "no_servers")) {}

ThreadTransport::~ThreadTransport() = default;

void ThreadTransport::receive(Message& msg) {
    auto server_id = std::this_thread::get_id();
    msg = internalBuffers_.at(server_id).pop();
}

void ThreadTransport::send(const Message& msg) {
    auto server_id = msg.peer();
    std::cout << "    server_id: " << server_id << std::endl;
    std::cout << "    map size: " << internalBuffers_.size() << std::endl;
    std::cout << "    no_servers_: " << no_servers_ << std::endl;
    addNewQueueIfNeeded(server_id);

    // Make a copy that is stored in the internal buffer
    auto msg_out = msg;
    msg_out.peer(std::this_thread::get_id());
    internalBuffers_.at(server_id).push(std::move(msg_out));
}

void ThreadTransport::addNewQueueIfNeeded(std::thread::id server_id) {
    if (internalBuffers_.find(server_id) == end(internalBuffers_)) {
        ASSERT(internalBuffers_.size() < no_servers_);
        internalBuffers_.emplace(server_id, 1024);
    }
}

}  // namespace sandbox
}  // namespace multio


#include "ThreadTransport.h"

namespace multio {
namespace sandbox {

ThreadTransport::ThreadTransport(const eckit::Configuration& config) :
    Transport{config},
    no_servers_(get_config_value<int>(config, "no_servers")),
    no_clients_(get_config_value<int>(config, "no_clients")) {}

ThreadTransport::~ThreadTransport() = default;

void ThreadTransport::receive(Message& msg) {
    Peer local_peer = msg.peer();
    while (buffers_.find(local_peer) == end(buffers_)) {}
    msg = buffers_.at(local_peer).pop();
 }

void ThreadTransport::send(const Message& msg) {
    auto remote_peer = msg.peer();
    addNewQueueIfNeeded(remote_peer);

    // Make a copy that is stored in the internal buffer
    auto msg_out = msg;
    msg_out.peer();
    buffers_.at(remote_peer).push(std::move(msg_out));
}

void ThreadTransport::print(std::ostream& os) const {
    os << "ThreadTransport(name = " << name_ << ")";
}

void ThreadTransport::addNewQueueIfNeeded(Peer consumer) {
    std::lock_guard<std::mutex> lock(mutex_);
    if (buffers_.find(consumer) == end(buffers_)) {
        std::cout << "Adding queue..." << std::endl;
        ASSERT(buffers_.size() < static_cast<size_t>(no_servers_));
        buffers_.emplace(consumer, 1024);
    }
}

}  // namespace sandbox
}  // namespace multio

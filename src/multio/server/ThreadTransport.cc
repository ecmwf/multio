/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "ThreadTransport.h"

#include "eckit/log/Log.h"
#include "eckit/config/Resource.h"

namespace multio {
namespace server {

ThreadTransport::ThreadTransport(const eckit::Configuration& cfg) :
    Transport(cfg),
    messageQueueSize_(
        eckit::Resource<size_t>("multioMessageQueueSize;$MULTIO_MESSAGE_QUEUE_SIZE", 1024)) {}

ThreadTransport::~ThreadTransport() = default;

Message ThreadTransport::receive() {

    Peer receiver = localPeer();

    auto& queue = receiveQueue(receiver);

    Message msg;

    ASSERT(queue.pop(msg) >= 0);
    ASSERT(msg.destination() == receiver);

    return msg;
}

void ThreadTransport::send(const Message& msg) {
    receiveQueue(msg.destination()).push(msg);
}

Peer ThreadTransport::localPeer() const {
    return Peer{"thread", std::hash<std::thread::id>{}(std::this_thread::get_id())};
}

void ThreadTransport::print(std::ostream& os) const {
    os << "ThreadTransport(number of queues = " << queues_.size() << ")";
}

eckit::Queue<Message>& ThreadTransport::receiveQueue(Peer dest) {

    std::unique_lock<std::mutex> locker(mutex_);

    auto qitr = queues_.find(dest);
    if (qitr != end(queues_)) {
        return *qitr->second;
    }

    queues_.emplace(dest, new eckit::Queue<Message>(messageQueueSize_));

    eckit::Log::info() << "ADD QUEUE for " << dest << " --- " << queues_.at(dest).get()
                       << std::endl;

    return *queues_.at(dest);
}

static TransportBuilder<ThreadTransport> ThreadTransportBuilder("thread");

}  // namespace server
}  // namespace multio

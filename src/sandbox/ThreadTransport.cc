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
namespace sandbox {

ThreadTransport::ThreadTransport(const eckit::Configuration& config) :
    Transport{config},
    messageQueueSize_(eckit::Resource<size_t>("multioMessageQueueSize;$MULTIO_MESSAGE_QUEUE_SIZE", 1024))
{
}

ThreadTransport::~ThreadTransport() = default;

Message ThreadTransport::receive() {

    Peer receiver = localPeer();

    eckit::Log::info() << "RECEIVER " << receiver << std::endl;

    auto& queue = receiveQueue(receiver);

    Message msg = queue.pop();

    eckit::Log::info() << "RECV " << msg << std::endl;

    ASSERT(msg.to() == receiver);

    return msg;
}

void ThreadTransport::send(const Message& msg) {

    Peer to = msg.to();

    auto& queue = receiveQueue(to);

    queue.push(msg);
}

Peer ThreadTransport::localPeer() const
{
    Peer peer("thread", std::hash<std::thread::id>{}(std::this_thread::get_id()));
    return peer;
}


void ThreadTransport::print(std::ostream& os) const {
    os << "ThreadTransport(name = " << name_ << ")";
}

eckit::Queue<Message>& ThreadTransport::receiveQueue(Peer to) {

    std::unique_lock<std::mutex> locker(mutex_);

    eckit::Log::info() << "FIND QUEUE for " << to << std::endl;

    auto qitr = queues_.find(to);
    if (qitr != end(queues_)) {
        eckit::Log::info() << "FOUND QUEUE for " << to << " --- " << qitr->second << std::endl;
        return * qitr->second;
    }

    auto queue = new eckit::Queue<Message>(messageQueueSize_);

    eckit::Log::info() << "ADD QUEUE for " << to << " --- " << queue << std::endl;

    queues_.emplace(to, queue);

    return * queue;
}

}  // namespace sandbox
}  // namespace multio

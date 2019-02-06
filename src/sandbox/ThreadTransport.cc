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

ThreadTransport::ThreadTransport(const eckit::Configuration& config) : Transport{config} {}

ThreadTransport::~ThreadTransport() = default;

Message ThreadTransport::receive() {

    Peer receiver = localPeer();

    auto& queue = receiveQueue(receiver);

    Message msg = queue.pop();

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

    std::lock_guard<std::mutex> lock(mutex_);

    auto qitr = queues_.find(to);
    if (qitr != end(queues_)) {
        return qitr->second;
    }

   eckit::Log::info() << "Adding queue..." << std::endl;

   static size_t multioMessageQueueSize = eckit::Resource<size_t>("multioMessageQueueSize;$MULTIO_MESSAGE_QUEUE_SIZE", 1024);

   queues_.emplace(to, multioMessageQueueSize);
}

}  // namespace sandbox
}  // namespace multio

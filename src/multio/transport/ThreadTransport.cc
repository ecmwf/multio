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

#include "eckit/config/LibEcKit.h"
#include "eckit/config/Resource.h"
#include "eckit/exception/Exceptions.h"

#include "multio/LibMultio.h"

namespace multio::transport {

ThreadPeer::ThreadPeer(std::thread t) :
    Peer{"thread", std::hash<std::thread::id>{}(t.get_id())}, thread_{std::move(t)} {}

void ThreadPeer::join() {
    thread_.join();
}

ThreadTransport::ThreadTransport(const ComponentConfiguration& compConf) :
    Transport(compConf),
    messageQueueSize_(eckit::Resource<size_t>("multioMessageQueueSize;$MULTIO_MESSAGE_QUEUE_SIZE", 1024)) {}

void ThreadTransport::openConnections() {
    throw eckit::NotImplemented{Here()};
}

void ThreadTransport::closeConnections() {
    throw eckit::NotImplemented{Here()};
}


Message ThreadTransport::receive() {

    const Peer& receiver = localPeer();

    auto& queue = receiveQueue(receiver);

    Message msg;

    ASSERT(queue.pop(msg) >= 0);
    ASSERT(msg.destination() == receiver);

    return msg;
}

void ThreadTransport::abort(std::exception_ptr) {
    eckit::LibEcKit::instance().abort();
}

void ThreadTransport::send(const Message& msg) {
    receiveQueue(msg.destination()).push(msg);
}

void ThreadTransport::bufferedSend(const Message&) {
    throw eckit::NotImplemented{Here()};
}

const Peer& ThreadTransport::localPeer() const {
    thread_local static Peer peer{"thread", std::hash<std::thread::id>{}(std::this_thread::get_id())};
    return peer;
}

PeerList ThreadTransport::createServerPeers() const {
    throw eckit::NotImplemented{Here()};
}

void ThreadTransport::createPeers() const {
    // Hack to work around the very different logic of creating ThreadPeers.
    // See multio-hammer.cc: MultioHammer::executeThread
    clientPeers_ = PeerList(compConf_.parsedConfig().getUnsigned("clientCount"));
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

    auto q = queues_.emplace(dest, std::make_unique<eckit::Queue<Message>>(messageQueueSize_)).first;

    eckit::Log::debug<LibMultio>() << "ADD QUEUE for " << dest << " --- " << q->second.get() << std::endl;

    return *q->second.get();
}

static TransportBuilder<ThreadTransport> ThreadTransportBuilder("thread");

}  // namespace multio::transport

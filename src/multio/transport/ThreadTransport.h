/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Domokos Sarmany
/// @author Simon Smart
/// @author Tiago Quintino

/// @date Jan 2019

#pragma once

#include <map>
#include <mutex>
#include <thread>

#include "eckit/container/Queue.h"

#include "multio/transport/Transport.h"
#include "multio/util/ScopedThread.h"

namespace multio::transport {

class ThreadPeer : public Peer {
public:
    ThreadPeer(std::thread t);

    void join();

private:
    util::ScopedThread thread_;
};

class ThreadTransport final : public Transport {
public:
    ThreadTransport(const ComponentConfiguration& compConf);

    Message receive() override;

    void abort(std::exception_ptr) override;

private:
    void openConnections() override;
    void closeConnections() override;

    void send(const Message& message) override;

    void bufferedSend(const Message& msg) override;

    void print(std::ostream& os) const override;

    const Peer& localPeer() const override;

    PeerList createServerPeers() const override;

    void createPeers() const override;

    eckit::Queue<Message>& receiveQueue(Peer to);

    std::map<Peer, std::unique_ptr<eckit::Queue<Message>>> queues_;

    std::mutex mutex_;

    size_t messageQueueSize_;
};

}  // namespace multio::transport

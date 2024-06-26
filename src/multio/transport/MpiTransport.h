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

#include <queue>
#include <tuple>

#include "eckit/container/Queue.h"
#include "eckit/io/Buffer.h"
#include "eckit/log/Statistics.h"
#include "eckit/mpi/Comm.h"
#include "eckit/mpi/Group.h"
#include "eckit/serialisation/ResizableMemoryStream.h"


#include "multio/transport/StreamPool.h"
#include "multio/transport/Transport.h"

namespace multio::transport {

struct ReceivedBuffer {
    MpiBuffer* buffer;
    size_t size;
};

using MpiPeerSetup = std::tuple<MpiPeer, eckit::mpi::Group, eckit::mpi::Group, eckit::mpi::Group>;

class MpiTransport final : public Transport {
public:
    MpiTransport(const ComponentConfiguration& compConf);
    ~MpiTransport();

private:
    MpiTransport(const ComponentConfiguration& compConf, MpiPeerSetup&& peerSetup);

    void openConnections() override;
    void closeConnections() override;

    Message receive() override;

    void abort(std::exception_ptr) override;

    void send(const Message& msg) override;

    void bufferedSend(const Message& msg) override;

    void createPeers() const override;

    void print(std::ostream& os) const override;

    const Peer& localPeer() const override;

    void listen() override;

    PeerList createServerPeers() const override;

    const eckit::mpi::Comm& comm() const;

    eckit::mpi::Status probe();
    size_t blockingReceive(eckit::mpi::Status& status, MpiBuffer& buffer);

    void encodeMessage(eckit::Stream& strm, const Message& msg);

    MpiPeer local_;
    eckit::mpi::Group parentGroup_;
    eckit::mpi::Group clientGroup_;
    eckit::mpi::Group serverGroup_;

    StreamPool pool_;

    eckit::Queue<ReceivedBuffer> streamQueue_;

    std::queue<Message> msgPack_;
};

}  // namespace multio::transport

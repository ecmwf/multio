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

#include <optional>
#include <queue>
#include <string>

#include "eckit/container/Queue.h"
#include "eckit/io/Buffer.h"
#include "eckit/log/Statistics.h"
#include "eckit/serialisation/ResizableMemoryStream.h"

#include "multio/server/StepMeter.h"
#include "multio/transport/StreamPool.h"
#include "multio/transport/Transport.h"

namespace eckit {
namespace mpi {
class Comm;
}
}  // namespace eckit

namespace multio::transport {

struct ReceivedBuffer {
    MpiBuffer* buffer;
    size_t size;
};

struct MpiPeerSetup {
    MpiPeer localPeer;
    std::string parentCommName;
    std::string clientCommName;
    std::string serverCommName;
};

class MpiTransport final : public Transport {
public:
    MpiTransport(const ComponentConfiguration& compConf);
    ~MpiTransport();

private:
    MpiTransport(const ComponentConfiguration& compConf, MpiPeerSetup&& peerSetup);

    void openConnections() override;
    void closeConnections() override;

    void synchronize(const Message& msg = Message{}) override;

    void reportStep(const Message& msg);

    Message receive() override;

    void abort(std::exception_ptr) override;

    void send(const Message& msg) override;

    void bufferedSend(const Message& msg) override;

    void createPeers() const override;

    void print(std::ostream& os) const override;

    const Peer& localPeer() const override;

    void listen() override;

    PeerList createServerPeers() const override;

    eckit::mpi::Status probe();
    size_t blockingReceive(eckit::mpi::Status& status, MpiBuffer& buffer);

    void encodeMessage(eckit::Stream& strm, const Message& msg);

    size_t getMpiPoolSize(const ComponentConfiguration& compConf);
    size_t getMpiBufferSize(const ComponentConfiguration& compConf);

    const eckit::mpi::Comm& comm() const;
    const eckit::mpi::Comm& clientComm() const;
    const eckit::mpi::Comm& serverComm() const;

    MpiPeer local_;
    std::string parentCommName_;
    std::string clientCommName_;
    std::string serverCommName_;

    StreamPool pool_;

    eckit::Queue<ReceivedBuffer> streamQueue_;

    std::queue<Message> msgPack_;

    std::optional<server::StepMeter> stepMeter_ = std::nullopt;  // Only used by servers!

#ifdef HAVE_ECFLOW_LIGHT
    // Pacing state (persistent across synchronize calls)
    double startTime_ = 0.0;
    double lastTime_ = 0.0;
    double paceLastStep_ = 0.0;
    int lastForecastHour_ = 0;
    bool firstReportCall_ = true;
    double paceWarnThreshold_ = 0.0;
#endif
};

}  // namespace multio::transport

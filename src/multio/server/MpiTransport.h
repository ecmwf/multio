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

#ifndef multio_server_MpiTransport_H
#define multio_server_MpiTransport_H

#include <queue>

#include "eckit/io/ResizableBuffer.h"
#include "eckit/log/Statistics.h"
#include "eckit/mpi/Comm.h"
#include "eckit/serialisation/ResizableMemoryStream.h"

#include "multio/server/Transport.h"
#include "multio/server/MpiStream.h"

namespace multio {
namespace server {

class MpiPeer : public Peer {
public:
    MpiPeer(Peer peer);
    MpiPeer(const std::string& comm, size_t rank);
};

struct StreamPool {
    explicit StreamPool(size_t poolSize, size_t maxBufSize);

    MpiBuffer& buffer(size_t idx);

    MpiStream& getStream(const message::Peer& dest);
    void removeStream(const message::Peer& dest);

    MpiBuffer& findAvailableBuffer();

    void printPoolStatus() const;

private:
    std::vector<MpiBuffer> buffers_;
    std::map<MpiPeer, MpiStream> streams_;
};


class MpiTransport final : public Transport {
public:
    MpiTransport(const eckit::Configuration& config);
    ~MpiTransport();

private:
    Message receive() override;

    void send(const Message& msg) override;

    void print(std::ostream& os) const override;

    Peer localPeer() const override;

    void blockingSend(const Message& msg);

    MpiPeer local_;

    eckit::ResizableBuffer buffer_;

    StreamPool pool_;

    std::queue<Message> msgPack_;

    eckit::Timing sendTiming_;
    eckit::Timing receiveTiming_;
    eckit::Timing bufferWaitTiming_;

    std::size_t bytesSent_ = 0;
    std::size_t bytesReceived_ = 0;
};

}  // namespace server
}  // namespace multio

#endif

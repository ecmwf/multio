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

class StreamPool {
public:
    explicit StreamPool(size_t poolSize, size_t maxBufSize, const eckit::mpi::Comm& comm);

    MpiBuffer& buffer(size_t idx);

    MpiStream& getStream(const message::Message& msg);
    void removeStream(const message::Peer& dest);

    void send(const message::Message& msg);

    void timings(std::ostream& os) const;

private:
    MpiBuffer& findAvailableBuffer();

    MpiStream& createNewStream(const message::Peer& dest);

    void print(std::ostream& os) const;

    friend std::ostream& operator<<(std::ostream& os, const StreamPool& transport) {
        transport.print(os);
        return os;
    }

    const eckit::mpi::Comm& comm_;
    std::vector<MpiBuffer> buffers_;
    std::map<MpiPeer, MpiStream> streams_;

    eckit::Timing sendTiming_;
    eckit::Timing waitTiming_;

    std::size_t bytesSent_ = 0;
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

    const eckit::mpi::Comm& comm() const;

    MpiPeer local_;

    eckit::ResizableBuffer buffer_;

    StreamPool pool_;

    std::queue<Message> msgPack_;

    eckit::Timing receiveTiming_;

    std::size_t bytesReceived_ = 0;
};

}  // namespace server
}  // namespace multio

#endif

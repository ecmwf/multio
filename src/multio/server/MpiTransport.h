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

namespace multio {
namespace server {

inline std::vector<eckit::ResizableBuffer> makeBuffers(size_t poolSize, size_t maxBufSize) {
    std::vector<eckit::ResizableBuffer> bufs;
    for (auto ii = 0u; ii < poolSize; ++ii) {
        bufs.emplace_back(eckit::ResizableBuffer{maxBufSize});
    }
    return bufs;
}

struct BufferPool{
    explicit BufferPool(size_t poolSize, size_t maxBufSize) :
        request(poolSize), buffer{makeBuffers(poolSize, maxBufSize)} {}

    std::vector<eckit::mpi::Request> request;
    std::vector<eckit::ResizableBuffer> buffer;
};

class MpiPeer : public Peer {
public:
    MpiPeer(Peer peer);
    MpiPeer(const std::string& comm, size_t rank);
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

    void nonBlockingSend(const Message& msg);

    size_t findAvailableBuffer(const eckit::mpi::Comm& comm);

    MpiPeer local_;

    eckit::ResizableBuffer buffer_;

    BufferPool pool_;

    struct MpiStream : public eckit::ResizableMemoryStream {
        MpiStream(eckit::ResizableBuffer& buf) : eckit::ResizableMemoryStream{buf}, buf_{buf} {}
        void setRequest(eckit::mpi::Request& req) {req_ = &req;}
        eckit::mpi::Request& request() { return *req_; }
        eckit::ResizableBuffer& buffer() const { return buf_; }

        eckit::mpi::Request* req_;
        eckit::ResizableBuffer& buf_;
    };
    std::map<MpiPeer, MpiStream> streams_;

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

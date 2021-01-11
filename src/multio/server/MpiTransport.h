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

#include "eckit/io/ResizableBuffer.h"
#include "eckit/log/Statistics.h"
#include "eckit/mpi/Comm.h"
#include "eckit/serialisation/ResizableMemoryStream.h"

#include "multio/server/Transport.h"

namespace multio {
namespace server {

inline std::vector<eckit::ResizableBuffer> makeBuffers(std::size_t sz) {
    std::vector<eckit::ResizableBuffer> bufs;
    for (auto ii = 0u; ii < sz; ++ii) {
        bufs.emplace_back(eckit::ResizableBuffer{0});
    }
    return bufs;
}

struct BufferList{
    explicit BufferList(std::size_t sz) : size_{sz}, request(size_), buffer{makeBuffers(size_)} {}

    std::size_t size_;
    std::vector<eckit::mpi::Request> request;
    std::vector<eckit::ResizableBuffer> buffer;
};

class MpiPeer : public Peer {
public:
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

    MpiPeer local_;

    eckit::ResizableBuffer buffer_;

    BufferList bufList_;

    eckit::Timing sendTiming_;
    eckit::Timing receiveTiming_;
};

}  // namespace server
}  // namespace multio

#endif

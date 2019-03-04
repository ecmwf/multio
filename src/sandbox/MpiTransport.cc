/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "MpiTransport.h"

#include "eckit/config/Resource.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/maths/Functions.h"
#include "eckit/serialisation/MemoryStream.h"

namespace multio {
namespace sandbox {

MpiTransport::MpiTransport(const eckit::Configuration& cfg) :
    Transport(cfg),
    comm_name_(cfg.getString("domain")),
    buffer_{0} {}

Message MpiTransport::receive() {
    Message msg{};

    const auto& comm = eckit::mpi::comm(comm_name_.c_str());

    auto status = comm.probe(comm.anySource(), comm.anyTag());

    buffer_.resize(eckit::round(comm.getCount<void>(status), 8));

    comm.receive<void>(buffer_, buffer_.size(), status.source(), status.tag());

    eckit::ResizableMemoryStream stream{buffer_};

    msg.decode(stream);

    return msg;
}

void MpiTransport::send(const Message& msg) {

    auto msg_tag = static_cast<int>(msg.tag());

    auto dest = msg.destination().id_;

    // Add 4K for header/footer etc. Should be plenty
    buffer_.resize(eckit::round(msg.size(), 8) + 4096);

    eckit::ResizableMemoryStream stream{buffer_};

    msg.encode(stream);

    eckit::mpi::comm(comm_name_.c_str()).send<void>(buffer_, stream.bytesWritten(), dest, msg_tag);
}

Peer MpiTransport::localPeer() const {
    return Peer{comm_name_, eckit::mpi::comm(comm_name_.c_str()).rank()};
}

void MpiTransport::print(std::ostream& os) const {
    os << "MpiTransport()";
}

static TransportBuilder<MpiTransport> MpiTransportBuilder("Mpi");

}  // namespace sandbox
}  // namespace multio

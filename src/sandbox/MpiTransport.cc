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

/// @date Feb 2019

#include "MpiTransport.h"

#include "eckit/config/Resource.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/maths/Functions.h"
#include "eckit/serialisation/MemoryStream.h"

namespace multio {
namespace sandbox {

MpiTransport::MpiTransport(const eckit::Configuration& cfg) :
    Transport(cfg),
    comm_(eckit::mpi::comm(cfg.getString("domain").c_str())),
    buffer_{0} {}

Message MpiTransport::receive() {
    Message msg{};

    Peer receiver = localPeer();
    const auto& domain_name = receiver.domain_;

    ASSERT(comm_.communicator() == eckit::mpi::comm(domain_name.c_str()).communicator());

    auto status = comm_.probe(comm_.anySource(), comm_.anyTag());

    buffer_.resize(eckit::round(comm_.getCount<void>(status), 8));
    comm_.receive<void>(buffer_, buffer_.size(), status.source(), status.tag());

    eckit::ResizableMemoryStream stream{buffer_};

    msg.decode(stream);

    return msg;
}

void MpiTransport::send(const Message& msg) {

    ASSERT(comm_.communicator() == eckit::mpi::comm(localPeer().domain_.c_str()).communicator());

    auto msg_tag = static_cast<int>(msg.tag());

    auto dest = msg.destination().id_;

    // Add 4K for header/footer etc. Should be plenty
    buffer_.resize(eckit::round(msg.size(), 8) + 4096);

    eckit::ResizableMemoryStream stream{buffer_};

    msg.encode(stream);

    comm_.send<void>(buffer_, stream.bytesWritten(), dest, msg_tag);
}

Peer MpiTransport::localPeer() const {
    Peer peer{"world", comm_.rank()};
    return peer;
}

void MpiTransport::print(std::ostream& os) const {
    os << "MpiTransport()";
}

static TransportBuilder<MpiTransport> MpiTransportBuilder("Mpi");

}  // namespace sandbox
}  // namespace multio

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
#include "eckit/serialisation/MemoryStream.h"

namespace multio {
namespace sandbox {

MpiTransport::MpiTransport(const eckit::Configuration& cfg) :
    Transport(cfg),
    comm_(eckit::mpi::comm(cfg.getString("domain").c_str())) {}

Message MpiTransport::receive() {
    Message msg{};

    Peer receiver = localPeer();
    const auto& domain_name = receiver.domain_;

    ASSERT(comm_.communicator() == eckit::mpi::comm(domain_name.c_str()).communicator());

    auto status = comm_.probe(comm_.anySource(), comm_.anyTag());

    eckit::Buffer buffer{comm_.getCount<void>(status)};
    comm_.receive<void>(buffer, buffer.size(), status.source(), status.tag());

    eckit::MemoryStream strm(buffer);
    msg.decode(strm);
    return msg;
}

void MpiTransport::send(const Message& msg) {

    ASSERT(comm_.communicator() == eckit::mpi::comm(localPeer().domain_.c_str()).communicator());

    auto msg_tag = static_cast<int>(msg.tag());

    auto dest = msg.destination().id_;

    eckit::Buffer buffer(
        eckit::Resource<size_t>("multioMessageQueueSize;$MULTIO_MESSAGE_SIZE", 1024 * 1024));
    eckit::MemoryStream strm(buffer);
    msg.encode(strm);

    comm_.send<void>(buffer, strm.bytesWritten(), dest, msg_tag);
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

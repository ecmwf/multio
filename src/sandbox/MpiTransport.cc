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

#include "eckit/exception/Exceptions.h"

namespace multio {
namespace sandbox {

MpiTransport::MpiTransport(const eckit::Configuration& cfg) :
    Transport(cfg),
    comm_(eckit::mpi::comm(cfg.getString("domain").c_str())) {}

Message MpiTransport::receive() {

    Peer receiver = localPeer();
    const auto& domain_name = receiver.domain_;

    ASSERT(comm_.communicator() == eckit::mpi::comm(domain_name.c_str()).communicator());

    auto status = comm_.probe(comm_.anySource(), comm_.anyTag());

    auto msg_tag = static_cast<Message::Tag>(status.tag());

    auto source = Peer{domain_name, static_cast<size_t>(status.source())};

    auto dest = localPeer();

    eckit::Buffer payload{comm_.getCount<void>(status)};
    comm_.receive<void>(payload, payload.size(), status.source(), status.tag());

    return Message{msg_tag, source, dest, payload};
}

void MpiTransport::send(const Message& msg) {

    ASSERT(comm_.communicator() == eckit::mpi::comm(localPeer().domain_.c_str()).communicator());

    auto msg_tag = static_cast<int>(msg.tag());

    auto dest = msg.destination().id_;

    comm_.send<void>(msg.payload(), msg.size(), dest, msg_tag);
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

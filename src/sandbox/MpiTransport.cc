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

MpiTransport::MpiTransport(const eckit::Configuration& cfg) : Transport(cfg) {}

MpiTransport::~MpiTransport() = default;

Message MpiTransport::receive() {

    Peer receiver = localPeer();

    const auto& domain_name = receiver.domain_.c_str();
    const auto& comm = eckit::mpi::comm(domain_name);

    auto status = comm.probe(comm.anySource(), comm.anyTag());

    eckit::Buffer payload{comm.getCount<void>(status)};
    comm.receive<void>(payload, payload.size(), status.source(), status.tag());

    auto msg_tag = static_cast<Message::Tag>(status.tag());
    auto source = Peer{domain_name, static_cast<size_t>(status.source())};
    auto dest = localPeer();

    return Message{msg_tag, source, dest, payload};
}

void MpiTransport::send(const Message& msg) {

    const auto& comm = eckit::mpi::comm(msg.destination().domain_.c_str());

    auto dest = msg.destination().id_;
    auto msg_tag = static_cast<int>(msg.tag());

    comm.send<void>(msg.payload(), msg.size(), dest, msg_tag);
}

Peer MpiTransport::localPeer() const
{
    Peer peer{"world", eckit::mpi::comm("world").rank()};
    return peer;
}

void MpiTransport::print(std::ostream& os) const {
    os << "MpiTransport()";
}

static TransportBuilder<MpiTransport> MpiTransportBuilder("Mpi");

}  // namespace sandbox
}  // namespace multio

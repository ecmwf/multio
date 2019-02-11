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

MpiTransport::MpiTransport(const eckit::Configuration& cfg, const std::string& name) :
    Transport(cfg),
    comm_name_(name),
    comm_(eckit::mpi::comm(comm_name_.c_str())) {
    eckit::mpi::setCommDefault(comm_name_.c_str());
}

MpiTransport::~MpiTransport() = default;

Message MpiTransport::receive() {
    auto status = comm_.probe(comm_.anySource(), comm_.anyTag());

    eckit::Buffer payload{comm_.getCount<void>(status)};
    comm_.receive<void>(payload, payload.size(), status.source(), status.tag());

    auto msg_tag = static_cast<Message::Tag>(status.tag());
    auto source = Peer{comm_name_, comm_.rank()};
    auto dest = localPeer();

    return Message{msg_tag, source, dest, payload};
}

void MpiTransport::send(const Message& msg) {

    ASSERT(comm_name_ == msg.destination().domain_);

    auto dest = msg.destination().id_;
    auto msg_tag = static_cast<int>(msg.tag());

    comm_.send<void>(msg.payload(), msg.size(), dest, msg_tag);
}

Peer MpiTransport::localPeer() const
{
    Peer peer{std::to_string(comm_.communicator()), comm_.rank()};
    return peer;
}

void MpiTransport::print(std::ostream& os) const {
    os << "MpiTransport(size of the communicator = " << comm_.size() << ")";
}

static TransportBuilder<MpiTransport> MpiTransportBuilder("Mpi");

}  // namespace sandbox
}  // namespace multio

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

#include "multio/util/ScopedTimer.h"

namespace multio {
namespace server {

namespace {
Message decodeMessage(eckit::Stream& stream) {
    unsigned t;
    stream >> t;

    std::string src_grp;
    stream >> src_grp;
    size_t src_id;
    stream >> src_id;

    std::string dest_grp;
    stream >> dest_grp;
    size_t dest_id;
    stream >> dest_id;

    Message msg{Message::Header{static_cast<Message::Tag>(t), MpiPeer{src_grp, src_id},
                                MpiPeer{dest_grp, dest_id}}};

    msg.decode(stream);

    return msg;
}
}  // namespace


MpiPeer::MpiPeer(const std::string& comm, size_t rank) : Peer{comm, rank} {}

MpiTransport::MpiTransport(const eckit::Configuration& cfg) :
    Transport(cfg),
    local_{cfg.getString("group"), eckit::mpi::comm(cfg.getString("group").c_str()).rank()},
    buffer_{0} {}

MpiTransport::~MpiTransport() {
    std::ostringstream os;
    os << " ******* " << *this << std::endl
       << "         -- Total wall-clock time spent sending data: " << sendTiming_ << "s"
       << std::endl
       << "         -- Total wall-clock time spent receiving data: " << receiveTiming_ << "s"
       << std::endl;
    eckit::Log::info() << os.str();
}

Message MpiTransport::receive() {
    util::ScopedTimer scTimer{receiveTiming_};

    const auto& comm = eckit::mpi::comm(local_.group().c_str());

    auto status = comm.probe(comm.anySource(), comm.anyTag());

    buffer_.resize(eckit::round(comm.getCount<void>(status), 8));

    comm.receive<void>(buffer_, buffer_.size(), status.source(), status.tag());

    eckit::ResizableMemoryStream stream{buffer_};

    return decodeMessage(stream);
}

void MpiTransport::send(const Message& msg) {
    util::ScopedTimer scTimer{sendTiming_};

    auto msg_tag = static_cast<int>(msg.tag());

    // Add 4K for header/footer etc. Should be plenty
    buffer_.resize(eckit::round(msg.size(), 8) + 4096);

    eckit::ResizableMemoryStream stream{buffer_};

    msg.encode(stream);

    auto sz = static_cast<size_t>(stream.bytesWritten());
    auto dest = static_cast<int>(msg.destination().id());
    eckit::mpi::comm(local_.group().c_str()).send<void>(buffer_, sz, dest, msg_tag);
}

Peer MpiTransport::localPeer() const {
    return local_;
}

void MpiTransport::print(std::ostream& os) const {
    os << "MpiTransport(" << local_ << ")";
}

static TransportBuilder<MpiTransport> MpiTransportBuilder("mpi");

}  // namespace server
}  // namespace multio

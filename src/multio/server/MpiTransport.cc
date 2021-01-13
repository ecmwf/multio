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

#include <algorithm>

#include "eckit/config/Resource.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/maths/Functions.h"
#include "eckit/serialisation/MemoryStream.h"

#include "multio/util/ScopedTimer.h"
#include "multio/util/print_buffer.h"

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

    std::string fieldId;
    stream >> fieldId;

    unsigned long sz;
    stream >> sz;

    eckit::Buffer buffer(sz);
    stream >> buffer;

    return Message{Message::Header{static_cast<Message::Tag>(t), MpiPeer{src_grp, src_id},
                                MpiPeer{dest_grp, dest_id}, std::move(fieldId)},
                std::move(buffer)};
}
}  // namespace


MpiPeer::MpiPeer(const std::string& comm, size_t rank) : Peer{comm, rank} {}

MpiTransport::MpiTransport(const eckit::Configuration& cfg) :
    Transport(cfg),
    local_{cfg.getString("group"), eckit::mpi::comm(cfg.getString("group").c_str()).rank()},
    buffer_{0},
    bufList_{128} {} // TODO: use eckit::Resource

MpiTransport::~MpiTransport() {
    // TODO: check why eckit::Log::info() crashes here for the clients
    const std::size_t scale = 1024*1024;
    std::ostringstream os;
    os << " ******* " << *this
       << "\n         -- Waiting for buffers: " << bufferWaitTiming_
       << "s\n         -- Sending data:        " << bytesSent_ / scale << " MiB, " << sendTiming_
       << "s\n         -- Receiving data:      " << bytesReceived_ / scale << " MiB, " << receiveTiming_
       << "s" << std::endl;

    std::cout << os.str();
}

Message MpiTransport::receive() {
    const auto& comm = eckit::mpi::comm(local_.group().c_str());

    auto status = comm.probe(comm.anySource(), comm.anyTag());

    buffer_.resize(eckit::round(comm.getCount<void>(status), 8));

    {
        util::ScopedTimer scTimer{receiveTiming_};
        comm.receive<void>(buffer_, buffer_.size(), status.source(), status.tag());
    }

    bytesReceived_ += buffer_.size();
    eckit::ResizableMemoryStream stream{buffer_};

    return decodeMessage(stream);
}

void MpiTransport::send(const Message& msg) {

    nonBlockingSend(msg);

//    auto msg_tag = static_cast<int>(msg.tag());

//    // Add 4K for header/footer etc. Should be plenty
//    buffer_.resize(eckit::round(msg.size(), 8) + 4096);

//    eckit::ResizableMemoryStream stream{buffer_};

//    msg.encode(stream);

//    auto sz = static_cast<size_t>(stream.bytesWritten());
//    auto dest = static_cast<int>(msg.destination().id());

//    util::ScopedTimer scTimer{sendTiming_};

//    eckit::mpi::comm(local_.group().c_str()).send<void>(buffer_, sz, dest, msg_tag);
}

Peer MpiTransport::localPeer() const {
    return local_;
}

void MpiTransport::nonBlockingSend(const Message& msg) {
    auto msg_tag = static_cast<int>(msg.tag());

    const auto& comm = eckit::mpi::comm(local_.group().c_str());

    auto it = std::find_if(std::begin(bufList_.request), std::end(bufList_.request),
                           [](eckit::mpi::Request& req) { return req.test(); });

    int idx;
    if (it == std::end(bufList_.request)) {
        util::ScopedTimer scTimer{bufferWaitTiming_};
        auto status = comm.waitAny(bufList_.request, idx);
        // eckit::Log::info() << " *** " << *this << " *** use just-freed buffer idx = " << idx << std::endl;
    }
    else {
        idx = std::distance(std::begin(bufList_.request), it);
        // eckit::Log::info() << " *** " << *this << " *** use available buffer idx = " << idx << std::endl;
    }

    util::ScopedTimer scTimer{sendTiming_};

    // Add 4K for header/footer etc. Should be plenty
    auto& buf = bufList_.buffer[idx];

    buf.resize(eckit::round(msg.size(), 8) + 4096);

    bytesSent_ += buf.size();
    eckit::ResizableMemoryStream stream{buf};

    msg.encode(stream);

    auto sz = static_cast<size_t>(stream.bytesWritten());
    auto dest = static_cast<int>(msg.destination().id());

    bufList_.request[idx] = comm.iSend<void>(buf, sz, dest, msg_tag);
}

void MpiTransport::print(std::ostream& os) const {
    os << "MpiTransport(" << local_ << ")";
}

static TransportBuilder<MpiTransport> MpiTransportBuilder("mpi");

}  // namespace server
}  // namespace multio

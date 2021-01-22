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
#include "unistd.h"

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
    eckit::Log::info() << " *** Tag = " << t << " -- Reading source group" << std::endl;
    stream >> src_grp;
    size_t src_id;
    stream >> src_id;

    std::string dest_grp;
    eckit::Log::info() << " *** Reading dest group" << std::endl;
    stream >> dest_grp;
    size_t dest_id;
    stream >> dest_id;

    std::string fieldId;
    eckit::Log::info() << " *** Reading field id" << std::endl;
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
MpiPeer::MpiPeer(Peer peer) : Peer{peer} {}

MpiTransport::MpiTransport(const eckit::Configuration& cfg) :
    Transport(cfg),
    local_{cfg.getString("group"), eckit::mpi::comm(cfg.getString("group").c_str()).rank()},
    buffer_{64*1024*1024},
    pool_{128, 64*1024*1024} {} // TODO: use eckit::Resource
//     buffer_{
//         eckit::Resource<size_t>("multioMpiBufferSize;$MULTIO_MPI_BUFFER_SIZE", 64 * 1024 * 1024)},
//     pool_{
//         eckit::Resource<size_t>("multioMpiPoolSize;$MULTIO_MPI_POOL_SIZE", 64 * 1024 * 1024),
//         eckit::Resource<size_t>("multioMpiBufferSize;$MULTIO_MPI_BUFFER_SIZE", 64 * 1024 * 1024)} {
// }  // TODO: use eckit::Resource

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

    while (not msgPack_.empty()) {
        auto msg = msgPack_.front();
        msgPack_.pop();
        return msg;
    }
    const auto& comm = eckit::mpi::comm(local_.group().c_str());

    auto status = comm.probe(comm.anySource(), comm.anyTag());

    auto sz = comm.getCount<void>(status);
    ASSERT(sz < buffer_.size());

    ::sleep(1);

    eckit::Log::info() << " *** " << local_ << " -- Received " << sz
                       << " bytes (to put into buffer sized " << buffer_.size() << ") from source "
                       << status.source() << std::endl;

    ::sleep(1);

    {
        util::ScopedTimer scTimer{receiveTiming_};
        comm.receive<void>(buffer_, sz, status.source(), status.tag());
    }

    bytesReceived_ += sz;

    eckit::ResizableMemoryStream stream{buffer_};

    while (stream.position() < sz) {
        auto msg = decodeMessage(stream);
        eckit::Log::info() << " *** " << local_ << " -- " << msg << std::endl;
        msgPack_.push(msg);
        eckit::Log::info() << " *** " << local_ << " -- position = " << stream.position()
                           << ", size = " << sz << std::endl;
    }

    auto msg = msgPack_.front();
    msgPack_.pop();
    return msg;
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

    std::for_each(std::begin(pool_.request), std::end(pool_.request), [](eckit::mpi::Request& req) {
        eckit::Log::info() << (req.test() ? "free" : "used") << " ";
    });
    eckit::Log::info() << std::endl;

    if (streams_.find(msg.destination()) == std::end(streams_)) {
        // Find an available buffer
        auto idx = findAvailableBuffer(comm);
        streams_.emplace(msg.destination(), pool_.buffer[idx]);
        streams_.at(msg.destination()).setRequest(pool_.request[idx]);
    }

    auto& strm = streams_.at(msg.destination());
    if (strm.buffer().size() < strm.position() + msg.size() + 4096) {
        util::ScopedTimer scTimer{sendTiming_};

        auto sz = static_cast<size_t>(strm.bytesWritten());
        auto dest = static_cast<int>(msg.destination().id());
        eckit::Log::info() << " *** " << local_ << " -- Sending " << sz << " bytes to destination "
                           << msg.destination() << std::endl;
        strm.request() = comm.iSend<void>(strm.buffer(), sz, dest, msg_tag);

        bytesSent_ += sz;

        streams_.erase(msg.destination());

        auto idx = findAvailableBuffer(comm);
        streams_.emplace(msg.destination(), pool_.buffer[idx]);
        streams_.at(msg.destination()).setRequest(pool_.request[idx]);
    }

    eckit::Log::info() << " *** Encode " << msg << " into stream for " << msg.destination()
                       << std::endl;
    msg.encode(streams_.at(msg.destination()));
    if (msg.tag() == Message::Tag::Close) {
        util::ScopedTimer scTimer{sendTiming_};
        // Shadow on purpuse
        auto& strm = streams_.at(msg.destination());
        auto sz = static_cast<size_t>(strm.bytesWritten());
        auto dest = static_cast<int>(msg.destination().id());
        eckit::Log::info() << " *** " << local_ << " -- Sending " << sz << " bytes to destination "
                           << msg.destination() << std::endl;
        comm.send<void>(strm.buffer(), sz, dest, msg_tag);
        bytesSent_ += sz;
    }
}

size_t MpiTransport::findAvailableBuffer(const eckit::mpi::Comm& comm) {
    auto it = std::find_if(std::begin(pool_.mask), std::end(pool_.mask),
                           [](uint8_t isUsed) { return isUsed == 0; });

    int idx;
    if (it == std::end(pool_.mask)) {
        util::ScopedTimer scTimer{bufferWaitTiming_};
        auto status = comm.waitAny(pool_.request, idx);
        pool_.mask[idx] = 0;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          pool_.mask[idx] = 0;
        // eckit::Log::info() << " *** " << *this << " *** use just-freed buffer idx = " << idx << std::endl;
    }
    else {
        idx = std::distance(std::begin(pool_.mask), it);
        // eckit::Log::info() << " *** " << *this << " *** use available buffer idx = " << idx << std::endl;
    }

    eckit::Log::info() << " *** " << local_ << " -- Found available buffer with idx = " << idx
                       << std::endl;
    pool_.mask[idx] = 1;
    return static_cast<size_t>(idx);
}

void MpiTransport::print(std::ostream& os) const {
    os << "MpiTransport(" << local_ << ")";
}

static TransportBuilder<MpiTransport> MpiTransportBuilder("mpi");

}  // namespace server
}  // namespace multio

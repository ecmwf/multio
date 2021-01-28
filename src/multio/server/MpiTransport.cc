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

std::vector<MpiBuffer> makeBuffers(size_t poolSize, size_t maxBufSize) {
    std::vector<MpiBuffer> bufs;
    eckit::Log::info() << " *** Allocating " << poolSize << " buffers of size " << maxBufSize
                       << " each" << std::endl;
    size_t totMem = 0;
    for (auto ii = 0u; ii < poolSize; ++ii) {
        bufs.emplace_back(maxBufSize);
        totMem += maxBufSize;
    }
    totMem /= 1024*1024*1024;
    eckit::Log::info() << " *** Allocated a total of " << totMem << "GiB of memory for this peer"
                       << std::endl;
    return bufs;
}

}  // namespace


MpiPeer::MpiPeer(const std::string& comm, size_t rank) : Peer{comm, rank} {}
MpiPeer::MpiPeer(Peer peer) : Peer{peer} {}

StreamPool::StreamPool(size_t poolSize, size_t maxBufSize) :
    buffers_(makeBuffers(poolSize, maxBufSize)) {}

MpiBuffer& StreamPool::buffer(size_t idx) {
    return buffers_[idx];
}

MpiStream& StreamPool::getStream(const message::Peer& dest) {
    if (streams_.find(dest) != std::end(streams_)) {
        return streams_.at(dest);
    }

    ASSERT_MSG(streams_.size() < buffers_.size(), "Too few buffers to cover all MPI destinations");

    auto& buf = findAvailableBuffer();
    streams_.emplace(dest, buf);
    buf.status = BufferStatus::fillingUp;

    return streams_.at(dest);
}

void StreamPool::removeStream(const message::Peer& dest) {
    streams_.erase(dest);
}

MpiBuffer& StreamPool::findAvailableBuffer() {
    auto it = std::end(buffers_);
    while (it == std::end(buffers_)) {
        it = std::find_if(std::begin(buffers_), std::end(buffers_),
                          [](MpiBuffer& buf) { return buf.isFree(); });
    }
    return *it;
}

void StreamPool::print(std::ostream& os) const {
    os << "StreamPool(size=" << buffers_.size() << ",status=";
    std::for_each(std::begin(buffers_), std::end(buffers_),
                  [&os](const MpiBuffer& buf) { os << static_cast<unsigned>(buf.status); });
    os << ")";
}

MpiTransport::MpiTransport(const eckit::Configuration& cfg) :
    Transport(cfg),
    local_{cfg.getString("group"), eckit::mpi::comm(cfg.getString("group").c_str()).rank()},
    buffer_{
        eckit::Resource<size_t>("multioMpiBufferSize;$MULTIO_MPI_BUFFER_SIZE", 64 * 1024 * 1024)},
    pool_{
        eckit::Resource<size_t>("multioMpiPoolSize;$MULTIO_MPI_POOL_SIZE", 128),
        eckit::Resource<size_t>("multioMpiBufferSize;$MULTIO_MPI_BUFFER_SIZE", 64 * 1024 * 1024)} {}

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

    {
        util::ScopedTimer scTimer{receiveTiming_};
        comm.receive<void>(buffer_, sz, status.source(), status.tag());
    }

    bytesReceived_ += sz;

    eckit::ResizableMemoryStream stream{buffer_};

    while (stream.position() < sz) {
        auto msg = decodeMessage(stream);
        msgPack_.push(msg);
    }

    auto msg = msgPack_.front();
    msgPack_.pop();
    return msg;
}

void MpiTransport::send(const Message& msg) {
    auto msg_tag = static_cast<int>(msg.tag());

    const auto& comm = eckit::mpi::comm(local_.group().c_str());

    // Note: it would be more elegant to wait until *after* we have encoded to message to make the
    // decision on whether to send the buffer or not -- but then we don't yet
    // have the information about whether the next message will fit in the buffer at all
    auto& strm = pool_.getStream(msg.destination());
    if (strm.readyToSend(msg.size())) {
        util::ScopedTimer scTimer{sendTiming_};

        auto sz = static_cast<size_t>(strm.bytesWritten());
        auto dest = static_cast<int>(msg.destination().id());
        strm.buffer().request = comm.iSend<void>(strm.buffer().content, sz, dest, msg_tag);
        strm.buffer().status = BufferStatus::transmitting;

        bytesSent_ += sz;

        pool_.removeStream(msg.destination());
    }

    msg.encode(pool_.getStream(msg.destination()));
    if (msg.tag() == Message::Tag::Close) {  // Send it now
        blockingSend(msg);
    }
}

Peer MpiTransport::localPeer() const {
    return local_;
}

void MpiTransport::blockingSend(const Message& msg) {
    util::ScopedTimer scTimer{sendTiming_};
    // Shadow on purpuse
    auto& strm = pool_.getStream(msg.destination());
    auto sz = static_cast<size_t>(strm.bytesWritten());
    auto dest = static_cast<int>(msg.destination().id());
    eckit::mpi::comm(local_.group().c_str())
        .send<void>(strm.buffer().content, sz, dest, static_cast<int>(msg.tag()));
    bytesSent_ += sz;
}

void MpiTransport::print(std::ostream& os) const {
    os << "MpiTransport(" << local_ << ")";
}

static TransportBuilder<MpiTransport> MpiTransportBuilder("mpi");

}  // namespace server
}  // namespace multio

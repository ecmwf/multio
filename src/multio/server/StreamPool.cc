
#include "StreamPool.h"

#include <algorithm>

#include "eckit/exception/Exceptions.h"
#include "eckit/mpi/Comm.h"
#include "multio/util/ScopedTimer.h"


namespace multio {
namespace server {

namespace  {
std::vector<MpiBuffer> makeBuffers(size_t poolSize, size_t maxBufSize) {
    std::vector<MpiBuffer> bufs;
    eckit::Log::info() << " *** qAllocating " << poolSize << " buffers of size "
                       << maxBufSize / 1024 / 1024 << " each" << std::endl;
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

StreamPool::StreamPool(size_t poolSize, size_t maxBufSize, const eckit::mpi::Comm& comm) :
    comm_{comm}, buffers_(makeBuffers(poolSize, maxBufSize)) {}

MpiBuffer& StreamPool::buffer(size_t idx) {
    return buffers_[idx];
}

MpiStream& StreamPool::getStream(const message::Message& msg) {
    auto dest = msg.destination();

    if (streams_.find(dest) == std::end(streams_)) {
        return createNewStream(dest);
    }

    auto& strm = streams_.at(dest);
    if (strm.canFitMessage(msg.size())) {
        return strm;
    }

    util::ScopedTimer scTimer{sendTiming_};

    auto sz = static_cast<size_t>(strm.bytesWritten());
    auto destId = static_cast<int>(dest.id());
    auto msg_tag = static_cast<int>(msg.tag());
    // strm.buffer().request = comm_.iSend<void>(strm.buffer().content, sz, destId, msg_tag);
    // strm.buffer().status = BufferStatus::transmitting;

    strm.buffer().status = BufferStatus::available;

    bytesSent_ += sz;

    return replaceStream(dest);
}

MpiStream& StreamPool::replaceStream(const message::Peer& dest) {
    streams_.erase(dest);
    return createNewStream(dest);
}

void StreamPool::send(const message::Message& msg) {
    util::ScopedTimer scTimer{sendTiming_};

    auto& strm = getStream(msg);
    auto sz = static_cast<size_t>(strm.bytesWritten());
    auto destId = static_cast<int>(msg.destination().id());
    comm_.send<void>(strm.buffer().content, sz, destId, static_cast<int>(msg.tag()));
    bytesSent_ += sz;
}

MpiBuffer& StreamPool::findAvailableBuffer() {
    auto it = std::end(buffers_);
    while (it == std::end(buffers_)) {
        it = std::find_if(std::begin(buffers_), std::end(buffers_),
                          [](MpiBuffer& buf) { return buf.isFree(); });
    }

    // eckit::Log::info() << " *** -- Found available buffer with idx = "
    //                    << static_cast<size_t>(std::distance(std::begin(buffers_), it)) << std::endl;

    return *it;
}

void StreamPool::timings(std::ostream &os) const
{
    const std::size_t scale = 1024*1024;
    os << "         -- Waiting for buffer: " << waitTiming_ << "s\n"
       << "         -- Sending data:       " << bytesSent_ / scale << " MiB, " << sendTiming_
       << "s";
}

MpiStream& StreamPool::createNewStream(const message::Peer& dest) {
    util::ScopedTimer scTimer{waitTiming_};

    if (buffers_.size() <= streams_.size()) {
        throw eckit::BadValue("Too few buffers to cover all MPI destinations", Here());
    }

    auto& buf = findAvailableBuffer();
    streams_.emplace(dest, buf);
    buf.status = BufferStatus::fillingUp;

    return streams_.at(dest);
}

void StreamPool::print(std::ostream& os) const {
    os << "StreamPool(size=" << buffers_.size() << ",status=";
    std::for_each(std::begin(buffers_), std::end(buffers_),
                  [&os](const MpiBuffer& buf) { os << static_cast<unsigned>(buf.status); });
    os << ")";
}

}
}


#include "StreamPool.h"

#include <algorithm>
#include <iomanip>

#include "eckit/exception/Exceptions.h"
#include "eckit/mpi/Comm.h"

#include "eckit/types/DateTime.h"

namespace multio {
namespace server {

namespace  {
std::vector<MpiBuffer> makeBuffers(size_t poolSize, size_t maxBufSize) {
    std::vector<MpiBuffer> bufs;
    eckit::Log::info() << " *** Allocating " << poolSize << " buffers of size "
                       << maxBufSize / 1024 / 1024 << " each" << std::endl;
    double totMem = 0.0;
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

StreamPool::StreamPool(size_t poolSize, size_t maxBufSize, const eckit::mpi::Comm& comm,
                       TransportStatistics& stats) :
    comm_{comm}, statistics_{stats}, buffers_(makeBuffers(poolSize, maxBufSize)) {}

MpiBuffer& StreamPool::buffer(size_t idx) {
    return buffers_[idx];
}

MpiOutputStream& StreamPool::getStream(const message::Message& msg) {
    auto dest = msg.destination();

    if (streams_.find(dest) == std::end(streams_)) {
        return createNewStream(dest);
    }

    //  Carry on using stream if
    //    * can fit this message into the the stream's buffer AND
    //    * a probablistic algorithm return true
    //  The algorithm returns true if buffer is less than 50% filled up, and false if 100% filled up
    //  It returns either true of false depending on whether a randomly chosen number between 0.5
    //  and 1.0 is smaller than the fill-up ratio

    auto& strm = streams_.at(dest);
    if (strm.shallFitMessage(msg.size())) {
        return strm;
    }

    sendBuffer(dest, static_cast<int>(msg.tag()));

    return replaceStream(dest);
}

MpiOutputStream& StreamPool::replaceStream(const message::Peer& dest) {
    streams_.erase(dest);
    return createNewStream(dest);
}

void StreamPool::sendBuffer(const message::Peer& dest, int msg_tag) {
    auto& strm = streams_.at(dest);

    auto sz = static_cast<size_t>(strm.bytesWritten());
    auto destId = static_cast<int>(dest.id());

    ++counter_[dest];
    struct ::timeval tstamp;
    ::gettimeofday(&tstamp, 0);
    auto mSecs = tstamp.tv_usec;

    os_ << " *** Dispatching buffer to " << dest << " -- counter: " << std::setw(4)
        << std::setfill('0') << counter_.at(dest)
        << ", timestamps: " << eckit::DateTime{static_cast<double>(tstamp.tv_sec)}.time().now()
        << ":" << std::setw(6) << std::setfill('0') << mSecs;

    eckit::AutoTiming(statistics_.timer_, statistics_.isendTiming_);

    strm.buffer().request = comm_.iSend<void>(strm.buffer().content, sz, destId, msg_tag);
    strm.buffer().status = BufferStatus::transmitting;

    ::gettimeofday(&tstamp, 0);
    mSecs = tstamp.tv_usec;
    os_ << " and " << eckit::DateTime{static_cast<double>(tstamp.tv_sec)}.time().now()
        << ":" << std::setw(6) << std::setfill('0') << mSecs << '\n';

    ++statistics_.isendCount_;
    statistics_.isendSize_ += sz;
}

MpiBuffer& StreamPool::findAvailableBuffer(std::ostream& os) {
    eckit::AutoTiming(statistics_.timer_, statistics_.waitTiming_);

    auto it = std::end(buffers_);
    while (it == std::end(buffers_)) {
        it = std::find_if(std::begin(buffers_), std::end(buffers_),
                          [](MpiBuffer& buf) { return buf.isFree(); });
    }

    os << " *** Found available buffer with idx = "
       << static_cast<size_t>(std::distance(std::begin(buffers_), it)) << std::endl;

    return *it;
}

void StreamPool::waitAll() {
    eckit::AutoTiming(statistics_.timer_, statistics_.waitTiming_);
    while (not std::all_of(std::begin(buffers_), std::end(buffers_),
                           [](MpiBuffer& buf) { return buf.isFree(); })) {}
}

MpiOutputStream& StreamPool::createNewStream(const message::Peer& dest) {
    if (buffers_.size() < streams_.size()) {
        throw eckit::BadValue("Too few buffers to cover all MPI destinations", Here());
    }

    auto& buf = findAvailableBuffer(eckit::Log::debug<LibMultio>());
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

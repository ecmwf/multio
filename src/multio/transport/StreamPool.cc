
#include "StreamPool.h"

#include <algorithm>
#include <iomanip>

#include "eckit/exception/Exceptions.h"
#include "eckit/mpi/Comm.h"
#include "eckit/types/DateTime.h"


namespace multio::transport {

namespace {
std::vector<MpiBuffer> makeBuffers(size_t poolSize, size_t maxBufSize) {
    std::vector<MpiBuffer> bufs;
    LOG_DEBUG_LIB(multio::LibMultio) << "*** Allocating " << poolSize << " buffers of size " << maxBufSize / 1024 / 1024
                                     << " each" << std::endl;
    double totMem = 0.0;
    for (auto ii = 0u; ii < poolSize; ++ii) {
        bufs.emplace_back(maxBufSize);
        totMem += maxBufSize;
    }
    totMem /= 1024 * 1024 * 1024;
    LOG_DEBUG_LIB(multio::LibMultio) << "*** Allocated a total of " << totMem << "GiB of memory for this peer"
                                     << std::endl;
    return bufs;
}
}  // namespace

MpiPeer::MpiPeer(const std::string& comm, size_t rank) : Peer{comm, rank} {}
MpiPeer::MpiPeer(Peer peer) : Peer{peer} {}

StreamPool::StreamPool(size_t poolSize, size_t maxBufSize, const eckit::mpi::Comm& comm, TransportStatistics& stats) :
    comm_{comm}, statistics_{stats}, buffers_(makeBuffers(poolSize, maxBufSize)) {}

MpiBuffer& StreamPool::buffer(size_t idx) {
    return buffers_[idx];
}

MpiOutputStream& StreamPool::getStream(const message::Message& msg) {
    // TODO
    // Why do we need to store the stream? When we start to share the pool with receiving and sending thread,
    // one of these stored streams might already be used
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

    os_ << " *** Dispatching buffer to " << dest << " -- counter: " << std::setw(4) << std::setfill('0')
        << counter_.at(dest) << ", timestamps: " << eckit::DateTime{static_cast<double>(tstamp.tv_sec)}.time().now()
        << ":" << std::setw(6) << std::setfill('0') << mSecs;

    util::ScopedTiming(statistics_.isendTiming_);

    strm.buffer().request = comm_.iSend<void>(strm.buffer().content, sz, destId, msg_tag);
    strm.buffer().status.store(BufferStatus::transmitting, std::memory_order_release);

    ::gettimeofday(&tstamp, 0);
    mSecs = tstamp.tv_usec;
    os_ << " and " << eckit::DateTime{static_cast<double>(tstamp.tv_sec)}.time().now() << ":" << std::setw(6)
        << std::setfill('0') << mSecs << '\n';

    ++statistics_.isendCount_;
    statistics_.isendSize_ += sz;
}

MpiBuffer& StreamPool::acquireAvailableBuffer(BufferStatus newStatus, std::ostream& os) {
    util::ScopedTiming(statistics_.waitTiming_);

    auto it = std::end(buffers_);

    // TODO optimize this by keeping queues of available buffers?
    // Once we start using the pool for receiving and sending, we need this mechanism -
    // or we just split up the pool
    while (it == std::end(buffers_)) {
        it = std::find_if(std::begin(buffers_), std::end(buffers_), [](MpiBuffer& buf) {
            // To a "quick" relaxed load to see if the buffer is not already used
            BufferStatus status = buf.status.load(std::memory_order_relaxed);
            switch (status) {
                // Buffer might be available
                case BufferStatus::available: {
                    BufferStatus expectedAvailableStatus = BufferStatus::available;
                    // Compare exchange to see if the status is still the same and acquire the buffer
                    return buf.status.compare_exchange_weak(expectedAvailableStatus, BufferStatus::fillingUp,
                                                            std::memory_order_acq_rel);
                }
                // Buffer is transmitting but might have finished
                case BufferStatus::transmitting: {
                    if (!buf.request.test()) {
                        // Not finished yet
                        return false;
                    }
                    BufferStatus expectedTransmittingStatus = BufferStatus::transmitting;
                    // Request finished, try to acquire
                    if (buf.status.compare_exchange_weak(expectedTransmittingStatus, BufferStatus::fillingUp,
                                                         std::memory_order_acq_rel)) {
                        // Buffer is acquired, test if the request is still finished - i.e. no other send is performed
                        // meanwhile
                        if (buf.request.test()) {
                            return true;
                        }
                        else {
                            // Something went wrong buffer seems to be transmitting ... some other thread might has
                            // acquired before we exchanged Change back to transmitting and continue --- might be free
                            // in the next round
                            buf.status.store(BufferStatus::transmitting, std::memory_order_relaxed);
                            return false;
                        }
                    }
                    else {
                        // Buffer is already used
                        return false;
                    }
                }
                default:
                    // Buffer is already used
                    return false;
            };
        });
    }


    os << " *** Found available buffer with idx = " << static_cast<size_t>(std::distance(std::begin(buffers_), it))
       << std::endl;

    return *it;
}

void StreamPool::waitAll() {
    util::ScopedTiming(statistics_.waitTiming_);
    while (not std::all_of(std::begin(buffers_), std::end(buffers_), [](MpiBuffer& buf) { return buf.isFree(); })) {}
}

MpiOutputStream& StreamPool::createNewStream(const message::Peer& dest) {
    if (buffers_.size() < streams_.size()) {
        throw eckit::BadValue("Too few buffers to cover all MPI destinations", Here());
    }

    auto& buf = acquireAvailableBuffer(BufferStatus::fillingUp, eckit::Log::debug<LibMultio>());
    streams_.emplace(dest, buf);

    return streams_.at(dest);
}

void StreamPool::print(std::ostream& os) const {
    os << "StreamPool(size=" << buffers_.size() << ",status=";
    std::for_each(std::begin(buffers_), std::end(buffers_), [&os](const MpiBuffer& buf) {
        os << static_cast<unsigned>(buf.status.load(std::memory_order_relaxed));
    });
    os << ")";
}

}  // namespace multio::transport

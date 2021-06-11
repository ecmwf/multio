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

std::string filename(size_t id) {
    std::ostringstream os;
    os << "mpi-transport-" << std::setw(4) << std::setfill('0') << id;
    os << ".log";
    return os.str();
}

std::string system_call(const char* cmd) {
    std::array<char, 128> buffer;
    std::string result;
    std::unique_ptr<FILE, decltype(&pclose)> pipe(popen(cmd, "r"), pclose);
    if (!pipe) {
        throw std::runtime_error("popen() failed!");
    }
    while (fgets(buffer.data(), buffer.size(), pipe.get()) != nullptr) {
        result += buffer.data();
    }
    return result;
}

const size_t defaultBufferSize = 64 * 1024 * 1024;
const size_t defaultPoolSize = 128;

}  // namespace

MpiTransport::MpiTransport(const eckit::Configuration& cfg) :
    Transport(cfg),
    local_{cfg.getString("group"), eckit::mpi::comm(cfg.getString("group").c_str()).rank()},
    pool_{eckit::Resource<size_t>("multioMpiPoolSize;$MULTIO_MPI_POOL_SIZE", defaultPoolSize),
          eckit::Resource<size_t>("multioMpiBufferSize;$MULTIO_MPI_BUFFER_SIZE", defaultBufferSize),
          comm()},
    log_{filename(local_.id())} {}

MpiTransport::~MpiTransport() {
    // TODO: check why eckit::Log::info() crashes here for the clients
    const std::size_t scale = 1024*1024;
    std::ostringstream os;
    os << " ******* " << *this << "\n";
    pool_.timings(os);
    os << "\n         -- Send time (block):   " << sendTiming_
       << "s\n         -- Send and serialise:  " << totSendTiming_
       << "s\n         -- Receiving data:      " << bytesReceived_ / scale
       << " MiB\n         -- Probing for data:    " << probeTiming_
       << "s\n         -- Receive timing:      " << receiveTiming_
       << "s\n         -- Push-queue timing:   " << pushToQueueTiming_
       << "s\n         -- Deserialise data:    " << decodeTiming_
       << "s\n         -- Returning data:      " << returnTiming_
       << "s\n         -- Total for return:    " << totReturnTiming_ << "s" << std::endl;

    log_ << os.str();
}

void MpiTransport::openConnections() {
    for (auto& server : createServerPeers()) {
        Message msg{Message::Header{Message::Tag::Open, local_, *server}};
        bufferedSend(msg);
    }
}

void MpiTransport::closeConnections() {
    for (auto& server : createServerPeers()) {
        Message msg{Message::Header{Message::Tag::Close, local_, *server}};
        bufferedSend(msg);
        pool_.sendBuffer(msg.destination(), static_cast<int>(msg.tag()));
    }
    pool_.waitAll();
}

Message MpiTransport::receive() {
    util::ScopedTimer scTimer{totReturnTiming_};

    do {
        while (not msgPack_.empty()) {
            util::ScopedTimer retTimer{returnTiming_};
            auto msg = msgPack_.front();
            msgPack_.pop();
            return msg;
        }

        if (not streamQueue_.empty()) {
            util::ScopedTimer decTimer{decodeTiming_};
            auto& strm = streamQueue_.front();
            while (strm.position() < strm.size()) {
                auto msg = decodeMessage(strm);
                msgPack_.push(msg);
            }
            std::lock_guard<std::mutex> lock{mutex_};
            strm.buffer().status = BufferStatus::available;
            streamQueue_.pop();
        }
    } while (true);
}

void MpiTransport::send(const Message& msg) {
    util::ScopedTimer tscTimer{totSendTiming_};

    auto msg_tag = static_cast<int>(msg.tag());

    // TODO: find available buffer instead
    // Add 4K for header/footer etc. Should be plenty
    eckit::Buffer buffer{eckit::round(msg.size(), 8) + 4096};

    eckit::ResizableMemoryStream stream{buffer};

    msg.encode(stream);

    util::ScopedTimer scTimer{sendTiming_};

    auto sz = static_cast<size_t>(stream.bytesWritten());
    auto dest = static_cast<int>(msg.destination().id());
    eckit::mpi::comm(local_.group().c_str()).send<void>(buffer, sz, dest, msg_tag);
}

void MpiTransport::bufferedSend(const Message& msg) {
    util::ScopedTimer scTimer{totSendTiming_};

    msg.encode(pool_.getStream(msg));
}

void MpiTransport::print(std::ostream& os) const {
    os << "MpiTransport(" << local_ << ")";
}

Peer MpiTransport::localPeer() const {
    return local_;
}

void MpiTransport::listen() {
    auto status = probe();
    if(status.error()) {
        return;
    }
    auto& buf = pool_.findAvailableBuffer();
    buf.status = BufferStatus::fillingUp;
    auto sz = blockingReceive(status, buf);
    util::ScopedTimer scTimer{pushToQueueTiming_};
    bytesReceived_ += sz;
    std::lock_guard<std::mutex> lock{mutex_};
//    log_ << " *** " << localPeer() << ": current pool = " << pool_ << std::endl;
    streamQueue_.emplace(buf, sz);
}

PeerList MpiTransport::createServerPeers() {
    PeerList serverPeers;

    std::string group = config_.getString("group");
    // This is dangerous as it requires having the same logic as in NEMO or IFS
    // This needs to come from teh configuration or perhpas you want to create an intercommunicator
    auto comm_size = config_.getUnsigned("clientCount") + config_.getUnsigned("serverCount");
    auto rank = config_.getUnsigned("clientCount");
    while (rank != comm_size) {
        serverPeers.emplace_back(new MpiPeer{group, rank++});
    }

    return serverPeers;
}

const eckit::mpi::Comm& MpiTransport::comm() const {
    return eckit::mpi::comm(local_.group().c_str());
}

eckit::mpi::Status MpiTransport::probe() {
    util::ScopedTimer scTimer{probeTiming_};
    auto status = comm().iProbe(comm().anySource(), comm().anyTag());

    return status;
}

size_t MpiTransport::blockingReceive(eckit::mpi::Status& status, MpiBuffer& buffer) {
    auto sz = comm().getCount<void>(status);
    ASSERT(sz < buffer.content.size());

    util::ScopedTimer scTimer{receiveTiming_};
    comm().receive<void>(buffer.content, sz, status.source(), status.tag());

    return sz;
}

static TransportBuilder<MpiTransport> MpiTransportBuilder("mpi");

}  // namespace server
}  // namespace multio

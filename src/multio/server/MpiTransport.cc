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
    os << "\n         -- Total for send:      " << totSendTiming_
       << "s\n         -- Probing for data:    " << probeTiming_
       << "s\n         -- Receiving data:      " << bytesReceived_ / scale << " MiB, "
       << receiveTiming_
       << "s\n         -- Deserialising data:  " << decodeTiming_
       << "s\n         -- Returning data:      " << returnTiming_
       << "s\n         -- Total for receive:   " << totReceiveTiming_ << "s"
       << std::endl;

    log_ << os.str();
}

Message MpiTransport::receive() {
    util::ScopedTimer scTimer{totReceiveTiming_};

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
    util::ScopedTimer scTimer{totSendTiming_};

    msg.encode(pool_.getStream(msg));

    if (msg.tag() == Message::Tag::Close) {  // Send it now
        pool_.send(msg);
    }
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
    std::lock_guard<std::mutex> lock{mutex_};
    streamQueue_.emplace(buf, sz);
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

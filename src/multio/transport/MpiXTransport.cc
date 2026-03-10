#include "MpiXTransport.h"

#include <algorithm>
#include <fstream>

#include "eckit/maths/Functions.h"
#include "eckit/mpi/Comm.h"
#include "eckit/runtime/Main.h"
#include "eckit/serialisation/MemoryStream.h"
#include "eckit/utils/Translator.h"

#include "multio/util/Environment.h"

namespace multio::transport {

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

    return Message{Message::Header{static_cast<Message::Tag>(t),
                                   MpiPeer{src_grp, src_id},
                                   MpiPeer{dest_grp, dest_id},
                                   std::move(fieldId)},
                   std::move(buffer)};
}

const size_t defaultBufferSize = 64 * 1024 * 1024;

std::string makeParentName(const std::string& base) {
    return base + "-parent";
}

std::string makeClientName(const std::string& base) {
    return base + "-client";
}

std::string makeServerName(const std::string& base) {
    return base + "-server";
}


MpiPeerSetup setupMPI_(const ComponentConfiguration& compConf) {
    const auto& multioConf = compConf.multioConfig();

    if (multioConf.getCommunicatorCount() == 0) {
        throw TransportException("MpiXTransport: no communicator registered in MultioConfiguration", Here());
    }

    // For now: use communicator 0 only.
    const std::string baseName = multioConf.getCommunicatorNameById(0);

    const std::string parentName = makeParentName(baseName);
    const std::string clientName = makeClientName(baseName);
    const std::string serverName = makeServerName(baseName);

    if (!eckit::mpi::hasComm(parentName.c_str())) {
        std::ostringstream oss;
        oss << "MpiXTransport: parent communicator \"" << parentName
            << "\" is not registered in eckit::mpi";
        throw TransportException(oss.str(), Here());
    }

    if (!eckit::mpi::hasComm(clientName.c_str())) {
        std::ostringstream oss;
        oss << "MpiXTransport: client communicator \"" << clientName
            << "\" is not registered in eckit::mpi";
        throw TransportException(oss.str(), Here());
    }

    if (!eckit::mpi::hasComm(serverName.c_str())) {
        std::ostringstream oss;
        oss << "MpiXTransport: server communicator \"" << serverName
            << "\" is not registered in eckit::mpi";
        throw TransportException(oss.str(), Here());
    }

    auto& parentComm = eckit::mpi::comm(parentName.c_str());
    auto& clientComm = eckit::mpi::comm(clientName.c_str());
    auto& serverComm = eckit::mpi::comm(serverName.c_str());

    eckit::mpi::Group parentGroup = parentComm.group();
    eckit::mpi::Group clientGroup = clientComm.group();
    eckit::mpi::Group serverGroup = serverComm.group();

    MpiPeer local{parentName, static_cast<size_t>(parentComm.rank())};

    return MpiPeerSetup(std::move(local), std::move(parentGroup), std::move(clientGroup), std::move(serverGroup));
}

}  // namespace

MpiXTransport::MpiXTransport(const ComponentConfiguration& compConf, MpiPeerSetup&& peerSetup) :
    Transport(compConf),
    local_{std::move(std::get<0>(peerSetup))},
    parentGroup_{std::move(std::get<1>(peerSetup))},
    clientGroup_{std::move(std::get<2>(peerSetup))},
    serverGroup_{std::move(std::get<3>(peerSetup))},
    pool_{getMpiPoolSize(compConf), getMpiBufferSize(compConf), comm(), statistics_},
    streamQueue_{1024} {

    auto poolSize = getMpiPoolSize(compConf);
    if (compConf.multioConfig().localPeerTag() == config::LocalPeerTag::Client) {
        if (poolSize < serverGroup_.size()) {
            std::ostringstream os;
            os << "Pool size of the client must be at least equal to the size of the server MPI communicator. ";
            os << "Consider unsetting or increasing the values of the following environment variables:\n";
            os << "    MULTIO_CLIENT_MPI_POOL_SIZE\n";
            os << "    MULTIO_MPI_POOL_SIZE\n";
            os << "Currently client MPI pool size is " << poolSize
               << " size of server communicator is " << serverGroup_.size();
            throw eckit::UserError(os.str(), Here());
        }
    }
    else {
        if (poolSize < 1) {
            std::ostringstream os;
            os << "Pool size of the server must be at least 1. ";
            os << "Consider unsetting or increasing the values of the following environment variables:\n";
            os << "    MULTIO_SERVER_MPI_POOL_SIZE\n";
            os << "    MULTIO_MPI_POOL_SIZE\n";
            os << "Currently server MPI pool size is " << poolSize;
            throw eckit::UserError(os.str(), Here());
        }
    }
}

MpiXTransport::MpiXTransport(const ComponentConfiguration& compConf) :
    MpiXTransport(compConf, setupMPI_(compConf)) {}

MpiXTransport::~MpiXTransport() = default;

void MpiXTransport::openConnections() {
    for (auto& server : serverPeers()) {
        Message msg{Message::Header{Message::Tag::Open, local_, *server}};
        send(msg);
    }
}

void MpiXTransport::closeConnections() {
    for (auto& server : serverPeers()) {
        Message msg{Message::Header{Message::Tag::Close, local_, *server}};
        bufferedSend(msg);
        pool_.sendBuffer(msg.destination());
    }
    pool_.waitAll();
}

void MpiXTransport::synchronize() {
    if (compConf_.multioConfig().localPeerTag() == config::LocalPeerTag::Client) {
        for (auto& server : serverPeers()) {
            Message msg{Message::Header{Message::Tag::Synchronization, local_, *server}};
            bufferedSend(msg);
            pool_.sendBuffer(msg.destination());
        }
        pool_.waitAll();
    }

    comm().barrier();
}

Message MpiXTransport::receive() {
    util::ScopedTiming timing{statistics_.totReturnTiming_};

    while (true) {
        if (!msgPack_.empty()) {
            util::ScopedTiming retTiming{statistics_.returnTiming_};
            auto msg = std::move(msgPack_.front());
            msgPack_.pop();
            return msg;
        }

        ReceivedBuffer streamArgs;
        streamQueue_.pop(streamArgs);
        if (streamArgs.buffer) {
            eckit::ResizableMemoryStream strm{streamArgs.buffer->content};
            while (strm.position() < streamArgs.size) {
                util::ScopedTiming decodeTiming{statistics_.decodeTiming_};
                auto msg = decodeMessage(strm);
                msgPack_.push(std::move(msg));
            }
            streamArgs.buffer->status.store(BufferStatus::available, std::memory_order_release);
        }
    }
}

void MpiXTransport::abort(std::exception_ptr ptr) {
    streamQueue_.interrupt(ptr);
    comm().abort();
}

void MpiXTransport::send(const Message& msg) {
    std::lock_guard<std::mutex> lock{mutex_};

    eckit::Buffer buffer{eckit::round(msg.size(), 8) + 4096};
    eckit::ResizableMemoryStream stream{buffer};

    encodeMessage(stream, msg);

    util::ScopedTiming timing{statistics_.sendTiming_};

    auto sz = static_cast<size_t>(stream.bytesWritten());
    auto dest = static_cast<int>(msg.destination().id());

    comm().send<void>(buffer, sz, dest, 0);

    ++statistics_.sendCount_;
    statistics_.sendSize_ += sz;
}

void MpiXTransport::bufferedSend(const Message& msg) {
    std::lock_guard<std::mutex> lock{mutex_};
    encodeMessage(pool_.getStream(msg), msg);
}

void MpiXTransport::createPeers() const {
    auto parentSize = comm().size();
    std::vector<int> parentRanks(parentSize);

    for (int r = 0; r < parentSize; ++r) {
        parentRanks[r] = r;
    }

    auto clientRankMap = parentGroup_.translate_ranks(parentRanks, clientGroup_);
    auto serverRankMap = parentGroup_.translate_ranks(parentRanks, serverGroup_);

    for (auto& it : clientRankMap) {
        clientPeers_.emplace_back(std::make_unique<MpiPeer>(local_.group(), static_cast<unsigned long>(it.first)));
    }
    for (auto& it : serverRankMap) {
        serverPeers_.emplace_back(std::make_unique<MpiPeer>(local_.group(), static_cast<unsigned long>(it.first)));
    }
}

void MpiXTransport::print(std::ostream& os) const {
    os << "MpiXTransport(" << local_ << ")";
}

const Peer& MpiXTransport::localPeer() const {
    return local_;
}

void MpiXTransport::listen() {
    auto status = probe();
    if (status.error()) {
        return;
    }

    auto& buf = pool_.acquireAvailableBuffer(BufferStatus::fillingUp);
    auto sz = blockingReceive(status, buf);
    util::ScopedTiming timing{statistics_.pushToQueueTiming_};
    streamQueue_.push(ReceivedBuffer{&buf, sz});
}

PeerList MpiXTransport::createServerPeers() const {
    PeerList serverPeers;

    auto parentSize = comm().size();
    std::vector<int> parentRanks(parentSize);

    for (int r = 0; r < parentSize; ++r) {
        parentRanks[r] = r;
    }

    auto serverRankMap = parentGroup_.translate_ranks(parentRanks, serverGroup_);

    for (auto& it : serverRankMap) {
        serverPeers.emplace_back(std::make_unique<MpiPeer>(local_.group(), static_cast<unsigned long>(it.first)));
    }

    return serverPeers;
}

const eckit::mpi::Comm& MpiXTransport::comm() const {
    return eckit::mpi::comm(local_.group().c_str());
}

eckit::mpi::Status MpiXTransport::probe() {
    util::ScopedTiming timing{statistics_.probeTiming_};
    return comm().iProbe(comm().anySource(), comm().anyTag());
}

size_t MpiXTransport::blockingReceive(eckit::mpi::Status& status, MpiBuffer& buffer) {
    auto sz = comm().getCount<void>(status);
    ASSERT(sz < buffer.content.size());

    util::ScopedTiming timing{statistics_.receiveTiming_};
    comm().receive<void>(buffer.content, sz, status.source(), status.tag());

    ++statistics_.receiveCount_;
    statistics_.receiveSize_ += sz;

    return sz;
}

void MpiXTransport::encodeMessage(eckit::Stream& strm, const Message& msg) {
    util::ScopedTiming timing{statistics_.encodeTiming_};
    msg.encode(strm);
}

size_t MpiXTransport::getMpiPoolSize(const ComponentConfiguration& compConf) {
    switch (compConf.multioConfig().localPeerTag()) {
        case config::LocalPeerTag::Server: {
            auto pServ = util::getEnv("MULTIO_SERVER_MPI_POOL_SIZE");
            if (pServ) {
                return eckit::translate<size_t>(std::string{*pServ});
            }
            auto pMul = util::getEnv("MULTIO_MPI_POOL_SIZE");
            if (pMul) {
                return eckit::translate<size_t>(std::string{*pMul});
            }
            return std::max<size_t>(1, std::min<size_t>(clientGroup_.size() / 2, 4));
        }

        case config::LocalPeerTag::Client: {
            auto pClient = util::getEnv("MULTIO_CLIENT_MPI_POOL_SIZE");
            if (pClient) {
                return eckit::translate<size_t>(std::string{*pClient});
            }
            auto pMul = util::getEnv("MULTIO_MPI_POOL_SIZE");
            if (pMul) {
                return eckit::translate<size_t>(std::string{*pMul});
            }
            return 2 * serverGroup_.size();
        }

        default:
            throw TransportException("MpiXTransport::getMpiPoolSize: invalid localPeerTag", Here());
    }
}

size_t MpiXTransport::getMpiBufferSize(const ComponentConfiguration& compConf) {
    switch (compConf.multioConfig().localPeerTag()) {
        case config::LocalPeerTag::Server: {
            auto pServ = util::getEnv("MULTIO_SERVER_MPI_BUFFER_SIZE");
            if (pServ) {
                return eckit::translate<size_t>(std::string{*pServ});
            }
            auto pMul = util::getEnv("MULTIO_MPI_BUFFER_SIZE");
            if (pMul) {
                return eckit::translate<size_t>(std::string{*pMul});
            }
            return defaultBufferSize;
        }

        case config::LocalPeerTag::Client: {
            auto pClient = util::getEnv("MULTIO_CLIENT_MPI_BUFFER_SIZE");
            if (pClient) {
                return eckit::translate<size_t>(std::string{*pClient});
            }
            auto pMul = util::getEnv("MULTIO_MPI_BUFFER_SIZE");
            if (pMul) {
                return eckit::translate<size_t>(std::string{*pMul});
            }
            return defaultBufferSize;
        }

        default:
            throw TransportException("MpiXTransport::getMpiBufferSize: invalid localPeerTag", Here());
    }
}

static TransportBuilder<MpiXTransport> MpiXTransportBuilder("mpix");

}  // namespace multio::transport
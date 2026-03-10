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
#include <fstream>

#include "eckit/maths/Functions.h"
#include "eckit/mpi/Comm.h"
#include "eckit/mpi/Group.h"
#include "eckit/runtime/Main.h"
#include "eckit/serialisation/MemoryStream.h"
#include "eckit/utils/Translator.h"

#include "multio/transport/MpiCommSetup.h"
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

    return Message{Message::Header{static_cast<Message::Tag>(t), MpiPeer{src_grp, src_id}, MpiPeer{dest_grp, dest_id},
                                   std::move(fieldId)},
                   std::move(buffer)};
}

const size_t defaultBufferSize = 64 * 1024 * 1024;

MpiPeerSetup setupMPI_(const ComponentConfiguration& compConf) {
    const std::string& groupName = compConf.parsedConfig().getString("group", "multio");
    mpi::CommSetupOptions groupOptions;
    groupOptions.defaultType = std::optional<mpi::CommSetupType>(mpi::CommSetupType::Passed);

    eckit::mpi::Comm& groupComm
        = mpi::getComm(compConf, groupName, std::optional<mpi::CommSetupOptions>{std::move(groupOptions)});

    std::string clientCommName;
    std::string serverCommName;

    switch (compConf.multioConfig().localPeerTag()) {
        case config::LocalPeerTag::Client: {
            mpi::CommSetupOptions options;
            options.defaultType = std::optional<mpi::CommSetupType>(mpi::CommSetupType::Split);
            options.parentCommName = std::optional<std::string>(groupName);

            const auto& mpiInitInfo = compConf.multioConfig().getMPIInitInfo();

            clientCommName = compConf.parsedConfig().has("client-group")
                               ? compConf.parsedConfig().getString("client-group")
                               : groupName + "-clients";

            auto& clientComm
                = mpi::getComm(compConf, clientCommName, std::optional<mpi::CommSetupOptions>{std::move(options)});

            if (mpiInitInfo && mpiInitInfo->returnClientComm != nullptr) {
                *(mpiInitInfo->returnClientComm) = clientComm.communicator();
                mpiInitInfo->returnClientComm = nullptr;
            }

            // Server comm name — clients don't create it, but need to know the name
            serverCommName = compConf.parsedConfig().has("server-group")
                               ? compConf.parsedConfig().getString("server-group")
                               : groupName + "-servers";
        } break;
        case config::LocalPeerTag::Server: {
            mpi::CommSetupOptions options;
            options.defaultType = std::optional<mpi::CommSetupType>(mpi::CommSetupType::Split);
            options.parentCommName = std::optional<std::string>(groupName);

            const auto& mpiInitInfo = compConf.multioConfig().getMPIInitInfo();

            serverCommName = compConf.parsedConfig().has("server-group")
                               ? compConf.parsedConfig().getString("server-group")
                               : groupName + "-servers";

            auto& serverComm
                = mpi::getComm(compConf, serverCommName, std::optional<mpi::CommSetupOptions>{std::move(options)});

            if (mpiInitInfo && mpiInitInfo->returnServerComm != nullptr) {
                *(mpiInitInfo->returnServerComm) = serverComm.communicator();
                mpiInitInfo->returnServerComm = nullptr;
            }

            // Client comm name — servers don't create it, but need to know the name
            clientCommName = compConf.parsedConfig().has("client-group")
                               ? compConf.parsedConfig().getString("client-group")
                               : groupName + "-clients";
        } break;
    }
    return MpiPeerSetup{MpiPeer{groupName, groupComm.rank()}, groupName, clientCommName, serverCommName};
}

}  // namespace


MpiTransport::MpiTransport(const ComponentConfiguration& compConf, MpiPeerSetup&& peerSetup) :
    Transport(compConf),
    local_{std::move(peerSetup.localPeer)},
    parentCommName_{std::move(peerSetup.parentCommName)},
    clientCommName_{std::move(peerSetup.clientCommName)},
    serverCommName_{std::move(peerSetup.serverCommName)},
    pool_{getMpiPoolSize(compConf), getMpiBufferSize(compConf), comm(), statistics_},
    streamQueue_{1024} {
    // Check if the MpiPoolSize is correct:
    //   If LocalPeerTag == Client -> MpiPoolSize >= size of server communicator
    //   If LocalPeerTag == Server -> MpiPoolSize >= 1
    auto poolSize = getMpiPoolSize(compConf);
    if (compConf.multioConfig().localPeerTag() == config::LocalPeerTag::Client) {
        auto serverGroup = comm().group().difference(clientComm().group());
        if (poolSize < serverGroup.size()) {
            std::ostringstream os;
            os << "Pool size of the client must be at least equal to the size of the server MPI communicator. ";
            os << "Consider unsetting or increasing the values of the following environment variables:\n";
            os << "    MULTIO_CLIENT_MPI_POOL_SIZE\n";
            os << "    MULTIO_MPI_POOL_SIZE\n";
            os << "Currently client MPI pool size is " << poolSize << " size of server communicator is "
               << serverGroup.size();
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

MpiTransport::MpiTransport(const ComponentConfiguration& compConf) : MpiTransport(compConf, setupMPI_(compConf)) {}

MpiTransport::~MpiTransport() = default;

void MpiTransport::openConnections() {
    for (auto& server : serverPeers()) {
        Message msg{Message::Header{Message::Tag::Open, local_, *server}};
        send(msg);
    }
}

void MpiTransport::closeConnections() {
    for (auto& server : serverPeers()) {
        Message msg{Message::Header{Message::Tag::Close, local_, *server}};
        bufferedSend(msg);
        pool_.sendBuffer(msg.destination());
    }
    pool_.waitAll();
}

void MpiTransport::synchronize() {
    if (compConf_.multioConfig().localPeerTag() == config::LocalPeerTag::Client) {
        for (auto& server : serverPeers()) {
            Message msg{Message::Header{Message::Tag::Synchronization, local_, *server}};
            bufferedSend(msg);
            pool_.sendBuffer(msg.destination());
        }
    }
    else {
        serverComm().barrier();
    }
}

Message MpiTransport::receive() {
    util::ScopedTiming timing{statistics_.totReturnTiming_};
    /**
     * Read raw messages from streamQueue_ (filled by listen() in other thread)
     *
     * Decode and add to msgPack_ (msgQueue)
     *
     * Return single messages until msgPack_ is empty and start over
     */

    while (true) {
        if (not msgPack_.empty()) {
            util::ScopedTiming retTiming{statistics_.returnTiming_};
            //! TODO For switch to MPMC queue: combine front() and pop()
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

void MpiTransport::abort(std::exception_ptr ptr) {
    streamQueue_.interrupt(ptr);
    comm().abort();
}

void MpiTransport::send(const Message& msg) {
    std::lock_guard<std::mutex> lock{mutex_};

    // TODO: find available buffer instead
    // Add 4K for header/footer etc. Should be plenty
    eckit::Buffer buffer{eckit::round(msg.size(), 8) + 4096};

    eckit::ResizableMemoryStream stream{buffer};

    encodeMessage(stream, msg);

    util::ScopedTiming timing{statistics_.sendTiming_};

    auto sz = static_cast<size_t>(stream.bytesWritten());
    auto dest = static_cast<int>(msg.destination().id());

    // eckit::Log::info() << " *** MpiTransport::send from " << local_.group() << " " << local_.id
    // << std::endl;
    comm().send<void>(buffer, sz, dest, 0);

    ++statistics_.sendCount_;
    statistics_.sendSize_ += sz;
}

void MpiTransport::bufferedSend(const Message& msg) {
    std::lock_guard<std::mutex> lock{mutex_};
    encodeMessage(pool_.getStream(msg), msg);
}

void MpiTransport::createPeers() const {
    auto parentGroup = comm().group();

    eckit::mpi::Group clientGroup;
    eckit::mpi::Group serverGroup;

    if (compConf_.multioConfig().localPeerTag() == config::LocalPeerTag::Client) {
        clientGroup = clientComm().group();
        serverGroup = parentGroup.difference(clientGroup);
    }
    else {
        serverGroup = serverComm().group();
        clientGroup = parentGroup.difference(serverGroup);
    }

    auto parentSize = comm().size();
    std::vector<int> parentRanks(parentSize);
    for (int r = 0; r < parentSize; ++r) {
        parentRanks[r] = r;
    }

    auto clientRankMap = parentGroup.translate_ranks(parentRanks, clientGroup);
    auto serverRankMap = parentGroup.translate_ranks(parentRanks, serverGroup);

    for (auto& it : clientRankMap) {
        clientPeers_.emplace_back(std::make_unique<MpiPeer>(local_.group(), (unsigned long)it.first));
    }
    for (auto& it : serverRankMap) {
        serverPeers_.emplace_back(std::make_unique<MpiPeer>(local_.group(), (unsigned long)it.first));
    }

    eckit::Log::info() << " *** MpiTransport::createPeers clientCount: " << clientPeers_.size()
                       << ", serverCount: " << serverPeers_.size() << ", commSize: " << parentSize << std::endl;
}

void MpiTransport::print(std::ostream& os) const {
    os << "MpiTransport(" << local_ << ")";
}

const Peer& MpiTransport::localPeer() const {
    return local_;
}

void MpiTransport::listen() {
    auto status = probe();
    if (status.error()) {
        return;
    }
    // TODO status contains information on required message size - use that to retrieve a sufficient
    // large buffer?
    auto& buf = pool_.acquireAvailableBuffer(BufferStatus::fillingUp);
    auto sz = blockingReceive(status, buf);
    util::ScopedTiming timing{statistics_.pushToQueueTiming_};
    streamQueue_.push(ReceivedBuffer{&buf, sz});
}

PeerList MpiTransport::createServerPeers() const {
    if (peersMissing()) {
        createPeers();
    }
    // Return a copy of the already-populated serverPeers_
    PeerList result;
    for (const auto& peer : serverPeers_) {
        result.emplace_back(std::make_unique<MpiPeer>(static_cast<const MpiPeer&>(*peer)));
    }
    return result;
}

const eckit::mpi::Comm& MpiTransport::comm() const {
    return eckit::mpi::comm(parentCommName_.c_str());
}

const eckit::mpi::Comm& MpiTransport::clientComm() const {
    return eckit::mpi::comm(clientCommName_.c_str());
}

const eckit::mpi::Comm& MpiTransport::serverComm() const {
    return eckit::mpi::comm(serverCommName_.c_str());
}

eckit::mpi::Status MpiTransport::probe() {
    util::ScopedTiming timing{statistics_.probeTiming_};
    auto status = comm().iProbe(comm().anySource(), comm().anyTag());

    return status;
}

size_t MpiTransport::blockingReceive(eckit::mpi::Status& status, MpiBuffer& buffer) {
    auto sz = comm().getCount<void>(status);
    ASSERT(sz < buffer.content.size());

    util::ScopedTiming timing{statistics_.receiveTiming_};
    comm().receive<void>(buffer.content, sz, status.source(), status.tag());

    ++statistics_.receiveCount_;
    statistics_.receiveSize_ += sz;

    return sz;
}

void MpiTransport::encodeMessage(eckit::Stream& strm, const Message& msg) {
    util::ScopedTiming timing{statistics_.encodeTiming_};

    msg.encode(strm);
}

size_t MpiTransport::getMpiPoolSize(const ComponentConfiguration& compConf) {
    switch (compConf.multioConfig().localPeerTag()) {
        case config::LocalPeerTag::Server: {
            auto pServ = util::getEnv("MULTIO_SERVER_MPI_POOL_SIZE");
            if (pServ) {
                return eckit::translate<size_t>(std::string{*pServ});
            };
            auto pMul = util::getEnv("MULTIO_MPI_POOL_SIZE");
            if (pMul) {
                return eckit::translate<size_t>(std::string{*pMul});
            };
            auto clientGroup = comm().group().difference(serverComm().group());  // Default between 1 and 4 (inclusive)
            return std::max<size_t>(1, std::min<size_t>(clientGroup.size() / 2, 4));
        }

        case config::LocalPeerTag::Client: {
            auto pClient = util::getEnv("MULTIO_CLIENT_MPI_POOL_SIZE");
            if (pClient) {
                return eckit::translate<size_t>(std::string{*pClient});
            };
            auto pMul = util::getEnv("MULTIO_MPI_POOL_SIZE");
            if (pMul) {
                return eckit::translate<size_t>(std::string{*pMul});
            };
            auto serverGroup = comm().group().difference(clientComm().group());
            return 2 * serverGroup.size();
        }

        default:
            std::ostringstream oss;
            oss << "getMpiPoolSize: localPeerTag is neither Server ("
                << static_cast<unsigned>(config::LocalPeerTag::Server) << ") nor Client ("
                << static_cast<unsigned>(config::LocalPeerTag::Client)
                << "). Value: " << static_cast<unsigned>(compConf.multioConfig().localPeerTag()) << std::endl;
            throw TransportException("", Here());
    }
}

size_t MpiTransport::getMpiBufferSize(const ComponentConfiguration& compConf) {
    switch (compConf.multioConfig().localPeerTag()) {
        case config::LocalPeerTag::Server: {
            auto pServ = util::getEnv("MULTIO_SERVER_MPI_BUFFER_SIZE");
            if (pServ) {
                return eckit::translate<size_t>(std::string{*pServ});
            };
            auto pMul = util::getEnv("MULTIO_MPI_BUFFER_SIZE");
            if (pMul) {
                return eckit::translate<size_t>(std::string{*pMul});
            };
            return defaultBufferSize;
        }

        case config::LocalPeerTag::Client: {
            auto pClient = util::getEnv("MULTIO_CLIENT_MPI_BUFFER_SIZE");
            if (pClient) {
                return eckit::translate<size_t>(std::string{*pClient});
            };
            auto pMul = util::getEnv("MULTIO_MPI_BUFFER_SIZE");
            if (pMul) {
                return eckit::translate<size_t>(std::string{*pMul});
            };
            return defaultBufferSize;
        }

        default:
            std::ostringstream oss;
            oss << "getMpiBufferSize: localPeerTag is neither Server ("
                << static_cast<unsigned>(config::LocalPeerTag::Server) << ") nor Client ("
                << static_cast<unsigned>(config::LocalPeerTag::Client)
                << "). Value: " << static_cast<unsigned>(compConf.multioConfig().localPeerTag()) << std::endl;
            throw TransportException("", Here());
    }
}

static TransportBuilder<MpiTransport> MpiTransportBuilder("mpi");

}  // namespace multio::transport

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
#include "eckit/utils/Translator.h"
#include "eckit/runtime/Main.h"
#include "eckit/serialisation/MemoryStream.h"

#include "multio/transport/MpiCommSetup.h"
#include "multio/util/ScopedTimer.h"
#include "multio/util/Environment.h"
#include "multio/util/logfile_name.h"

namespace multio {
namespace transport {

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
const size_t defaultPoolSize = 128;

MpiPeerSetup setupMPI_(const ConfigurationContext& confCtx) {
    const std::string& groupName = confCtx.config().getString("group", "multio");
    mpi::CommSetupOptions groupOptions;
    groupOptions.defaultType = eckit::Optional<mpi::CommSetupType>(mpi::CommSetupType::Passed);

    eckit::mpi::Comm& groupComm
        = mpi::getComm(confCtx, groupName, eckit::Optional<mpi::CommSetupOptions>{std::move(groupOptions)});

    eckit::mpi::Group parentGroup = groupComm.group();
    eckit::mpi::Group clientGroup;
    eckit::mpi::Group serverGroup;

    switch (confCtx.localPeerTag()) {
        case util::LocalPeerTag::Client: {
            mpi::CommSetupOptions options;
            // Add default in case of missing configuration
            options.defaultType = eckit::Optional<mpi::CommSetupType>(mpi::CommSetupType::Split);
            options.parentCommName = eckit::Optional<std::string>(groupName);

            const auto& mpiInitInfo = confCtx.getMPIInitInfo();
            options.alias = mpiInitInfo ? mpiInitInfo().clientId : eckit::Optional<std::string>{};

            std::string subGroupName
                = confCtx.config().has("client-group") ? confCtx.config().getString("client-group") : ([&]() {
                      std::ostringstream oss;
                      oss << groupName << "-"
                          << "clients";
                      return oss.str();
                  })();
            // eckit::Log::info() << " *** MpiTransport::setupMPI_ client " << subGroupName << "
            // alias: " << (options.alias? options.alias().c_str() : "none") << std::endl;

            // Setup client group
            auto& clientComm
                = mpi::getComm(confCtx, subGroupName, eckit::Optional<mpi::CommSetupOptions>{std::move(options)});
            // eckit::Log::info() << " *** MpiTransport::setupMPI_ created clientComm... " << std::endl;

            if (mpiInitInfo && mpiInitInfo().returnClientComm != nullptr) {
                *(mpiInitInfo().returnClientComm) = clientComm.communicator();
                mpiInitInfo().returnClientComm
                    = nullptr;  // Set to null to prevent setting the pointer at a later time when it may be invalid
            }

            clientGroup = clientComm.group();
            serverGroup = parentGroup.difference(clientGroup);
        } break;
        case util::LocalPeerTag::Server: {
            mpi::CommSetupOptions options;
            // Add default in case of missing configuration
            options.defaultType = eckit::Optional<mpi::CommSetupType>(mpi::CommSetupType::Split);
            options.parentCommName = eckit::Optional<std::string>(groupName);

            const auto& mpiInitInfo = confCtx.getMPIInitInfo();

            std::string subGroupName
                = confCtx.config().has("server-group") ? confCtx.config().getString("server-group") : ([&]() {
                      std::ostringstream oss;
                      oss << groupName << "-"
                          << "servers";
                      return oss.str();
                  })();
            // eckit::Log::info() << " *** MpiTransport::setupMPI_ server " << subGroupName <<
            // std::endl;

            // Setup client group
            auto& serverComm
                = mpi::getComm(confCtx, subGroupName, eckit::Optional<mpi::CommSetupOptions>{std::move(options)});

            if (mpiInitInfo && mpiInitInfo().returnServerComm != nullptr) {
                *(mpiInitInfo().returnServerComm) = serverComm.communicator();
                mpiInitInfo().returnServerComm
                    = nullptr;  // Set to null to prevent setting the pointer at a later time when it may be invalid
            }

            serverGroup = serverComm.group();
            clientGroup = parentGroup.difference(serverGroup);
        } break;
    }
    return MpiPeerSetup(MpiPeer{groupName, groupComm.rank()}, parentGroup, clientGroup, serverGroup);
}

size_t getMpiPoolSize(const ConfigurationContext& confCtx) {
    switch (confCtx.localPeerTag()) {
        case util::LocalPeerTag::Server: {
            auto pServ = util::getEnv("MULTIO_SERVER_MPI_POOL_SIZE");
            if (pServ) {
                return eckit::translate<size_t>(std::string{*pServ});
            };
            auto pMul = util::getEnv("MULTIO_MPI_POOL_SIZE");
            if (pMul) {
                return eckit::translate<size_t>(std::string{*pMul});
            };
            return defaultPoolSize;
        }
        case util::LocalPeerTag::Client: {
            auto pClient = util::getEnv("MULTIO_CLIENT_MPI_POOL_SIZE");
            if (pClient) {
                return eckit::translate<size_t>(std::string{*pClient});
            };
            auto pMul = util::getEnv("MULTIO_MPI_POOL_SIZE");
            if (pMul) {
                return eckit::translate<size_t>(std::string{*pMul});
            };
            return defaultPoolSize;
        }
        default:
            std::ostringstream oss;
            oss << "getMpiBufferSize: localPeerTag is neither Server (" << ((unsigned)util::LocalPeerTag::Server)
                << ") nor Client (" << ((unsigned)util::LocalPeerTag::Server)
                << "). Value: " << ((unsigned)confCtx.localPeerTag()) << std::endl;
            throw TransportException("", Here());
    }
}

size_t getMpiBufferSize(const ConfigurationContext& confCtx) {
    switch (confCtx.localPeerTag()) {
        case util::LocalPeerTag::Server: {
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
        case util::LocalPeerTag::Client: {
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
            oss << "getMpiBufferSize: localPeerTag is neither Server (" << ((unsigned)util::LocalPeerTag::Server)
                << ") nor Client (" << ((unsigned)util::LocalPeerTag::Server)
                << "). Value: " << ((unsigned)confCtx.localPeerTag()) << std::endl;
            throw TransportException("", Here());
    }
}


}  // namespace


MpiTransport::MpiTransport(const ConfigurationContext& confCtx, MpiPeerSetup&& peerSetup) :
    Transport(confCtx),
    local_{std::move(std::get<0>(peerSetup))},
    parentGroup_{std::move(std::get<1>(peerSetup))},
    clientGroup_{std::move(std::get<2>(peerSetup))},
    serverGroup_{std::move(std::get<3>(peerSetup))},
    pool_{getMpiPoolSize(confCtx), getMpiBufferSize(confCtx), comm(), statistics_} {}

MpiTransport::MpiTransport(const ConfigurationContext& confCtx) : MpiTransport(confCtx, setupMPI_(confCtx)) {}

MpiTransport::~MpiTransport() {
    std::ofstream logFile{util::logfile_name(), std::ios_base::app};
    logFile << "\n ** " << *this << "\n";
    statistics_.report(logFile);
}

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
        pool_.sendBuffer(msg.destination(), static_cast<int>(msg.tag()));
    }
    pool_.waitAll();
}

Message MpiTransport::receive() {
    util::ScopedTiming timing{statistics_.totReturnTimer_, statistics_.totReturnTiming_};
    /**
     * Read raw messages from streamQueue_ (filled by listen() in other thread)
     *
     * Decode and add to msgPack_ (msgQueue)
     *
     * Return single messages until msgPack_ is empty and start over
     */

    do {
        while (not msgPack_.empty()) {
            util::ScopedTiming retTiming{statistics_.returnTimer_, statistics_.returnTiming_};
            //! TODO For switch to MPMC queue: combine front() and pop()
            auto msg = msgPack_.front();
            msgPack_.pop();
            return msg;
        }

        //! TODO For switch to MPMC queue: combine front() and pop()
        if (auto strm = streamQueue_.front()) {
            while (strm->position() < strm->size()) {
                util::ScopedTiming decodeTiming{statistics_.decodeTimer_, statistics_.decodeTiming_};
                auto msg = decodeMessage(*strm);
                msgPack_.push(msg);
            }
            streamQueue_.pop();
        }

    } while (true);
}

void MpiTransport::abort() {
    comm().abort();
}

void MpiTransport::send(const Message& msg) {
    std::lock_guard<std::mutex> lock{mutex_};

    auto msg_tag = static_cast<int>(msg.tag());

    // TODO: find available buffer instead
    // Add 4K for header/footer etc. Should be plenty
    eckit::Buffer buffer{eckit::round(msg.size(), 8) + 4096};

    eckit::ResizableMemoryStream stream{buffer};

    encodeMessage(stream, msg);

    util::ScopedTiming timing{statistics_.sendTimer_, statistics_.sendTiming_};

    auto sz = static_cast<size_t>(stream.bytesWritten());
    auto dest = static_cast<int>(msg.destination().id());

    // eckit::Log::info() << " *** MpiTransport::send from " << local_.group() << " " << local_.id
    // << std::endl;
    eckit::mpi::comm(local_.group().c_str()).send<void>(buffer, sz, dest, msg_tag);

    ++statistics_.sendCount_;
    statistics_.sendSize_ += sz;
}

void MpiTransport::bufferedSend(const Message& msg) {
    std::lock_guard<std::mutex> lock{mutex_};
    encodeMessage(pool_.getStream(msg), msg);
}

void MpiTransport::createPeers() const {
    auto parentSize = comm().size();
    std::vector<int> parentRanks(parentSize);

    for (int r = 0; r < parentSize; ++r) {
        parentRanks[r] = r;
    }
    // clientGroup_ and serverGroup_ are disjoint and their union are expected to be the full parentGroup_
    // In theory we need just an translate_ranks call, however for simplicity we translate from the groups explicitly.
    auto clientRankMap = parentGroup_.translate_ranks(parentRanks, clientGroup_);
    auto serverRankMap = parentGroup_.translate_ranks(parentRanks, serverGroup_);

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

Peer MpiTransport::localPeer() const {
    return local_;
}

void MpiTransport::listen() {
    auto status = probe();
    if (status.error()) {
        return;
    }
    // TODO status contains information on required message size - use that to retrieve a sufficient
    // large buffer?
    auto& buf = pool_.findAvailableBuffer();
    auto sz = blockingReceive(status, buf);
    util::ScopedTiming timing{statistics_.pushToQueueTimer_, statistics_.pushToQueueTiming_};
    streamQueue_.emplace(buf, sz);
}

PeerList MpiTransport::createServerPeers() const {
    PeerList serverPeers;

    auto parentSize = comm().size();
    std::vector<int> parentRanks(parentSize);

    for (int r = 0; r < parentSize; ++r) {
        parentRanks[r] = r;
    }
    auto serverRankMap = parentGroup_.translate_ranks(parentRanks, serverGroup_);

    for (auto& it : serverRankMap) {
        serverPeers_.emplace_back(std::make_unique<MpiPeer>(local_.group(), (unsigned long)it.first));
    }
    eckit::Log::info() << " *** MpiTransport::createServerPeers serverCount: " << serverPeers.size()
                       << ", commSize: " << parentSize << std::endl;

    return serverPeers;
}

const eckit::mpi::Comm& MpiTransport::comm() const {
    return eckit::mpi::comm(local_.group().c_str());
}

eckit::mpi::Status MpiTransport::probe() {
    util::ScopedTiming timing{statistics_.probeTimer_, statistics_.probeTiming_};
    auto status = comm().iProbe(comm().anySource(), comm().anyTag());

    return status;
}

size_t MpiTransport::blockingReceive(eckit::mpi::Status& status, MpiBuffer& buffer) {
    auto sz = comm().getCount<void>(status);
    ASSERT(sz < buffer.content.size());

    util::ScopedTiming timing{statistics_.receiveTimer_, statistics_.receiveTiming_};
    comm().receive<void>(buffer.content, sz, status.source(), status.tag());

    ++statistics_.receiveCount_;
    statistics_.receiveSize_ += sz;

    return sz;
}

void MpiTransport::encodeMessage(eckit::Stream& strm, const Message& msg) {
    util::ScopedTiming timing{statistics_.encodeTimer_, statistics_.encodeTiming_};

    msg.encode(strm);
}

static TransportBuilder<MpiTransport> MpiTransportBuilder("mpi");

}  // namespace transport
}  // namespace multio

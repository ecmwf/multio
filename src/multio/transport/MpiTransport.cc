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

#include "eckit/config/Resource.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/maths/Functions.h"
#include "eckit/mpi/Parallel.h"
#include "eckit/runtime/Main.h"
#include "eckit/serialisation/MemoryStream.h"

#include "multio/transport/MpiCommSetup.h"
#include "multio/util/ScopedTimer.h"
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

    return Message{Message::Header{static_cast<Message::Tag>(t), MpiPeer{src_grp, src_id},
                                   MpiPeer{dest_grp, dest_id}, std::move(fieldId)},
                   std::move(buffer)};
}

const size_t defaultBufferSize = 64 * 1024 * 1024;
const size_t defaultPoolSize = 128;

void MPICall(int code, const char* mpifunc, const eckit::CodeLocation& loc) {
    if (code != MPI_SUCCESS) {
        char error[10240];
        int len = sizeof(error) - 1;
        MPI_Error_string(code, error, &len);
        error[len] = 0;

        std::ostringstream oss;
        oss << "MpiTransport setup: MPI call failed with error '" << error << "' while calling "
            << mpifunc;
        throw eckit::Exception(oss.str(), loc);
    }
}

#define MPI_CALL(a) MPICall(a, #a, Here())

eckit::mpi::Parallel& assumeParellelComm(eckit::mpi::Comm& comm) {
    try {
        return dynamic_cast<eckit::mpi::Parallel&>(comm);
    }
    catch (const std::bad_cast& e) {
        throw eckit::Exception(
            std::string("MpiTransport setup: Failed to cast generic communicator to underlying "
                        "eckit::mpi::Parallel implementation.") +
            e.what());
    }
}

MpiPeerSetup setupMPI_(const ConfigurationContext& confCtx) {
    if (!confCtx.config().has("group")) {
        std::ostringstream oss;
        oss << "No key \"group\" in MPI server config found (Configuration filename:  "
            << confCtx.fileName() << ")" << std::endl;
        throw eckit::Exception(oss.str());
    }
    const std::string& groupName = confCtx.config().getString("group");
    mpi::CommSetupOptions groupOptions;
    groupOptions.defaultType = eckit::Optional<mpi::CommSetupType>(mpi::CommSetupType::Passed);

    eckit::mpi::Comm& groupComm = mpi::getComm(
        confCtx, groupName, eckit::Optional<mpi::CommSetupOptions>{std::move(groupOptions)});

    MPI_Group parentGroup;
    MPI_Group clientGroup;
    MPI_Group serverGroup;

    MPI_CALL(MPI_Comm_group(assumeParellelComm(groupComm).MPIComm(), &parentGroup));

    switch (confCtx.localPeerTag()) {
        case util::LocalPeerTag::Client: {
            mpi::CommSetupOptions options;
            // Add default in case of missing configuration
            options.defaultType = eckit::Optional<mpi::CommSetupType>(mpi::CommSetupType::Split);
            options.parentCommName = eckit::Optional<std::string>(groupName);

            const auto& mpiInitInfo = confCtx.getMPIInitInfo();
            options.alias = mpiInitInfo ? mpiInitInfo().clientId : eckit::Optional<std::string>{};

            std::string subGroupName = confCtx.config().has("client-group")
                                           ? confCtx.config().getString("client-group")
                                           : ([&]() {
                                                 std::ostringstream oss;
                                                 oss << groupName << "-"
                                                     << "clients";
                                                 return oss.str();
                                             })();
            // eckit::Log::info() << " *** MpiTransport::setupMPI_ client " << subGroupName << "
            // alias: " << (options.alias? options.alias().c_str() : "none") << std::endl;

            // Setup client group
            auto& clientComm = mpi::getComm(
                confCtx, subGroupName, eckit::Optional<mpi::CommSetupOptions>{std::move(options)});
            // eckit::Log::info() << " *** MpiTransport::setupMPI_ created clientComm... " << std::endl;
            
            if( mpiInitInfo && mpiInitInfo().returnClientComm != nullptr ) {
                *(mpiInitInfo().returnClientComm) = clientComm.communicator();
                mpiInitInfo().returnClientComm = nullptr; // Set to null to prevent setting the pointer at a later time when it may be invalid
            }
            
            MPI_CALL(MPI_Comm_group(assumeParellelComm(clientComm).MPIComm(), &clientGroup));
            MPI_CALL(MPI_Group_difference(parentGroup, clientGroup, &serverGroup));
        } break;
        case util::LocalPeerTag::Server: {
            mpi::CommSetupOptions options;
            // Add default in case of missing configuration
            options.defaultType = eckit::Optional<mpi::CommSetupType>(mpi::CommSetupType::Split);
            options.parentCommName = eckit::Optional<std::string>(groupName);
            
            const auto& mpiInitInfo = confCtx.getMPIInitInfo();

            std::string subGroupName = confCtx.config().has("server-group")
                                           ? confCtx.config().getString("server-group")
                                           : ([&]() {
                                                 std::ostringstream oss;
                                                 oss << groupName << "-"
                                                     << "servers";
                                                 return oss.str();
                                             })();
            // eckit::Log::info() << " *** MpiTransport::setupMPI_ server " << subGroupName <<
            // std::endl;

            // Setup client group
            auto& serverComm = mpi::getComm(
                confCtx, subGroupName, eckit::Optional<mpi::CommSetupOptions>{std::move(options)});
                
            if( mpiInitInfo && mpiInitInfo().returnServerComm != nullptr ) {
                *(mpiInitInfo().returnServerComm) = serverComm.communicator();
                mpiInitInfo().returnServerComm = nullptr; // Set to null to prevent setting the pointer at a later time when it may be invalid
            }
            
            MPI_CALL(MPI_Comm_group(assumeParellelComm(serverComm).MPIComm(), &serverGroup));
            MPI_CALL(MPI_Group_difference(parentGroup, serverGroup, &clientGroup));
        } break;
    }
    return MpiPeerSetup(MpiPeer{groupName, groupComm.rank()}, parentGroup, clientGroup, serverGroup);
}


}  // namespace


MpiTransport::MpiTransport(const ConfigurationContext& confCtx, MpiPeerSetup&& peerSetup) :
    Transport(confCtx),
    local_{std::move(std::get<0>(peerSetup))},
    parentGroup_{std::move(std::get<1>(peerSetup))},
    clientGroup_{std::move(std::get<2>(peerSetup))},
    serverGroup_{std::move(std::get<3>(peerSetup))},
    pool_{eckit::Resource<size_t>("multioMpiPoolSize;$MULTIO_MPI_POOL_SIZE", defaultPoolSize),
          eckit::Resource<size_t>("multioMpiBufferSize;$MULTIO_MPI_BUFFER_SIZE", defaultBufferSize),
          comm(), statistics_} {}

MpiTransport::MpiTransport(const ConfigurationContext& confCtx) :
    MpiTransport(confCtx, setupMPI_(confCtx)) {}

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
                util::ScopedTiming decodeTiming{statistics_.decodeTimer_,
                                                statistics_.decodeTiming_};
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
    std::vector<int> clientRanks(parentSize);
    std::vector<int> serverRanks(parentSize);
    
    for(int r=0; r < parentSize; ++r) {
        parentRanks[r] = r;
    }
    // clientGroup_ and serverGroup_ are disjoint and their union are expected to be the full parentGroup_
    // In theory we need just an translate_ranks call, however for simplicity we translate from the groups explicitly.
    MPI_CALL(MPI_Group_translate_ranks(parentGroup_, parentSize, parentRanks.data(), clientGroup_, clientRanks.data()));
    MPI_CALL(MPI_Group_translate_ranks(parentGroup_, parentSize, parentRanks.data(), serverGroup_, serverRanks.data()));
    
    for(int r=0; r < parentSize; ++r) {
        if(clientRanks[r] != MPI_UNDEFINED) {
            clientPeers_.emplace_back(new MpiPeer{local_.group(), (unsigned long) parentRanks[r]});
        }
        if(serverRanks[r] != MPI_UNDEFINED) {
            serverPeers_.emplace_back(new MpiPeer{local_.group(), (unsigned long) parentRanks[r]});
        }
    }
    eckit::Log::info() << " *** MpiTransport::createPeers clientCount: " << clientPeers_.size()
                       << ", serverCount: " << serverPeers_.size()
                       << ", commSize: " << parentSize << std::endl;
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
    std::vector<int> serverRanks(parentSize);
    
    for(int r=0; r < parentSize; ++r) {
        parentRanks[r] = r;
    }
    MPI_CALL(MPI_Group_translate_ranks(parentGroup_, parentSize, parentRanks.data(), serverGroup_, serverRanks.data()));
    
    for(int r=0; r < parentSize; ++r) {
        if(serverRanks[r] != MPI_UNDEFINED) {
            serverPeers.emplace_back(new MpiPeer{local_.group(), (unsigned long) parentRanks[r]});
        }
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

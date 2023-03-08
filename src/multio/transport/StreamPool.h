#pragma once

#include <sstream>

#include "multio/LibMultio.h"
#include "multio/message/Message.h"
#include "multio/transport/MpiStream.h"
#include "multio/transport/TransportStatistics.h"

namespace multio::transport {

class MpiPeer : public message::Peer {
public:
    MpiPeer(Peer peer);
    MpiPeer(const std::string& comm, size_t rank);
};

class StreamPool {
public:
    explicit StreamPool(size_t poolSize, size_t maxBufSize, const eckit::mpi::Comm& comm, TransportStatistics& stats);

    MpiBuffer& buffer(size_t idx);

    MpiOutputStream& getStream(const message::Message& msg);

    void sendBuffer(const message::Peer& dest, int msg_tag);

    MpiBuffer& acquireAvailableBuffer(BufferStatus newStatus, std::ostream& os = eckit::Log::debug<LibMultio>());

    void waitAll();

private:
    MpiOutputStream& createNewStream(const message::Peer& dest);
    MpiOutputStream& replaceStream(const message::Peer& dest);

    void print(std::ostream& os) const;

    friend std::ostream& operator<<(std::ostream& os, const StreamPool& pool) {
        pool.print(os);
        return os;
    }

    const eckit::mpi::Comm& comm_;
    TransportStatistics& statistics_;
    std::vector<MpiBuffer> buffers_;
    std::map<MpiPeer, MpiOutputStream> streams_;

    std::map<MpiPeer, unsigned int> counter_;
    std::ostringstream os_;
};

}  // namespace multio::transport

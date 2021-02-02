
#ifndef multio_server_StreamPool_H
#define multio_server_StreamPool_H

#include "eckit/log/Statistics.h"

#include "multio/message/Message.h"
#include "multio/server/MpiStream.h"

namespace multio {
namespace server {

class MpiPeer : public message::Peer {
public:
    MpiPeer(Peer peer);
    MpiPeer(const std::string& comm, size_t rank);
};

class StreamPool {
public:
    explicit StreamPool(size_t poolSize, size_t maxBufSize, const eckit::mpi::Comm& comm);

    MpiBuffer& buffer(size_t idx);

    MpiStream& getStream(const message::Message& msg);

    void send(const message::Message& msg);

    void timings(std::ostream& os) const;

private:
    MpiBuffer& findAvailableBuffer();

    MpiStream& createNewStream(const message::Peer& dest);
    MpiStream& replaceStream(const message::Peer& dest);

    void print(std::ostream& os) const;

    friend std::ostream& operator<<(std::ostream& os, const StreamPool& transport) {
        transport.print(os);
        return os;
    }

    const eckit::mpi::Comm& comm_;
    std::vector<MpiBuffer> buffers_;
    std::map<MpiPeer, MpiStream> streams_;

    eckit::Timing sendTiming_;
    eckit::Timing waitTiming_;

    std::size_t bytesSent_ = 0;
};

}  // namespace server
}  // namespace multio

#endif // multio_server_StreamPool_H

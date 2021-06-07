
#ifndef multio_server_StreamPool_H
#define multio_server_StreamPool_H

#include <sstream>

#include "eckit/log/Statistics.h"

#include "multio/LibMultio.h"
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

    MpiOutputStream& getStream(const message::Message& msg);

    void sendBuffer(const message::Peer& dest, int msg_tag);

    MpiBuffer& findAvailableBuffer(std::ostream& os = eckit::Log::debug<LibMultio>());

    void waitAll();

    void timings(std::ostream& os) const;

private:
    MpiOutputStream& createNewStream(const message::Peer& dest);
    MpiOutputStream& replaceStream(const message::Peer& dest);

    void print(std::ostream& os) const;

    friend std::ostream& operator<<(std::ostream& os, const StreamPool& transport) {
        transport.print(os);
        return os;
    }

    const eckit::mpi::Comm& comm_;
    std::vector<MpiBuffer> buffers_;
    std::map<MpiPeer, MpiOutputStream> streams_;

    eckit::Timing isendTiming_;
    eckit::Timing sendTiming_;
    eckit::Timing waitTiming_;

    std::size_t bytesSent_ = 0;

    std::map<MpiPeer, unsigned int> counter_;
    std::ostringstream os_;
};

}  // namespace server
}  // namespace multio

#endif // multio_server_StreamPool_H

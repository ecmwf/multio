
/// @author Domokos Sarmany
/// @author Tiago Quintino

/// @date Jan 2021

#ifndef multio_server_MpiStream_H
#define multio_server_MpiStream_H

#include "eckit/mpi/Comm.h"
#include "eckit/io/ResizableBuffer.h"
#include "eckit/serialisation/ResizableMemoryStream.h"

namespace multio {
namespace server {

enum class BufferStatus : uint8_t
{
    available,
    inUse,
    inTransit
};

struct MpiBuffer {
    explicit MpiBuffer(size_t maxBufSize);

    bool isFree();

    BufferStatus status = BufferStatus::available;
    eckit::mpi::Request request;
    eckit::ResizableBuffer content;
};

struct MpiStream : public eckit::ResizableMemoryStream {
    MpiStream(MpiBuffer& buf);

    bool readyToSend(size_t sz);
    MpiBuffer& buffer() const;

private:
    MpiBuffer& buf_;
};

}  // namespace server
}  // namespace multio

#endif // multio_server_MpiStream_H

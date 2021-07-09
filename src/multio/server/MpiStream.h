
/// @author Domokos Sarmany
/// @author Tiago Quintino

/// @date Jan 2021

#ifndef multio_server_MpiStream_H
#define multio_server_MpiStream_H

#include "eckit/mpi/Comm.h"
#include "eckit/io/Buffer.h"
#include "eckit/serialisation/ResizableMemoryStream.h"

namespace multio {
namespace server {

enum class BufferStatus : uint8_t
{
    available,
    fillingUp,
    transmitting
};

class MpiBuffer {
public:
    explicit MpiBuffer(size_t maxBufSize);

    bool isFree();

    BufferStatus status = BufferStatus::available;
    eckit::mpi::Request request;
    eckit::Buffer content;
};

class MpiOutputStream : public eckit::ResizableMemoryStream {
public:
    MpiOutputStream(MpiBuffer& buf);

    bool canFitMessage(size_t sz);
    bool shallFitMessage(size_t sz);

    MpiBuffer& buffer() const;

private:
    std::string name() const override;

    MpiBuffer& buf_;
};

class MpiInputStream : public eckit::ResizableMemoryStream {
public:
    MpiInputStream(MpiBuffer& buf, size_t sz);

    MpiBuffer& buffer() const;

    size_t size() const;

private:
    std::string name() const override;

    MpiBuffer& buf_;
    size_t size_;
};

}  // namespace server
}  // namespace multio

#endif // multio_server_MpiStream_H


/// @author Domokos Sarmany
/// @author Tiago Quintino

/// @date Jan 2021

#pragma once

#include "eckit/io/Buffer.h"
#include "eckit/mpi/Comm.h"
#include "eckit/serialisation/ResizableMemoryStream.h"

#include <atomic>

namespace multio::transport {

enum class BufferStatus : uint8_t
{
    available,
    fillingUp,
    transmitting
};

class MpiBuffer {
public:
    explicit MpiBuffer(size_t maxBufSize);

    MpiBuffer(MpiBuffer&&);


    MpiBuffer& operator=(MpiBuffer&& other);

    bool isFree();

    std::atomic<BufferStatus> status{BufferStatus::available};
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

}  // namespace multio::transport


#include "MpiStream.h"

namespace multio {
namespace server {

MpiBuffer::MpiBuffer(size_t maxBufSize) : content{maxBufSize} {}

bool MpiBuffer::isFree() {
    return status == BufferStatus::available ||
            (status == BufferStatus::inTransit && request.test());
}

MpiStream::MpiStream(MpiBuffer& buf) : eckit::ResizableMemoryStream{buf.content}, buf_{buf} {}

bool MpiStream::readyToSend(size_t sz) {
    return (position() + sz + 4096 > buf_.content.size());
}

MpiBuffer& MpiStream::buffer() const {
    return buf_;
}
}
}  // namespace multio

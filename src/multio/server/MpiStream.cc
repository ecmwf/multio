
#include "MpiStream.h"

namespace multio {
namespace server {

MpiBuffer::MpiBuffer(size_t maxBufSize) : content{maxBufSize} {}

bool MpiBuffer::isFree() {
    return status == BufferStatus::available ||
            (status == BufferStatus::transmitting && request.test());
}

MpiStream::MpiStream(MpiBuffer& buf) : eckit::ResizableMemoryStream{buf.content}, buf_{buf} {}

bool MpiStream::canFitMessage(size_t sz) {
    return (position() + sz + 4096 < buf_.content.size());
}

MpiBuffer& MpiStream::buffer() const {
    return buf_;
}

std::string MpiStream::name() const {
    static const std::map<BufferStatus, std::string> st2str{
        {BufferStatus::available, "available"},
        {BufferStatus::fillingUp, "fillingUp"},
        {BufferStatus::transmitting, "transmitting"}};

    return "MpiStream(" + st2str.at(buf_.status) + ")";
}
}
}  // namespace multio


#include "MpiStream.h"

#include <random>

namespace multio::transport {

MpiBuffer::MpiBuffer(size_t maxBufSize) : content{maxBufSize} {}

bool MpiBuffer::isFree() {
    // Soft check
    BufferStatus s = status.load(std::memory_order_relaxed);
    return s == BufferStatus::available || (s == BufferStatus::transmitting && request.test());
}

MpiBuffer& MpiBuffer::operator=(MpiBuffer&& other) {
    this->status.exchange(other.status);
    this->request = std::move(other.request);
    this->content = std::move(other.content);
    return *this;
}

MpiBuffer::MpiBuffer(MpiBuffer&& other) {
    this->status.exchange(other.status);
    this->request = std::move(other.request);
    this->content = std::move(other.content);
}


MpiOutputStream::MpiOutputStream(MpiBuffer& buf) : eckit::ResizableMemoryStream{buf.content}, buf_{buf} {}

bool MpiOutputStream::canFitMessage(size_t sz) {
    return (position() + sz + 4096 < buf_.content.size());
}

bool MpiOutputStream::shallFitMessage(size_t sz) {
    if (not canFitMessage(sz)) {
        return false;
    }

    // return true;

    auto ratio = static_cast<double>(position()) / static_cast<double>(buf_.content.size());

    std::random_device rd;   // Will be used to obtain a seed for the random number engine
    std::mt19937 gen{rd()};  // Standard mersenne_twister_engine seeded with rd()
    std::uniform_real_distribution<double> dis{0.5, 1.0};
    auto randVal = dis(gen);

    return ratio < randVal;
}

MpiBuffer& MpiOutputStream::buffer() const {
    return buf_;
}

std::string MpiOutputStream::name() const {
    static const std::map<BufferStatus, std::string> st2str{{BufferStatus::available, "available"},
                                                            {BufferStatus::fillingUp, "fillingUp"},
                                                            {BufferStatus::transmitting, "transmitting"}};

    return "MpiOutputStream(" + st2str.at(buf_.status) + ")";
}

}  // namespace multio::transport

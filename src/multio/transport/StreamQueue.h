
#pragma once

#include <queue>

#include "multio/transport/MpiStream.h"

namespace multio {
namespace transport {

class StreamQueue {
public:
    StreamQueue();

    // TODO: Decoupling front & pop is bad when multiple consumers (i.e. multiple threads) race for the next value.
    // Currently tthe Queue is used in a SPSC (single-produces, single-consumer) fashion. # Philipp Geier
    MpiInputStream* front() {
        std::lock_guard<std::mutex> lock{mutex_};
        if (queue_.empty()) {
            return nullptr;
        }
        auto& strm = queue_.front();
        return &strm;
    }

    void pop() {
        const auto strm = front();
        std::lock_guard<std::mutex> lock{mutex_};
        strm->buffer().status = BufferStatus::available;
        queue_.pop();
    }

    template <typename... Args>
    void emplace(Args&&... args) {
        std::lock_guard<std::mutex> lock{mutex_};
        queue_.emplace(std::forward<Args>(args)...);
    }

private:
    std::queue<MpiInputStream> queue_;
    std::mutex mutex_;
};

}  // namespace transport
}  // namespace multio

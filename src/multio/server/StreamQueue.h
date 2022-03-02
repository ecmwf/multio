
#ifndef multio_server_StreamQueue_H
#define multio_server_StreamQueue_H

#include <queue>

#include "multio/server/MpiStream.h"

namespace multio {
namespace server {

class StreamQueue {
public:
    StreamQueue();

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

}  // namespace server
}  // namespace multio

#endif // multio_server_StreamQueue_H


#pragma once

#include <queue>

#include "multio/transport/MpiStream.h"

namespace multio::transport {

struct ReceivedBuffer {
    MpiBuffer* buffer;
    size_t size;
};



class StreamQueue {
private:
    
public:
    StreamQueue();

    bool pop(ReceivedBuffer& b) {
        std::lock_guard<std::mutex> lock{mutex_};
        if (queue_.empty()) {
            return false;
        }
        b = queue_.front();
        queue_.pop();
        return true;
    }

    template <typename... Args>
    void emplace(Args&&... args) {
        std::lock_guard<std::mutex> lock{mutex_};
        queue_.emplace(std::forward<Args>(args)...);
    }
    
    void push(const ReceivedBuffer& b) {
        std::lock_guard<std::mutex> lock{mutex_};
        queue_.push(b);
    }
    
    void push(ReceivedBuffer&& b) {
        std::lock_guard<std::mutex> lock{mutex_};
        queue_.push(std::move(b));
    }

private:
    std::queue<ReceivedBuffer> queue_;
    std::mutex mutex_;
};

}  // namespace multio::transport

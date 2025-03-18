
#pragma once

#include <thread>

#include "eckit/exception/Exceptions.h"

namespace multio::util {

class ScopedThread {
public:
    explicit ScopedThread(std::thread thread) : joined_{false}, thread_(std::move(thread)) {
        if (!thread_.joinable()) {
            throw eckit::SeriousBug("No thread");
        }
    }

    ~ScopedThread() { join(); }

    ScopedThread(const ScopedThread& rhs) = delete;
    ScopedThread& operator=(const ScopedThread& rhs) = delete;

    void join() {
        if (joined_) {
            return;
        }

        joined_ = true;

        if (thread_.joinable()) {
            thread_.join();
        }
    }

private:  // members
    bool joined_;
    std::thread thread_;
};

}  // namespace multio::util

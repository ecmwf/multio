#pragma once

#include <memory>

#include "eckit/container/Queue.h"
#include "multio/message/Message.h"

#include "MultIOProfilerState.h"

namespace multio::server {

class MultIOQueue {
public:
    using value_type = multio::message::Message;

    MultIOQueue(size_t capacity,
                MultIOProfilerState& profiler);

    ~MultIOQueue();

    MultIOQueue(const MultIOQueue&) = delete;
    MultIOQueue& operator=(const MultIOQueue&) = delete;

    // push side
    void emplace(value_type&& msg);

    // pop side (Dispatcher)
    long pop(value_type& msg);

    void close();
    bool closed() const;

    // needed by existing Dispatcher without refactor
    eckit::Queue<value_type>& impl() noexcept;

    size_t capacity() const noexcept;

private:
    struct Impl;
    std::unique_ptr<Impl> impl_;

    MultIOProfilerState& profiler_;
};

}  // namespace multio::server
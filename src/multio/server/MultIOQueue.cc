#include "MultIOQueue.h"


#include "eckit/config/Resource.h"

namespace multio::server {


MultIOQueue::MultIOQueue(size_t capacity,
                         MultIOProfilerState& profiler) :
    queue_(eckit::Resource<size_t>("multioMessageQueueSize;$MULTIO_MESSAGE_QUEUE_SIZE", 1024 * 1024)),
    profiler_(profiler) {}

MultIOQueue::~MultIOQueue() = default;


void MultIOQueue::emplace(value_type&& msg) {

    const auto sz = msg.size();   // adjust if different API

    queue_.emplace(std::move(msg));

    auto fill = profiler_.queue().fill.fetch_add(1, std::memory_order_relaxed) + 1;

    profiler_.queue().pushes.fetch_add(1, std::memory_order_relaxed);
    profiler_.queue().bytesIn.fetch_add(sz, std::memory_order_relaxed);

    auto prevMax = profiler_.queue().maxFill.load(std::memory_order_relaxed);
    while (fill > prevMax &&
           !profiler_.queue().maxFill.compare_exchange_weak(
               prevMax, fill, std::memory_order_relaxed)) {}
}


long MultIOQueue::pop(value_type& msg) {

    auto ret = queue_.pop(msg);

    if (ret >= 0) {

        const auto sz = msg.size();

        profiler_.queue().pops.fetch_add(1, std::memory_order_relaxed);
        profiler_.queue().bytesOut.fetch_add(sz, std::memory_order_relaxed);
        profiler_.queue().fill.fetch_sub(1, std::memory_order_relaxed);
    }

    return ret;
}


void MultIOQueue::close() {
    queue_.close();
}

bool MultIOQueue::closed() {
    return queue_.closed();
}

bool MultIOQueue::checkInterrupt() {
    return queue_.checkInterrupt();
}

void MultIOQueue::interrupt(std::exception_ptr expn) {
    queue_.interrupt(expn);
}


size_t MultIOQueue::capacity() const noexcept {
    return queue_.maxSize();
}

}  // namespace multio::server
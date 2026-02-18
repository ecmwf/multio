#include "MultIOQueue.h"

namespace multio::server {

struct MultIOQueue::Impl {
    explicit Impl(size_t capacity) :
        queue(capacity) {}

    eckit::Queue<value_type> queue;
};

MultIOQueue::MultIOQueue(size_t capacity,
                         MultIOProfilerState& profiler) :
    impl_(std::make_unique<Impl>(capacity)),
    profiler_(profiler) {}

MultIOQueue::~MultIOQueue() = default;


void MultIOQueue::emplace(value_type&& msg) {

    const auto sz = msg.size();   // adjust if different API

    impl_->queue.emplace(std::move(msg));

    auto fill = profiler_.queue.fill.fetch_add(1, std::memory_order_relaxed) + 1;

    profiler_.queue.pushes.fetch_add(1, std::memory_order_relaxed);
    profiler_.queue.bytesIn.fetch_add(sz, std::memory_order_relaxed);

    auto prevMax = profiler_.queue.maxFill.load(std::memory_order_relaxed);
    while (fill > prevMax &&
           !profiler_.queue.maxFill.compare_exchange_weak(
               prevMax, fill, std::memory_order_relaxed)) {}
}


long MultIOQueue::pop(value_type& msg) {

    auto ret = impl_->queue.pop(msg);

    if (ret >= 0) {

        const auto sz = msg.size();

        profiler_.queue.pops.fetch_add(1, std::memory_order_relaxed);
        profiler_.queue.bytesOut.fetch_add(sz, std::memory_order_relaxed);
        profiler_.queue.fill.fetch_sub(1, std::memory_order_relaxed);
    }

    return ret;
}


void MultIOQueue::close() {
    impl_->queue.close();
}

bool MultIOQueue::closed() const {
    return impl_->queue.closed();
}

eckit::Queue<MultIOQueue::value_type>& MultIOQueue::impl() noexcept {
    return impl_->queue;
}

size_t MultIOQueue::capacity() const noexcept {
    return impl_->queue.capacity();
}

}  // namespace multio::server
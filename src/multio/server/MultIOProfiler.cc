#include "MultIOQueueProfiler.h"

#include <thread>
#include <algorithm>
#include <iostream>

namespace multio::server {

MultIOProfiler::MultIOProfiler(
        const MultIOProfilerState& state,
        std::size_t queueCapacity,
        std::chrono::milliseconds period,
        std::size_t histogramBuckets) noexcept :

    state_(state),
    capacity_(queueCapacity),
    period_(period),
    histogram_(histogramBuckets, 0) {}


auto MultIOProfiler::readSample() const noexcept -> Sample {

    Sample s{};

    const auto& q = state_.queue();

    s.pushes = q.pushes.load(std::memory_order_relaxed);
    s.pops   = q.pops.load(std::memory_order_relaxed);
    s.fill   = q.fill.load(std::memory_order_relaxed);

    uint64_t dispatched = 0;

    for (const auto& d : state_.dispatchers()) {
        dispatched += d.messagesDispatched.load(std::memory_order_relaxed);
    }

    s.dispatched = dispatched;

    return s;
}


void MultIOProfiler::accumulateHistogram(uint64_t fill) {

    if (capacity_ == 0) return;

    double ratio = static_cast<double>(fill) / static_cast<double>(capacity_);

    std::size_t bucket =
        std::min(static_cast<std::size_t>(ratio * histogram_.size()),
                 histogram_.size() - 1);

    histogram_[bucket]++;
    samples_++;
}


void MultIOProfiler::run() {

    prev_ = readSample();
    prevTime_ = clock::now();

    while (!stop_) {

        std::this_thread::sleep_for(period_);

        auto now = clock::now();
        auto dt  = std::chrono::duration<double>(now - prevTime_).count();

        auto curr = readSample();

        accumulateHistogram(curr.fill);

        double pushRate =
            (curr.pushes - prev_.pushes) / dt;

        double popRate =
            (curr.pops - prev_.pops) / dt;

        double dispatchRate =
            (curr.dispatched - prev_.dispatched) / dt;

        double latency =
            popRate > 0.0 ? static_cast<double>(curr.fill) / popRate : 0.0;

        // You can store these values instead of printing
        (void)pushRate;
        (void)popRate;
        (void)dispatchRate;
        (void)latency;

        prev_ = curr;
        prevTime_ = now;
    }

    // Example final report (replace later with structured output)

    std::cout << "\n=== Queue Occupancy Histogram ===\n";
    for (std::size_t i = 0; i < histogram_.size(); ++i) {
        std::cout << "Bucket " << i << ": " << histogram_[i] << "\n";
    }

    std::cout << "Samples: " << samples_ << "\n";
}


void MultIOProfiler::stop() noexcept {
    stop_ = true;
}

}  // namespace multio::server
#pragma once

#include <chrono>
#include <vector>

#include "MultIOProfilerState.h"

namespace multio::server {

class MultIOProfiler {
public:

    using clock = std::chrono::steady_clock;

    MultIOProfiler(const MultIOProfilerState& state,
                        std::size_t queueCapacity,
                        std::chrono::milliseconds period,
                        std::size_t histogramBuckets = 10) noexcept;

    void run();
    void stop() noexcept;

private:

    struct Sample {
        uint64_t pushes{};
        uint64_t pops{};
        uint64_t fill{};
        uint64_t dispatched{};
    };

    Sample readSample() const noexcept;
    void accumulateHistogram(uint64_t fill);

private:

    const MultIOProfilerState& state_;
    const std::size_t capacity_;
    const std::chrono::milliseconds period_;

    bool stop_{false};

    std::vector<uint64_t> histogram_;
    uint64_t samples_{0};

    Sample prev_{};
    clock::time_point prevTime_;
};

}  // namespace multio::server
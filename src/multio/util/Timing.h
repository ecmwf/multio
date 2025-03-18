#pragma once

#include "eckit/log/Statistics.h"

#include <time.h>
#include <array>
#include <chrono>

namespace multio::util {

template <typename Clock = std::chrono::steady_clock, std::size_t NAgg = 16>
class Timing {
    using Duration = typename Clock::duration;
    using TimePoint = typename Clock::time_point;

public:
    void tic() {
        starts_[ind_] = Clock::now();
        cpu_starts_[ind_] = std::clock();
    }

    void toc() {
        ends_[ind_] = Clock::now();
        cpu_ends_[ind_] = std::clock();

        ++ind_;
        ++count_;
        if (ind_ >= NAgg) {
            process();
        }
    }

    Duration elapsedTime() const { return duration_; }

    double elapsedTimeSeconds() const {
        return std::chrono::duration<double, std::chrono::seconds::period>(duration_).count();
    }

    // To be devided by CLOCKS_PER_SEC
    std::clock_t elapsedCPU() const { return cpu_duration_; }

    //
    double elapsedCPUSeconds() const { return ((double)cpu_duration_) / CLOCKS_PER_SEC; }

    std::size_t updates() const { return count_; }

    operator eckit::Timing() {
        process();
        return eckit::Timing(elapsedTimeSeconds(), elapsedCPUSeconds(), updates());
    }

    void process() {
        if (ind_ == 0) {
            return;
        }

        // Loop over compile time constant to support auto vectorization.
        // For indices >= ind_ the computed difference should be 0
        for (std::size_t i = 0; i < NAgg; ++i) {
            duration_ += ends_[i] - starts_[i];
        }
        for (std::size_t i = 0; i < NAgg; ++i) {
            cpu_duration_ += cpu_ends_[i] - cpu_starts_[i];
        }

        starts_.fill(TimePoint{});
        ends_.fill(TimePoint{});
        cpu_starts_.fill(0);
        cpu_ends_.fill(0);

        ind_ = 0;
    }

private:
    std::size_t ind_{0};
    std::size_t count_{0};

    Duration duration_{0};
    // Value initialize arrays to 0 - their diff should be 0 as well
    std::array<TimePoint, NAgg> starts_{};
    std::array<TimePoint, NAgg> ends_{};

    clock_t cpu_duration_{0};
    // Value initialize arrays to 0 - their diff should be 0 as well
    std::array<std::clock_t, NAgg> cpu_starts_{};
    std::array<std::clock_t, NAgg> cpu_ends_{};
};

template <typename Clock, std::size_t NAgg>
std::ostream& operator<<(std::ostream& os, Timing<Clock, NAgg>& t) {
    t.process();
    os << "Time: " << t.elapsedTimeSeconds() << "s, Cpu: " << t.elapsedCPUSeconds() << "s, Updates: " << t.updates();
    return os;
}


template <typename Clock = std::chrono::steady_clock, std::size_t NAgg = 16>
class ScopedTiming {
    Timing<Clock, NAgg>& timing_;

public:
    explicit ScopedTiming(Timing<Clock, NAgg>& t) : timing_{t} { timing_.tic(); }
    ~ScopedTiming() { timing_.toc(); }
};

// Deduction guide
template <typename Clock = std::chrono::steady_clock, std::size_t NAgg = 16>
ScopedTiming(Timing<Clock, NAgg>&) -> ScopedTiming<Clock, NAgg>;


}  // namespace multio::util

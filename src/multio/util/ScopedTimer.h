#pragma once

#include "eckit/log/Statistics.h"

namespace multio {
namespace util {

class ScopedTimer {
    eckit::Timing& timing_;
    eckit::Timer timer_;

public:
    explicit ScopedTimer(eckit::Timing& t) : timing_{t} { timer_.start(); }
    ~ScopedTimer() {
        timer_.stop();
        timing_ += timer_;
    }
};

class ScopedTiming {
    eckit::Timer& timer_;
    eckit::Timing& timing_;
    eckit::Timing start_;

public:
    ScopedTiming(eckit::Timer& timer, eckit::Timing& timing) : timer_{timer}, timing_{timing}, start_{timer} {}

    ~ScopedTiming() { timing_ += eckit::Timing{timer_} - start_; }
};

}  // namespace util
}  // namespace multio

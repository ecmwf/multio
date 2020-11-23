
#ifndef multio_util_ScopedTimer_H
#define multio_util_ScopedTimer_H

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

}  // namespace util
}  // namespace multio

#endif

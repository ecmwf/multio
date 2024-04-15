#pragma once

#include <iosfwd>

#include "multio/util/Timing.h"

namespace multio::transport {

class TransportStatistics : public eckit::Statistics {
public:
    TransportStatistics();

    std::size_t isendCount_ = 0;
    std::size_t isendSize_ = 0;

    std::size_t sendCount_ = 0;
    std::size_t sendSize_ = 0;

    std::size_t receiveCount_ = 0;
    std::size_t receiveSize_ = 0;

    util::Timing<> waitTiming_;

    util::Timing<> isendTiming_;

    util::Timing<> sendTiming_;

    util::Timing<> encodeTiming_;


    util::Timing<> probeTiming_;

    util::Timing<> receiveTiming_;

    util::Timing<> pushToQueueTiming_;

    util::Timing<> decodeTiming_;

    util::Timing<> returnTiming_;

    util::Timing<> totReturnTiming_;

    void report(std::ostream& out, const char* indent = "");
};

}  // namespace multio::transport

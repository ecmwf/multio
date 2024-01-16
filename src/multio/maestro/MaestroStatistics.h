#pragma once

#include "multio/util/Timing.h"

namespace multio {

class MaestroStatistics : public eckit::Statistics {
public:
    MaestroStatistics();

    util::Timing<> sinkWriteTiming_;
    util::Timing<> sinkNameTiming_;
    util::Timing<> sinkAttributeTiming_;
    util::Timing<> sinkCdoCreationTiming_;
    util::Timing<> sinkCdoOfferTiming_;

    util::Timing<> syphonInitTiming_;
    util::Timing<> syphonExecuteTiming_;
    util::Timing<> syphonConsumeTiming_;
    util::Timing<> syphonJoinLeaveTiming_;
    util::Timing<> syphonOfferTiming_;
    util::Timing<> syphonBrokerTiming_;
    util::Timing<> syphonFinishTiming_;

    util::Timing<> workerProcessTiming_;
    util::Timing<> workerProcessPopWorkTiming_;
    util::Timing<> workerProcessInputTiming_;
    util::Timing<> workerProcessJobPrepareTiming_;

    util::Timing<> mirTiming_;

    void report(std::ostream& out, const char* indent = "") const;
};

}  // namespace multio

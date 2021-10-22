#ifndef multio_MaestroStatistics_H
#define multio_MaestroStatistics_H

#include "eckit/log/Statistics.h"

namespace multio {

class MaestroStatistics : public eckit::Statistics {
public:
    MaestroStatistics();
    
    eckit::Timing sinkWriteTiming_;
    eckit::Timing sinkNameTiming_;
    eckit::Timing sinkAttributeTiming_;
    eckit::Timing sinkCdoCreation_;
    eckit::Timing sinkCdoOffer_;

    eckit::Timing syphonInitTiming_;
    eckit::Timing syphonExecuteTiming_;
    eckit::Timing syphonConsumeTiming_;
    eckit::Timing syphonJoinLeaveTiming_;
    eckit::Timing syphonOfferTiming_;
    eckit::Timing syphonBrokerTiming_;
    eckit::Timing syphonFinishTiming_;

    eckit::Timing workerProcessTiming_;
    eckit::Timing workerProcessPopWorkTiming_;
    eckit::Timing workerProcessInputTiming_;
    eckit::Timing workerProcessJobPrepareTiming_;

    void report(std::ostream& out, const char *indent = "") const;
};

}  // namespace multio

#endif  // multio_MaestroStatistics_H

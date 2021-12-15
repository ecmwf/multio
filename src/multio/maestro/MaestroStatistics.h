#ifndef multio_MaestroStatistics_H
#define multio_MaestroStatistics_H

#include "eckit/log/Statistics.h"

namespace multio {

class MaestroStatistics : public eckit::Statistics {
public:
    MaestroStatistics();

    eckit::Timing sinkWriteTiming_;
    eckit::Timer  sinkWriteTimer_;
    eckit::Timing sinkNameTiming_;
    eckit::Timer  sinkNameTimer_;
    eckit::Timing sinkAttributeTiming_;
    eckit::Timer  sinkAttributeTimer_;
    eckit::Timing sinkCdoCreationTiming_;
    eckit::Timer  sinkCdoCreationTimer_;
    eckit::Timing sinkCdoOfferTiming_;
    eckit::Timer  sinkCdoOfferTimer_;

    eckit::Timing syphonInitTiming_;
    eckit::Timer  syphonInitTimer_;
    eckit::Timing syphonExecuteTiming_;
    eckit::Timer  syphonExecuteTimer_;
    eckit::Timing syphonConsumeTiming_;
    eckit::Timer  syphonConsumeTimer_;
    eckit::Timing syphonJoinLeaveTiming_;
    eckit::Timer  syphonJoinLeaveTimer_;
    eckit::Timing syphonOfferTiming_;
    eckit::Timer  syphonOfferTimer_;
    eckit::Timing syphonBrokerTiming_;
    eckit::Timer  syphonBrokerTimer_;
    eckit::Timing syphonFinishTiming_;
    eckit::Timer  syphonFinishTimer_;

    eckit::Timing workerProcessTiming_;
    eckit::Timer  workerProcessTimer_;
    eckit::Timing workerProcessPopWorkTiming_;
    eckit::Timer  workerProcessPopWorkTimer_;
    eckit::Timing workerProcessInputTiming_;
    eckit::Timer  workerProcessInputTimer_;
    eckit::Timing workerProcessJobPrepareTiming_;
    eckit::Timer  workerProcessJobPrepareTimer_;

    eckit::Timing  mirTiming_;
    eckit::Timer   mirTimer_;

    void report(std::ostream& out, const char *indent = "") const;
};

}  // namespace multio

#endif  // multio_MaestroStatistics_H

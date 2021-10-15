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

    eckit::Timing sourceConstructionTiming_;
    eckit::Timing sourceRetrieveTiming_;

    eckit::Timing cdoConstructionTiming_;
    eckit::Timing cdoDestructionTiming_;
    eckit::Timing cdoMoveTiming_;
    eckit::Timing cdoDeclareTiming_;
    eckit::Timing cdoSealTiming_;
    eckit::Timing cdoOfferTiming_;
    eckit::Timing cdoRequireTiming_;
    eckit::Timing cdoWithdrawTiming_;
    eckit::Timing cdoDemandTiming_;
    eckit::Timing cdoRetractTiming_;
    eckit::Timing cdoDisposeTiming_;
    eckit::Timing cdoSetSizeAndDataTiming_;
    eckit::Timing cdoGetSizeAndDataTiming_;

    eckit::Timing selectorConstructionTiming_;
    eckit::Timing selectorDestructionTiming_;
    eckit::Timing selectorSubscribeTiming_;

    eckit::Timing subscriptionConstructionTiming_;
    eckit::Timing subscriptionDestructionTiming_;
    eckit::Timing subscriptionPollTiming_;
    eckit::Timing subscriptionWaitTiming_;
    eckit::Timing subscriptionTimedWaitTiming_;
    eckit::Timing subscriptionAckTiming_;

    eckit::Timing eventConstructionTiming_;
    eckit::Timing eventDestructionTiming_;
    eckit::Timing eventMoveTiming_;
    eckit::Timing eventDisposeTiming_;
    eckit::Timing eventNonNullPolledTiming_;
    eckit::Timing eventNonNullDisposeTiming_;

    void report(std::ostream& out, const char *indent = "") const;
};

}  // namespace multio

#endif  // multio_MaestroStatistics_H

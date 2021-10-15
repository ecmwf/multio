#include "MaestroStatistics.h"

namespace multio {

MaestroStatistics::MaestroStatistics()
{}

void MaestroStatistics::report(std::ostream& out, const char* indent) const {
    reportTime(out, "MaestroSink: write method: ", sinkWriteTiming_, indent);
    reportTime(out, "MaestroSink: name creation: ", sinkNameTiming_, indent);
    reportTime(out, "MaestroSink: setting attributes: ", sinkAttributeTiming_, indent);
    reportTime(out, "MaestroSink: cdo creation: ", sinkCdoCreation_, indent);
    reportTime(out, "MaestroSink: cdo offer: ", sinkCdoOffer_, indent);

    reportTime(out, "MaestroSyphon: init: ", syphonInitTiming_, indent);
    reportTime(out, "MaestroSyphon: execute: ", syphonExecuteTiming_, indent);
    reportTime(out, "MaestroSyhpon: consume: ", syphonConsumeTiming_, indent);
    reportTime(out, "MaestroSyphon: join/leave: ", syphonJoinLeaveTiming_, indent);
    reportTime(out, "MaestroSyphon: offer: ", syphonOfferTiming_, indent);
    reportTime(out, "MaestroSyphon: broker: ", syphonBrokerTiming_, indent);
    reportTime(out, "MaestroSyphon: finish: ", syphonFinishTiming_, indent);

    reportTime(out, "MaestroWorker: process: ", workerProcessTiming_, indent);
    reportTime(out, "MaestroWorker: process->work after pop: ", workerProcessPopWorkTiming_, indent);
    reportTime(out, "MaestroWorker: process->input: ", workerProcessInputTiming_, indent);
    reportTime(out, "MaestroWorker: process->job->prepare: ", workerProcessJobPrepareTiming_, indent);

    reportTime(out, "MaestroSource: construction: ", sourceConstructionTiming_, indent);
    reportTime(out, "MaestroSource: retrieve: ", sourceRetrieveTiming_, indent);

    reportTime(out, "MaestroCdo: construction: ", cdoConstructionTiming_, indent);
    reportTime(out, "MaestroCdo: destruction: ", cdoDestructionTiming_, indent);
    reportTime(out, "MaestroCdo: move: ", cdoMoveTiming_, indent);
    reportTime(out, "MaestroCdo: declare: ", cdoDeclareTiming_, indent);
    reportTime(out, "MaestroCdo: seal: ", cdoSealTiming_, indent);
    reportTime(out, "MaestroCdo: offer: ", cdoOfferTiming_, indent);
    reportTime(out, "MaestroCdo: require: ", cdoRequireTiming_, indent);
    reportTime(out, "MaestroCdo: withdraw: ", cdoWithdrawTiming_, indent);
    reportTime(out, "MaestroCdo: demand: ", cdoDemandTiming_, indent);
    reportTime(out, "MaestroCdo: retract: ", cdoRetractTiming_, indent);
    reportTime(out, "MaestroCdo: dispose: ", cdoDisposeTiming_, indent);
    reportTime(out, "MaestroCdo: set_size_and_data: ", cdoSetSizeAndDataTiming_, indent);
    reportTime(out, "MaestroCdo: get_size_and_data: ", cdoGetSizeAndDataTiming_, indent);

    reportTime(out, "MaestroSelector: construction: ", selectorConstructionTiming_, indent);
    reportTime(out, "MaestroSelector: destruction: ", selectorDestructionTiming_, indent);
    reportTime(out, "MaestroSelector: subscribe: ", selectorSubscribeTiming_, indent);

    reportTime(out, "MaestroSubscription: construction: ", subscriptionConstructionTiming_, indent);
    reportTime(out, "MaestroSubscription: destruction: ", subscriptionDestructionTiming_, indent);
    reportTime(out, "MaestroSubscription: poll: ", subscriptionPollTiming_, indent);
    reportTime(out, "MaestroSubscription: wait: ", subscriptionWaitTiming_, indent);
    reportTime(out, "MaestroSubscription: timed_wait: ", subscriptionTimedWaitTiming_, indent);
    reportTime(out, "MaestroSubscription: ack: ", subscriptionAckTiming_, indent);

    reportTime(out, "MaestroEvent: construction: ", eventConstructionTiming_, indent);
    reportTime(out, "MaestroEvent: destruction: ", eventDestructionTiming_, indent);
    reportTime(out, "MaestroEvent: move: ", eventMoveTiming_, indent);
    reportTime(out, "MaestroEvent: dispose: ", eventDisposeTiming_, indent);
    reportTime(out, "MaestroEvent: non null polled: ", eventNonNullPolledTiming_, indent);
    reportTime(out, "MaestroEvent: non null dispose: ", eventNonNullDisposeTiming_, indent);
}

}  // namespace multio

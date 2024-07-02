#include "MaestroStatistics.h"

namespace multio {

MaestroStatistics::MaestroStatistics() {}

void MaestroStatistics::report(std::ostream& out, const char* indent) {
    reportTime(out, "MaestroSink: write method: ", sinkWriteTiming_, indent);
    reportTime(out, "MaestroSink: name creation: ", sinkNameTiming_, indent);
    reportTime(out, "MaestroSink: setting attributes: ", sinkAttributeTiming_, indent);
    reportTime(out, "MaestroSink: cdo creation: ", sinkCdoCreationTiming_, indent);
    reportTime(out, "MaestroSink: cdo offer: ", sinkCdoOfferTiming_, indent);

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

    reportTime(out, "MaestroWorker: mir: ", mirTiming_, indent);
}

}  // namespace multio

#include "StatisticsOptions.h"

#include "eckit/exception/Exceptions.h"

namespace multio {
namespace action {

StatisticsOptions::StatisticsOptions(const eckit::LocalConfiguration& confCtx) :
    useDateTime_{false}, stepFreq_{1}, timeStep_{3600}, startDate_{0}, startTime_{0} {

    if (!confCtx.has("options")) {
        return;
    }

    const auto& opt = confCtx.getSubConfiguration("options");

    useDateTime_ = opt.getBool("use-current-time", false);
    stepFreq_ = opt.getLong("step-frequency", 1L);
    timeStep_ = opt.getLong("time-step", 3600L);

    return;
};

StatisticsOptions::StatisticsOptions(const StatisticsOptions& opt, const message::Message& msg) :
    useDateTime_{opt.useDateTime()},
    stepFreq_{opt.stepFreq()},
    timeStep_{opt.timeStep()},
    startDate_{0},
    startTime_{0} {
    if (useDateTime() && msg.metadata().has("time")) {
        startDate_ = msg.metadata().getLong("time");
    }
    else if (!useDateTime() && msg.metadata().has("startTime")) {
        startDate_ = msg.metadata().getLong("startTime");
    }
    else {
        throw eckit::UserError{"Unable to find start time", Here()};
    }

    if (useDateTime() && msg.metadata().has("date")) {
        startDate_ = msg.metadata().getLong("date");
    }
    else if (!useDateTime() && msg.metadata().has("startDate")) {
        startDate_ = msg.metadata().getLong("startDate");
    }
    else {
        throw eckit::UserError{"Unable to find start date", Here()};
    }
    return;
};

bool StatisticsOptions::useDateTime() const {
    return useDateTime_;
};

long StatisticsOptions::stepFreq() const {
    return stepFreq_;
};
long StatisticsOptions::timeStep() const {
    return timeStep_;
};

long StatisticsOptions::startDate() const {
    return startDate_;
}

long StatisticsOptions::startTime() const {
    return startTime_;
}

}  // namespace action
}  // namespace multio
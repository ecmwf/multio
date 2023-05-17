#include "StatisticsOptions.h"

#include "eckit/exception/Exceptions.h"
#include "eckit/filesystem/PathName.h"

#include "multio/util/Substitution.h"

namespace multio::action {


StatisticsOptions::StatisticsOptions(const config::ComponentConfiguration& compConf) :
    useDateTime_{false},
    stepFreq_{1},
    timeStep_{3600},
    startDate_{0},
    startTime_{0},
    restart_{false},
    step_{-1},
    solverSendInitStep_{false},
    restartPath_{"."},
    restartPrefix_{"StatisticsRestartFile"} {

    if (!compConf.YAML().has("options")) {
        return;
    }

    const auto& opt = compConf.YAML().getSubConfiguration("options");

    // Overwrite defaults
    useDateTime_ = opt.getBool("use-current-time", false);
    stepFreq_ = opt.getLong("step-frequency", 1L);
    timeStep_ = opt.getLong("time-step", 3600L);
    solverSendInitStep_ = opt.getBool("initial-condition-present", false);
    std::optional<bool> r = util::parseBool(opt, "restart", false);
    if (r) {
        restart_ = *r;
    }
    else {
        throw eckit::SeriousBug{"Unable to restart value", Here()};
    }
    // TODO: Add functionality to automatically create restart path if it not exists
    // (same improvement can be done in sink). Feature already present in eckit::PathName
    if (opt.has("restart-path")) {
        restartPath_ = compConf.multioConfig().replaceCurly(opt.getString("restart-path", "."));
        eckit::PathName path{restartPath_};
        if (!path.exists() || !path.isDir()) {
            throw eckit::UserError{"restart path not exist", Here()};
        }
    }
    restartPrefix_ = compConf.multioConfig().replaceCurly(opt.getString("restart-prefix", "StatisticsDump"));

    return;
};

StatisticsOptions::StatisticsOptions(const StatisticsOptions& opt, const message::Message& msg) :
    useDateTime_{opt.useDateTime()},
    stepFreq_{opt.stepFreq()},
    timeStep_{opt.timeStep()},
    startDate_{0},
    startTime_{0},
    restart_{opt.restart()},
    step_{-1},
    solverSendInitStep_{opt.solver_send_initial_condition()},
    restartPath_{opt.restartPath()},
    restartPrefix_{opt.restartPrefix()} {

    if (useDateTime() && msg.metadata().has("time")) {
        startTime_ = msg.metadata().getLong("time");
    }
    else if (!useDateTime() && msg.metadata().has("startTime")) {
        startTime_ = msg.metadata().getLong("startTime");
    }
    else {
        throw eckit::SeriousBug{"Unable to find start time", Here()};
    }

    if (useDateTime() && msg.metadata().has("date")) {
        startDate_ = msg.metadata().getLong("date");
    }
    else if (!useDateTime() && msg.metadata().has("startDate")) {
        startDate_ = msg.metadata().getLong("startDate");
    }
    else {
        throw eckit::SeriousBug{"Unable to find start date", Here()};
    }

    // Step is here in case we need some hacks
    if (!msg.metadata().has("step")) {
        throw eckit::SeriousBug{"Step metadata not present", Here()};
    }
    step_ = msg.metadata().getLong("step");

    timeStep_ = msg.metadata().getLong("timeStep", timeStep_);
    stepFreq_ = msg.metadata().getLong("step-frequency", stepFreq_);

    return;
};

bool StatisticsOptions::restart() const {
    return ((step_ == 0 && solverSendInitStep_) || (step_ == 1 && solverSendInitStep_)) ? false : restart_;
};

const std::string& StatisticsOptions::restartPath() const {
    return restartPath_;
};

const std::string& StatisticsOptions::restartPrefix() const {
    return restartPrefix_;
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

long StatisticsOptions::step() const {
    return step_;
}

bool StatisticsOptions::solver_send_initial_condition() const {
    return solverSendInitStep_;
}

}  // namespace multio::action

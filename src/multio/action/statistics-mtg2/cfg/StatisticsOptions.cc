
#include "multio/action/statistics-mtg2/cfg/StatisticsOptions.h"

namespace multio::action::statistics_mtg2 {


StatisticsOptions::StatisticsOptions(const config::ComponentConfiguration& compConf) :
    stepFreq_{1},
    timeStep_{3600},
    solverSendInitStep_{false},
    readRestart_{false},
    writeRestart_{false},
    debugRestart_{false},
    useDateTime_{false},
    clientSideStatistics_{false},
    restartTime_{"latest"},  // 00000000-000000
    restartPath_{"."},
    restartPrefix_{"StatisticsRestartFile"},
    restartLib_{"fstream_io"},
    logPrefix_{"Plan"},
    windowType_{"forward-offset"},
    accumulatedFieldsResetFreqency_{"month"},
    valueCountThreshold_{},
    setMetadata_{} {
    // Dump usage
    if (compConf.parsedConfig().has("help")) {
        usage();
        throw eckit::UserError{"Usage requested", Here()};
    }


    // Read the options
    if (compConf.parsedConfig().has("options")) {
        const auto& options = compConf.parsedConfig().getSubConfiguration("options");
        parseUseDateTime(options);
        parseStepFrequency(options);
        parseTimeStep(options);
        parseInitialConditionPresent(options);
        parseWriteRestart(options);
        parseDebugRestart(options);
        parseClientSideStatistics(options);
        parseReadRestart(options);
        parseRestartPath(compConf, options);
        parseRestartPrefix(compConf, options);
        parseRestartLib(options);
        parseRestartTime(compConf, options);
        parseLogPrefix(compConf, options);
        parseWindowType(compConf, options);
        parseSolverResetAccumulatedFields(compConf, options);
        parseValueCountThreshold(compConf, options);
        parseSetMetadata(compConf, options);
    }


    // dump all the options
    dumpOptions();

    return;
};


void StatisticsOptions::parseUseDateTime(const eckit::LocalConfiguration& cfg) {
    // Distance in steps between two messages
    useDateTime_ = cfg.getLong("use-current-time", false);
    return;
};

void StatisticsOptions::parseStepFrequency(const eckit::LocalConfiguration& cfg) {
    // Distance in steps between two messages
    stepFreq_ = cfg.getLong("step-frequency", 1L);
    return;
};

void StatisticsOptions::parseTimeStep(const eckit::LocalConfiguration& cfg) {
    // How many seconds in a timestep
    timeStep_ = cfg.getLong("time-step", 3600L);
    return;
};

void StatisticsOptions::parseInitialConditionPresent(const eckit::LocalConfiguration& cfg) {
    // Used to determine if the solver emit the initial condition.
    // This is a relevant information for statistics computations.
    // At the moment ifs emit the initial condition and nemo not.
    // Default value is false so that nemo can work without options.
    solverSendInitStep_ = cfg.getBool("initial-condition-present", false);
    return;
};

void StatisticsOptions::parseWriteRestart(const eckit::LocalConfiguration& cfg) {
    // Used to determine if the simulation need to save/load
    // restart files.
    std::optional<bool> r;
    r = util::parseBool(cfg, "write-restart", false);
    if (r) {
        writeRestart_ = *r;
    }
    else {
        usage();
        throw eckit::SeriousBug{"Unable to read restart", Here()};
    }
    return;
};

void StatisticsOptions::parseDebugRestart(const eckit::LocalConfiguration& cfg) {
    // Used to determine if the simulation need to save/load
    // restart files.
    std::optional<bool> r;
    r = util::parseBool(cfg, "debug-restart", false);
    if (r) {
        debugRestart_ = *r;
    }
    else {
        usage();
        throw eckit::SeriousBug{"Unable to read restart", Here()};
    }
    return;
};


void StatisticsOptions::parseClientSideStatistics(const eckit::LocalConfiguration& cfg) {
    // Used to determine if the simulation need to save/load
    // restart files.
    std::optional<bool> r;
    r = util::parseBool(cfg, "is-client-side", false);
    if (r) {
        clientSideStatistics_ = *r;
    }
    else {
        usage();
        throw eckit::SeriousBug{"Unable to read client-side", Here()};
    }
    return;
};

void StatisticsOptions::parseReadRestart(const eckit::LocalConfiguration& cfg) {
    // Used to determine if the simulation need to save/load
    // restart files.
    std::optional<bool> r;
    r = util::parseBool(cfg, "read-restart", false);
    if (r) {
        readRestart_ = *r;
    }
    else {
        usage();
        throw eckit::SeriousBug{"Unable to read restart", Here()};
    }
    return;
};


void StatisticsOptions::parseRestartPath(const config::ComponentConfiguration& compConf,
                                         const eckit::LocalConfiguration& cfg) {
    // Read the path used to restart statistics
    // Default value is "."
    restartPath_ = cfg.getString("restart-path", ".");
    eckit::PathName path{restartPath_};
    if (!path.exists() || !path.isDir()) {
        std::ostringstream os;
        os << "Restart path does not exist :: " << restartPath_ << std::endl;
        throw eckit::UserError{os.str(), Here()};
    }
    return;
};


void StatisticsOptions::parseRestartTime(const config::ComponentConfiguration& compConf,
                                         const eckit::LocalConfiguration& cfg) {
    // Read the path used to restart statistics
    // Default value is "latest"
    restartTime_ = cfg.getString("restart-time", "latest");
    return;
};


void StatisticsOptions::parseRestartPrefix(const config::ComponentConfiguration& compConf,
                                           const eckit::LocalConfiguration& cfg) {
    // Prefix used for the restart file names in order
    // to make the file name unique across different plans
    restartPrefix_ = cfg.getString("restart-prefix", "StatisticsDump");
    return;
};

void StatisticsOptions::parseRestartLib(const eckit::LocalConfiguration& cfg) {
    restartLib_ = cfg.getString("restart-lib", "fstream_io");
    return;
};


void StatisticsOptions::parseLogPrefix(const config::ComponentConfiguration& compConf,
                                       const eckit::LocalConfiguration& cfg) {
    logPrefix_ = cfg.getString("log-prefix", "Plan");
    return;
};

void StatisticsOptions::parseWindowType(const config::ComponentConfiguration& compConf,
                                        const eckit::LocalConfiguration& cfg) {
    windowType_ = cfg.getString("window-type", "forward-offset");
    if (windowType_ != "forward-offset" && windowType_ != "backward-offset") {
        std::ostringstream os;
        os << "Invalid window type :: " << windowType_ << std::endl;
        throw eckit::UserError(os.str(), Here());
    }
    return;
};

void StatisticsOptions::parseSolverResetAccumulatedFields(const config::ComponentConfiguration& compConf,
                                                          const eckit::LocalConfiguration& cfg) {
    // Used in the deaccumulate action to not deaccumulate twice
    accumulatedFieldsResetFreqency_ = cfg.getString("solver-reset-accumulate-fields-every", "month");

    if (accumulatedFieldsResetFreqency_ != "hour" && accumulatedFieldsResetFreqency_ != "day"
        && accumulatedFieldsResetFreqency_ != "month" && accumulatedFieldsResetFreqency_ != "year"
        && accumulatedFieldsResetFreqency_ != "never") {
        std::ostringstream os;
        os << "Invalid reset period of accumulated fields :: " << accumulatedFieldsResetFreqency_ << std::endl;
        throw eckit::UserError(os.str(), Here());
    }
    return;
};

void StatisticsOptions::parseValueCountThreshold(const config::ComponentConfiguration& compConf,
                                                 const eckit::LocalConfiguration& cfg) {
    long threshold = cfg.getLong("value-count-threshold", -1);

    if (threshold == -1) {
        valueCountThreshold_ = std::nullopt;
        return;
    }
    if (threshold > 0) {
        valueCountThreshold_ = threshold;
        return;
    }

    std::ostringstream os;
    os << "Invalid value count threshold :: " << threshold << " (must be unset, -1 or positive value)" << std::endl;
    throw eckit::UserError(os.str(), Here());
}

void StatisticsOptions::parseSetMetadata(const config::ComponentConfiguration& compConf,
                                         const eckit::LocalConfiguration& cfg) {
    if (!cfg.has("set-metadata")) {
        return;
    }

    auto subCfg = cfg.getSubConfiguration("set-metadata");
    for (auto key : subCfg.keys()) {
        auto value = subCfg.getString(key);
        setMetadata_.emplace_back(std::pair<std::string, std::string>(key, value));
    }
}


const std::string& StatisticsOptions::logPrefix() const {
    return logPrefix_;
};


long StatisticsOptions::stepFreq() const {
    return stepFreq_;
};


long StatisticsOptions::timeStep() const {
    return timeStep_;
};


bool StatisticsOptions::solver_send_initial_condition() const {
    return solverSendInitStep_;
};

bool StatisticsOptions::readRestart() const {
    return readRestart_;
};


bool StatisticsOptions::writeRestart() const {
    return writeRestart_;
};


bool StatisticsOptions::debugRestart() const {
    return debugRestart_;
};

bool StatisticsOptions::clientSideStatistics() const {
    return clientSideStatistics_;
};


const std::string& StatisticsOptions::restartTime() const {
    return restartTime_;
};


const std::string& StatisticsOptions::restartPath() const {
    return restartPath_;
};


const std::string& StatisticsOptions::restartPrefix() const {
    return restartPrefix_;
};


const std::string& StatisticsOptions::windowType() const {
    return windowType_;
};


const std::string& StatisticsOptions::restartLib() const {
    return restartLib_;
};


const std::string& StatisticsOptions::solverResetAccumulatedFields() const {
    return accumulatedFieldsResetFreqency_;
};


std::optional<long> StatisticsOptions::valueCountThreshold() const {
    return valueCountThreshold_;
}


const std::vector<std::pair<std::string, std::string>>& StatisticsOptions::setMetadata() const {
    return setMetadata_;
}


void StatisticsOptions::dumpOptions() {
    // TODO: Implement this function
    return;
}


void StatisticsOptions::usage() {
    // TODO: Implement this function
    return;
}

}  // namespace multio::action::statistics_mtg2

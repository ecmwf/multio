
#include "multio/action/statistics/cfg/StatisticsOptions.h"

namespace multio::action {


StatisticsOptions::StatisticsOptions( const config::ComponentConfiguration& compConf ):
    dateKey_{"date"},
    timeKey_{"time"},
    paramKey_{"param"},
    levelKey_{"level"},
    levTypeKey_{"levtype"},
    gridTypeKey_{"gridType"},
    precisionKey_{"precision"},
    timeStepKey_{"timeStep"},
    stepFreqKey_{"step-frequency"},
    stepKey_{"step"},
    bitmapPresentKey_{"bitmapPresent"},
    missingValueKey_{"missingValue"},
    stepFreq_{1},
    timeStep_{3600},
    solverSendInitStep_{false},
    readRestart_{false},
    writeRestart_{false},
    debugRestart_{false},
    clientSideStatistics_{false},
    restartTime_{"latest"},//00000000-000000
    restartPath_{"."},
    restartPrefix_{"StatisticsRestartFile"},
    restartLib_{"fstream_io"},
    logPrefix_{"Plan"},
    windowType_{"forward-offset"},
    accumulatedFieldsResetFreqency_{"month"}
{
    // Dump usage
    if (compConf.parsedConfig().has("help")) {
        usage();
        throw eckit::UserError{"Usage requested", Here()};
    }


    // Read the metadata options
    if ( compConf.parsedConfig().has("metadata-options") ) {
        const auto& metadata_options = compConf.parsedConfig().getSubConfiguration("metadata-options");
        parseDateKey( metadata_options );
        parseTimeKey( metadata_options );
        parseParamKey( metadata_options );
        parseLevelKey( metadata_options );
        parseLevTypeKey( metadata_options );
        parseGridTypeKey( metadata_options );
        parsePrecisionKey( metadata_options );
        parseTimeStepKey( metadata_options );
        parseStepFreqKey( metadata_options );
        parseStepKey( metadata_options );
        parseBitmapPresentKey( metadata_options );
        parseMissingValueKey( metadata_options );
    }


    // Read the options
    if ( compConf.parsedConfig().has("options") ) {
        const auto& options = compConf.parsedConfig().getSubConfiguration("options");
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
    }


    // dump all the options
    dumpOptions();

    return;
};

void StatisticsOptions::parseDateKey(const eckit::LocalConfiguration& cfg) {
    dateKey_ = cfg.getString("date-key", "date");
    return;
};

void StatisticsOptions::parseTimeKey(const eckit::LocalConfiguration& cfg) {
    timeKey_ = cfg.getString("time-key", "date");
    return;
};

void StatisticsOptions::parseParamKey(const eckit::LocalConfiguration& cfg) {
    paramKey_ = cfg.getString("param-key", "param");
    return;
};

void StatisticsOptions::parseLevelKey(const eckit::LocalConfiguration& cfg) {
    levelKey_ = cfg.getString("level-key", "level");
    return;
};

void StatisticsOptions::parseLevTypeKey(const eckit::LocalConfiguration& cfg) {
    levTypeKey_ = cfg.getString("level-type-key", "levtype");
    return;
};

void StatisticsOptions::parseGridTypeKey(const eckit::LocalConfiguration& cfg) {
    gridTypeKey_ = cfg.getString("grid-type-key", "gridType");
    return;
};

void StatisticsOptions::parseTimeStepKey(const eckit::LocalConfiguration& cfg) {
    timeStepKey_ = cfg.getString("time-step-key", "timeStep");
    return;
};

void StatisticsOptions::parseStepFreqKey(const eckit::LocalConfiguration& cfg) {
    stepFreqKey_ = cfg.getString("step-frequency-key", "step-frequency");
    return;
};

void StatisticsOptions::parseStepKey(const eckit::LocalConfiguration& cfg) {
    stepKey_ = cfg.getString("step-key", "step");
    return;
};

void StatisticsOptions::parsePrecisionKey(const eckit::LocalConfiguration& cfg) {
    precisionKey_ = cfg.getString("precision-key", "precision");
    return;
};

void StatisticsOptions::parseBitmapPresentKey(const eckit::LocalConfiguration& cfg) {
    bitmapPresentKey_ = cfg.getString("bitmap-present-key", "bitmapPresent");
    return;
};

void StatisticsOptions::parseMissingValueKey(const eckit::LocalConfiguration& cfg) {
    missingValueKey_ = cfg.getString("missing-value-key", "missingValue");
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
    if (cfg.has("restart-path")) {
        restartPath_ = compConf.multioConfig().replaceCurly(cfg.getString("restart-path", "."));
        eckit::PathName path{restartPath_};
        if (!path.exists() || !path.isDir()) {
            std::ostringstream os;
            os << "Restart path does not exist :: " << restartPath_ << std::endl;
            throw eckit::UserError{ os.str(), Here()};
        }
    }
    return;
};


void StatisticsOptions::parseRestartTime(const config::ComponentConfiguration& compConf,
                                         const eckit::LocalConfiguration& cfg) {
    // Read the path used to restart statistics
    // Default value is "latest"
    if (cfg.has("restart-time")) {
        restartTime_ = compConf.multioConfig().replaceCurly(cfg.getString("restart-time", "latest"));
    }
    return;
};


void StatisticsOptions::parseRestartPrefix(const config::ComponentConfiguration& compConf,
                                                 const eckit::LocalConfiguration& cfg) {
    // Prefix used for the restart file names in order
    // to make the file name unique across different plans
    restartPrefix_ = compConf.multioConfig().replaceCurly(cfg.getString("restart-prefix", "StatisticsDump"));
    return;
};

void StatisticsOptions::parseRestartLib(const eckit::LocalConfiguration& cfg) {
    restartLib_ = cfg.getString("restart-lib", "fstream_io");
    return;
};


void StatisticsOptions::parseLogPrefix(const config::ComponentConfiguration& compConf, const eckit::LocalConfiguration& cfg) {
    logPrefix_ = cfg.getString("log-prefix", "Plan");
    return;
};

void StatisticsOptions::parseWindowType(const config::ComponentConfiguration& compConf, const eckit::LocalConfiguration& cfg) {
    windowType_ = cfg.getString("window-type", "forward-offset");
    if ( windowType_ != "forward-offset" && windowType_ != "backward-offset" ) {
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


const std::string& StatisticsOptions::logPrefix() const
{
    return logPrefix_;
};


const std::string& StatisticsOptions::dateKey() const
{
    return dateKey_;
};


const std::string& StatisticsOptions::timeKey() const
{
    return timeKey_;
};


const std::string& StatisticsOptions::paramKey() const
{
    return paramKey_;
 };


const std::string& StatisticsOptions::levelKey() const
{
    return levelKey_;
 };


const std::string& StatisticsOptions::levTypeKey() const
{
    return levTypeKey_;
};


const std::string& StatisticsOptions::gridTypeKey() const
{
    return gridTypeKey_;
};


const std::string& StatisticsOptions::precisionKey() const
{
    return precisionKey_;
};


const std::string& StatisticsOptions::timeStepKey() const
{
    return timeStepKey_;
};


const std::string& StatisticsOptions::stepFreqKey() const
{
    return stepFreqKey_;
};


const std::string& StatisticsOptions::stepKey() const
{
    return stepKey_;
};


const std::string& StatisticsOptions::bitmapPresentKey() const
{
    return bitmapPresentKey_;
};

const std::string& StatisticsOptions::missingValueKey() const
{
    return missingValueKey_;
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




void StatisticsOptions::dumpOptions() {
    return;
}


void StatisticsOptions::usage() {
    return;
}

}

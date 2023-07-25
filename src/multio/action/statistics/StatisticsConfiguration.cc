#include "StatisticsConfiguration.h"

#include <limits.h>
#include <unistd.h>
#include <iomanip>


#include "eckit/exception/Exceptions.h"
#include "eckit/filesystem/PathName.h"

#include "multio/LibMultio.h"
#include "multio/util/Substitution.h"

namespace multio::action {


StatisticsConfiguration::StatisticsConfiguration(const config::ComponentConfiguration& compConf) :
    useDateTime_{false},
    stepFreq_{1},
    timeStep_{3600},
    startDate_{0},
    startTime_{0},
    restart_{false},
    readRestart_{false},
    writeRestart_{false},
    step_{-1},
    restartStep_{-1},
    solverSendInitStep_{false},
    haveMissingValue_{false},
    missingValue_{9999.0},
    restartPath_{"."},
    restartPrefix_{"StatisticsRestartFile"},
    restartLib_{"fstream_io"},
    logPrefix_{"Plan"},
    accumulatedFieldsResetFreqency_{"month"} {

    if (compConf.parsedConfig().has("help")) {
        usage();
        throw eckit::SeriousBug{"Usage requested", Here()};
    }

    if (!compConf.parsedConfig().has("options")) {
        return;
    }

    const auto& cfg = compConf.parsedConfig().getSubConfiguration("options");

    parseUseDateTime(cfg);
    parseStepFrequency(cfg);
    parseTimeStep(cfg);
    parseInitialConditionPresent(cfg);
    parseRestartActivation(cfg);
    parseRestartPath(compConf, cfg);
    parseRestartPrefix(compConf, cfg);
    parseRestartLib(cfg);
    parseLogPrefix(compConf, cfg);
    parseSolverResetAccumulatedFields(compConf, cfg);

    return;
};

StatisticsConfiguration::StatisticsConfiguration(const StatisticsConfiguration& cfg, const message::Message& msg) :
    useDateTime_{cfg.useDateTime()},
    stepFreq_{cfg.stepFreq()},
    timeStep_{cfg.timeStep()},
    startDate_{0},
    startTime_{0},
    readRestart_{cfg.readRestart()},
    writeRestart_{cfg.writeRestart()},
    step_{-1},
    restartStep_{-1},
    solverSendInitStep_{cfg.solver_send_initial_condition()},
    haveMissingValue_{false},
    missingValue_{9999.0},
    restartPath_{cfg.restartPath()},
    restartPrefix_{cfg.restartPrefix()},
    restartLib_{cfg.restartLib()},
    logPrefix_{""},
    accumulatedFieldsResetFreqency_{cfg.solverResetAccumulatedFields()} {

    readStartTime(msg);
    readStartDate(msg);
    readStep(msg);
    readRestartStep(msg);
    readTimeStep(msg);
    readStepFrequency(msg);
    readMissingValue(msg);
    createLoggingPrefix(cfg, msg);

    dumpConfiguration();

    return;
};


void StatisticsConfiguration::parseUseDateTime(const eckit::LocalConfiguration& cfg) {
    // In nemo startDate and startTime are used, while in ifs
    // date and time are used with the same meaning.
    // If this flag is present and true, date/time are readed
    // otherwise startDate and startTime are readed.
    // Default value is false
    useDateTime_ = cfg.getBool("use-current-time", false);
    return;
};

void StatisticsConfiguration::parseStepFrequency(const eckit::LocalConfiguration& cfg) {
    // Distance in steps between two messages
    stepFreq_ = cfg.getLong("step-frequency", 1L);
    return;
};

void StatisticsConfiguration::parseTimeStep(const eckit::LocalConfiguration& cfg) {
    // How many seconds in a timestep
    timeStep_ = cfg.getLong("time-step", 3600L);
    return;
};

void StatisticsConfiguration::parseInitialConditionPresent(const eckit::LocalConfiguration& cfg) {
    // Used to determine if the solver emit the initial condition.
    // This is a relevant information for statistics computations.
    // At the moment ifs emit the initial condition and nemo not.
    // Default value is false so that nemo can work without options.
    solverSendInitStep_ = cfg.getBool("initial-condition-present", false);
    return;
};

void StatisticsConfiguration::parseRestartActivation(const eckit::LocalConfiguration& cfg) {
    // Used to determine if the simulation need to save/load
    // restart files.
    std::optional<bool> r;
    r = util::parseBool(cfg, "restart", false);
    if (r) {
        restart_ = *r;
        readRestart_ = *r;
        writeRestart_ = *r;
    }
    else {
        usage();
        throw eckit::SeriousBug{"Unable to read restart", Here()};
    }
    return;
};

void StatisticsConfiguration::parseRestartPath(const config::ComponentConfiguration& compConf,
                                               const eckit::LocalConfiguration& cfg) {
    // Read the path used to restart statistics
    // Default value is "."
    if (cfg.has("restart-path")) {
        restartPath_ = compConf.multioConfig().replaceCurly(cfg.getString("restart-path", "."));
        eckit::PathName path{restartPath_};
        if (!path.exists() || !path.isDir()) {
            throw eckit::UserError{"restart path not exist", Here()};
        }
    }
    return;
};


void StatisticsConfiguration::parseRestartPrefix(const config::ComponentConfiguration& compConf,
                                                 const eckit::LocalConfiguration& cfg) {
    // Prefix used for the restart file names in order
    // to make the file name unique across different plans
    restartPrefix_ = compConf.multioConfig().replaceCurly(cfg.getString("restart-prefix", "StatisticsDump"));
    return;
};

void StatisticsConfiguration::parseRestartLib(const eckit::LocalConfiguration& cfg) {
    restartLib_ = cfg.getString("restart-lib", "fstream_io");
    return;
};


void StatisticsConfiguration::parseSolverResetAccumulatedFields(const config::ComponentConfiguration& compConf,
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


void StatisticsConfiguration::parseLogPrefix(const config::ComponentConfiguration& compConf,
                                             const eckit::LocalConfiguration& cfg) {
    // Prefix used for logging. Pid and hostname are appended to the
    // log prefix in order to simplify the debug
    logPrefix_ = compConf.multioConfig().replaceCurly(cfg.getString("log-prefix", "Plan"));
    std::ostringstream os;
    os << logPrefix_ << ", pid=" << std::left << std::setw(10) << ::getpid();
    {
        char hostname[255];
        gethostname(hostname, 255);
        os << ", hostname=" << std::string{hostname} << ") ";
    }
    logPrefix_ = os.str();
    return;
};

void StatisticsConfiguration::readStartTime(const message::Message& msg) {
    if (useDateTime() && msg.metadata().has("time")) {
        startTime_ = msg.metadata().get<std::int64_t>("time");
    }
    else if (!useDateTime() && msg.metadata().has("startTime")) {
        startTime_ = msg.metadata().get<std::int64_t>("startTime");
    }
    else {
        throw eckit::SeriousBug{"Unable to find start time", Here()};
    }
    return;
};

void StatisticsConfiguration::readStartDate(const message::Message& msg) {
    if (useDateTime() && msg.metadata().has("date")) {
        startDate_ = msg.metadata().get<std::int64_t>("date");
    }
    else if (!useDateTime() && msg.metadata().has("startDate")) {
        startDate_ = msg.metadata().get<std::int64_t>("startDate");
    }
    else {
        throw eckit::SeriousBug{"Unable to find start date", Here()};
    }
    return;
};


void StatisticsConfiguration::readStep(const message::Message& msg) {
    if (!msg.metadata().has("step")) {
        throw eckit::SeriousBug{"Step metadata not present", Here()};
    }
    step_ = msg.metadata().get<std::int64_t>("step");
    return;
};

void StatisticsConfiguration::readRestartStep(const message::Message& msg) {
    // TODO: for restart statistics with nemo some special handling is needed
    restartStep_
        = msg.metadata().getOpt<std::int64_t>("restart-step").value_or(solverSendInitStep_ ? step_ : step_ - 1);
    return;
};

void StatisticsConfiguration::readTimeStep(const message::Message& msg) {
    timeStep_ = msg.metadata().getOpt<std::int64_t>("timeStep").value_or(timeStep_);
    return;
};

void StatisticsConfiguration::readStepFrequency(const message::Message& msg) {
    stepFreq_ = msg.metadata().getOpt<std::int64_t>("step-frequency").value_or(stepFreq_);
    return;
};


void StatisticsConfiguration::readMissingValue(const message::Message& msg) {
    if (msg.metadata().has("missingValue") && msg.metadata().has("bitmapPresent")
        && msg.metadata().get<bool>("bitmapPresent")) {
        haveMissingValue_ = true;
        missingValue_ = msg.metadata().get<double>("missingValue");
    }
    return;
};
void StatisticsConfiguration::createLoggingPrefix(const StatisticsConfiguration& cfg, const message::Message& msg) {
    std::ostringstream os;
    if (cfg.logPrefix() != "Plan") {
        os << "(prefix=" << cfg.logPrefix();
    }
    else if (cfg.restartPrefix() != "StatisticsRestartFile") {
        os << "(prefix=" << cfg.restartPrefix();
    }
    os << ", step=" << std::left << std::setw(6) << step_;
    if (msg.metadata().has("param")) {
        os << ", param=" << std::left << std::setw(10) << msg.metadata().get<std::string>("param");
    }
    else if (msg.metadata().has("paramId")) {
        os << ", param=" << std::left << std::setw(10) << msg.metadata().get<std::string>("paramId");
    }
    else {
        throw eckit::SeriousBug{"param/paramId metadata not present", Here()};
    }
    if (msg.metadata().has("level")) {
        os << ", level=" << std::left << std::setw(4) << msg.metadata().get<std::int64_t>("level");
    }
    else if (msg.metadata().has("levelist")) {
        os << ", level=" << std::left << std::setw(4) << msg.metadata().get<std::int64_t>("levelist");
    }
    if (msg.metadata().has("levtype")) {
        os << ", level-type=" << std::left << std::setw(5) << msg.metadata().get<std::string>("levtype");
    }
    logPrefix_ = os.str();
    return;
};


void StatisticsConfiguration::dumpConfiguration() {
    LOG_DEBUG_LIB(LibMultio) << " + useDateTime_                :: " << useDateTime_ << ";" << std::endl;
    LOG_DEBUG_LIB(LibMultio) << " + stepFreq_                   :: " << stepFreq_ << ";" << std::endl;
    LOG_DEBUG_LIB(LibMultio) << " + timeStep_                   :: " << timeStep_ << ";" << std::endl;
    LOG_DEBUG_LIB(LibMultio) << " + startDate_                  :: " << startDate_ << ";" << std::endl;
    LOG_DEBUG_LIB(LibMultio) << " + startTime_                  :: " << startTime_ << ";" << std::endl;
    LOG_DEBUG_LIB(LibMultio) << " + restart_                    :: " << restart_ << ";" << std::endl;
    LOG_DEBUG_LIB(LibMultio) << " + readRestart_                :: " << readRestart_ << ";" << std::endl;
    LOG_DEBUG_LIB(LibMultio) << " + writeRestart_               :: " << writeRestart_ << ";" << std::endl;
    LOG_DEBUG_LIB(LibMultio) << " + step_                       :: " << step_ << ";" << std::endl;
    LOG_DEBUG_LIB(LibMultio) << " + restartStep_                :: " << restartStep_ << ";" << std::endl;
    LOG_DEBUG_LIB(LibMultio) << " + solverSendInitStep_         :: " << solverSendInitStep_ << ";" << std::endl;
    LOG_DEBUG_LIB(LibMultio) << " + haveMissingValue_           :: " << haveMissingValue_ << ";" << std::endl;
    LOG_DEBUG_LIB(LibMultio) << " + missingValue_               :: " << missingValue_ << ";" << std::endl;
    LOG_DEBUG_LIB(LibMultio) << " + restartPath_                :: " << restartPath_ << ";" << std::endl;
    LOG_DEBUG_LIB(LibMultio) << " + restartPrefix_              :: " << restartPrefix_ << ";" << std::endl;
    LOG_DEBUG_LIB(LibMultio) << " + restartLib_                 :: " << restartLib_ << ";" << std::endl;
    LOG_DEBUG_LIB(LibMultio) << " + logPrefix_                  :: " << logPrefix_ << ";" << std::endl;
    LOG_DEBUG_LIB(LibMultio) << " + resetAccumulatedFieldsFreq_ :: " << accumulatedFieldsResetFreqency_ << ";"
                             << std::endl;
    return;
}


void StatisticsConfiguration::usage() {
    std::cout << "use-current-time          : "
              << "type=bool,   "
              << "default=false              : "
              << "use \"time\" or \"startTime\" from message metadata" << std::endl;
    std::cout << "step-frequency            : "
              << "type=int,    "
              << "default=1                  : "
              << "distance in number of steps between two messages" << std::endl;
    std::cout << "time-step                 : "
              << "type=int,    "
              << "default=3600               : "
              << "length in seconds of a step" << std::endl;
    std::cout << "initial-condition-present : "
              << "type=bool,   "
              << "default=false              : "
              << "true if the solver send the initial condition to multio" << std::endl;
    std::cout << "restart                   : "
              << "type=bool,   "
              << "default=false              : "
              << "if true restart file are generated and loaded" << std::endl;
    std::cout << "restart-path              : "
              << "type=string, "
              << "default=\".\"              : "
              << "path used for restart files" << std::endl;
    std::cout << "restart-prefix            : "
              << "type=string, "
              << "default=\"StatisticsDump\" : "
              << "prefix used to make the restart file names unique across plans" << std::endl;
    std::cout << "log-prefix                : "
              << "type=string, "
              << "default=\"Plan\"           : "
              << "Prefix used in loggin (useful in debug to identify the plans)" << std::endl;
    std::cout << "restart-library           : "
              << "type=string, "
              << "default=\"fstream_io\"     : "
              << "library used to write/read the restart files" << std::endl;
    std::cout << "solver-reset-accumulate-fields-every : "
              << "type=string, "
              << "default=\"month\"     : "
              << "When the solver reset the accumulated fields; used to avoid double deaccumulation" << std::endl;
    return;
}

bool StatisticsConfiguration::readRestart() const {
    return ((step_ == 0 && solverSendInitStep_) || (step_ == 1 && solverSendInitStep_)) ? false : readRestart_;
};

bool StatisticsConfiguration::writeRestart() const {
    return writeRestart_;
};

const std::string& StatisticsConfiguration::restartPath() const {
    return restartPath_;
};

const std::string& StatisticsConfiguration::restartPrefix() const {
    return restartPrefix_;
};

const std::string& StatisticsConfiguration::restartLib() const {
    return restartLib_;
};

const std::string& StatisticsConfiguration::logPrefix() const {
    return logPrefix_;
};

bool StatisticsConfiguration::useDateTime() const {
    return useDateTime_;
};

long StatisticsConfiguration::stepFreq() const {
    return stepFreq_;
};
long StatisticsConfiguration::timeStep() const {
    return timeStep_;
};

long StatisticsConfiguration::startDate() const {
    return startDate_;
}

long StatisticsConfiguration::startTime() const {
    return startTime_;
}

long StatisticsConfiguration::step() const {
    return step_;
}

long StatisticsConfiguration::restartStep() const {
    return restartStep_;
}

bool StatisticsConfiguration::solver_send_initial_condition() const {
    return solverSendInitStep_;
}

bool StatisticsConfiguration::haveMissingValue() const {
    return haveMissingValue_ != 0;
};

double StatisticsConfiguration::missingValue() const {
    return missingValue_;
};

const std::string& StatisticsConfiguration::solverResetAccumulatedFields() const {
    return accumulatedFieldsResetFreqency_;
};

}  // namespace multio::action

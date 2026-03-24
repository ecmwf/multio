#include "StatisticsOptions.h"

#include "eckit/exception/Exceptions.h"

#include "eckit/filesystem/PathName.h"
#include "multio/util/Substitution.h"

namespace multio::action::statistics_mtg2 {

bool parseInitialConditionPresent(const eckit::LocalConfiguration& cfg) {
    // Used to determine if the solver emit the initial condition.
    // This is a relevant information for statistics computations.
    // At the moment ifs emit the initial condition and nemo not.
    // Default value is false so that nemo can work without options.
    const auto r = util::parseBool(cfg, "initial-condition-present", false);
    if (r) {
        return *r;
    }
    throw eckit::SeriousBug{"Unable to read boolean initial-condition-present", Here()};
}

bool parseReadRestart(const eckit::LocalConfiguration& cfg) {
    // Used to determine if the simulation need to load restart files
    const auto r = util::parseBool(cfg, "read-restart", false);
    if (r) {
        return *r;
    }
    throw eckit::SeriousBug{"Unable to read boolean read-restart", Here()};
}

bool parseWriteRestart(const eckit::LocalConfiguration& cfg) {
    // Used to determine if the simulation need to save restart files
    const auto r = util::parseBool(cfg, "write-restart", false);
    if (r) {
        return *r;
    }
    throw eckit::SeriousBug{"Unable to read boolean write-restart", Here()};
}

bool parseDebugRestart(const eckit::LocalConfiguration& cfg) {
    const auto r = util::parseBool(cfg, "debug-restart", false);
    if (r) {
        return *r;
    }
    throw eckit::SeriousBug{"Unable to read boolean debug-restart", Here()};
}

std::string parseRestartTime(const eckit::LocalConfiguration& cfg) {
    // Used to determine which file to restart from
    return cfg.getString("restart-time", "latest");
}

std::string parseRestartPath(const eckit::LocalConfiguration& cfg) {
    // Path where restart files are loaded from / saved to
    const auto restartPath = cfg.getString("restart-path", ".");
    eckit::PathName path{restartPath};
    if (!path.exists() || !path.isDir()) {
        std::ostringstream os;
        os << "Restart path does not exist :: " << restartPath << std::endl;
        throw eckit::UserError{os.str(), Here()};
    }
    return restartPath;
}

std::string parseRestartPrefix(const eckit::LocalConfiguration& cfg) {
    // Prefix used for the restart file names in order
    // to make the file name unique across different plans
    return cfg.getString("restart-prefix", "StatisticsDump");
}

std::string parseRestartLib(const eckit::LocalConfiguration& cfg) {
    return cfg.getString("restart-lib", "fstream_io");
}

std::string parseLogPrefix(const eckit::LocalConfiguration& cfg) {
    return cfg.getString("log-prefix", "Plan");
}

WindowType parseWindowType(const eckit::LocalConfiguration& cfg) {
    const auto windowType = cfg.getString("window-type", "forward-offset");
    if (windowType == "forward-offset") {
        return WindowType::ForwardOffset;
    }
    if (windowType == "backward-offset") {
        return WindowType::BackwardOffset;
    }

    std::ostringstream os;
    os << "Invalid window type :: " << windowType << std::endl;
    throw eckit::UserError(os.str(), Here());
}

std::optional<std::int64_t> parseValueCountThreshold(const eckit::LocalConfiguration& cfg) {
    const auto threshold = cfg.getLong("value-count-threshold", -1);

    if (threshold == -1) {
        return std::nullopt;
    }
    if (threshold > 0) {
        return std::optional{threshold};
    }

    std::ostringstream os;
    os << "Invalid value count threshold :: " << threshold << " (must be unset, -1 or positive value)" << std::endl;
    throw eckit::UserError(os.str(), Here());
}

bool parseDisableStrictMapping(const eckit::LocalConfiguration& cfg) {
    const auto r = util::parseBool(cfg, "disable-strict-mapping", false);
    if (r) {
        return *r;
    }
    throw eckit::SeriousBug{"Unable to read boolean disable-strict-mapping", Here()};
}

bool parseDisableSquashing(const eckit::LocalConfiguration& cfg) {
    const auto r = util::parseBool(cfg, "disable-squashing", false);
    if (r) {
        return *r;
    }
    throw eckit::SeriousBug{"Unable to read boolean disable-squashing", Here()};
}

std::optional<OutputTimeReference> parseOutputTimeRef(const eckit::LocalConfiguration& cfg) {
    const auto outputTimeRef = cfg.getString("output-time-reference", "");
    if (outputTimeRef.empty()) {
        return {};
    }
    if (outputTimeRef == "start-of-window") {
        return OutputTimeReference::StartOfWindow;
    }
    if (outputTimeRef == "start-of-forecast") {
        return OutputTimeReference::StartOfForecast;
    }

    std::ostringstream os;
    os << "Invalid output time reference :: " << outputTimeRef << std::endl;
    throw eckit::UserError(os.str(), Here());
}

std::vector<std::pair<std::string, std::string>> parseSetMetadata(const eckit::LocalConfiguration& cfg) {
    if (!cfg.has("set-metadata")) {
        return {};
    }

    auto subCfg = cfg.getSubConfiguration("set-metadata");
    std::vector<std::pair<std::string, std::string>> res;
    for (auto key : subCfg.keys()) {
        auto value = subCfg.getString(key);
        res.emplace_back(std::pair<std::string, std::string>(key, value));
    }
    return res;
}


StatisticsOptions::StatisticsOptions(const eckit::LocalConfiguration& cfg) :
    initialConditionPresent_{parseInitialConditionPresent(cfg)},
    readRestart_{parseReadRestart(cfg)},
    writeRestart_{parseWriteRestart(cfg)},
    debugRestart_{parseDebugRestart(cfg)},
    restartTime_{parseRestartTime(cfg)},  // 00000000-000000
    restartPath_{parseRestartPath(cfg)},
    restartPrefix_{parseRestartPrefix(cfg)},
    restartLib_{parseRestartLib(cfg)},
    logPrefix_{parseRestartPrefix(cfg)},
    windowType_{parseWindowType(cfg)},
    valueCountThreshold_{parseValueCountThreshold(cfg)},
    disableStrictMapping_{parseDisableStrictMapping(cfg)},
    disableSquashing_{parseDisableSquashing(cfg)},
    setMetadata_{parseSetMetadata(cfg)},
    outputTimeReference_{parseOutputTimeRef(cfg)} {}


bool StatisticsOptions::initialConditionPresent() const {
    return initialConditionPresent_;
}

bool StatisticsOptions::readRestart() const {
    return readRestart_;
}
bool StatisticsOptions::writeRestart() const {
    return writeRestart_;
}
bool StatisticsOptions::debugRestart() const {
    return debugRestart_;
}
const std::string& StatisticsOptions::restartTime() const {
    return restartTime_;
}
const std::string& StatisticsOptions::restartPath() const {
    return restartPath_;
}
const std::string& StatisticsOptions::restartPrefix() const {
    return restartPrefix_;
}
const std::string& StatisticsOptions::restartLib() const {
    return restartLib_;
}

const std::string& StatisticsOptions::logPrefix() const {
    return logPrefix_;
}
WindowType StatisticsOptions::windowType() const {
    return windowType_;
}

std::optional<std::int64_t> StatisticsOptions::valueCountThreshold() const {
    return valueCountThreshold_;
}

bool StatisticsOptions::disableStrictMapping() const {
    return disableStrictMapping_;
}
bool StatisticsOptions::disableSquashing() const {
    return disableSquashing_;
}
const std::vector<std::pair<std::string, std::string>>& StatisticsOptions::setMetadata() const {
    return setMetadata_;
}

std::optional<OutputTimeReference> StatisticsOptions::outputTimeReference() const {
    return outputTimeReference_;
}

}  // namespace multio::action::statistics_mtg2

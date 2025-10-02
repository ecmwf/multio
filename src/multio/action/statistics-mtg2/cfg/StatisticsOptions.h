#pragma once

#include <string>

#include "eckit/config/LocalConfiguration.h"
#include "multio/action/Action.h"
#include "multio/config/ComponentConfiguration.h"
#include "multio/message/Message.h"


namespace multio::action::statistics_mtg2 {

/*
 * This class handle all the statistics configurations under the option keyword in the yaml
 */
class StatisticsOptions {
private:
    // Default values for configurations
    long timeStep_;
    bool solverSendInitStep_;
    bool readRestart_;
    bool writeRestart_;
    bool debugRestart_;
    std::string restartTime_;

    std::string restartPath_;
    std::string restartPrefix_;
    std::string restartLib_;
    std::string logPrefix_;
    std::string windowType_;
    std::string accumulatedFieldsResetFreqency_;

    std::optional<long> valueCountThreshold_;

    bool disableStrictMapping_;
    bool disableSquashing_;
    std::vector<std::pair<std::string, std::string>> setMetadata_;

private:
    void parseTimeStep(const eckit::LocalConfiguration& cfg);
    void parseInitialConditionPresent(const eckit::LocalConfiguration& cfg);
    void parseWriteRestart(const eckit::LocalConfiguration& cfg);
    void parseDebugRestart(const eckit::LocalConfiguration& cfg);
    void parseReadRestart(const eckit::LocalConfiguration& cfg);
    void parseRestartPath(const eckit::LocalConfiguration& cfg);
    void parseRestartPrefix(const eckit::LocalConfiguration& cfg);
    void parseRestartTime(const eckit::LocalConfiguration& cfg);
    void parseRestartLib(const eckit::LocalConfiguration& cfg);
    void parseLogPrefix(const eckit::LocalConfiguration& cfg);
    void parseWindowType(const eckit::LocalConfiguration& cfg);
    void parseSolverResetAccumulatedFields(const eckit::LocalConfiguration& cfg);
    void parseValueCountThreshold(const eckit::LocalConfiguration& cfg);
    void parseDisableStrictMapping(const eckit::LocalConfiguration& cfg);
    void parseDisableSquashing(const eckit::LocalConfiguration& cfg);
    void parseSetMetadata(const eckit::LocalConfiguration& cfg);

    void dumpOptions();
    void usage();

public:
    StatisticsOptions(const config::ComponentConfiguration& compConf);

    const std::string& logPrefix() const;
    const std::string& windowType() const;

    // Default values
    long timeStep() const;
    bool solver_send_initial_condition() const;
    bool readRestart() const;
    bool writeRestart() const;
    bool debugRestart() const;

    const std::string& restartTime() const;
    const std::string& restartPath() const;
    const std::string& restartPrefix() const;
    const std::string& restartLib() const;
    const std::string& solverResetAccumulatedFields() const;

    std::optional<long> valueCountThreshold() const;

    bool disableStrictMapping() const;
    bool disableSquashing() const;
    const std::vector<std::pair<std::string, std::string>>& setMetadata() const;
};

}  // namespace multio::action::statistics_mtg2

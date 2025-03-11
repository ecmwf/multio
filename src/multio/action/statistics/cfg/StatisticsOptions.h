#pragma once

#include <string>

#include "eckit/config/LocalConfiguration.h"
#include "multio/action/Action.h"
#include "multio/config/ComponentConfiguration.h"
#include "multio/message/Message.h"


namespace multio::action {

/*
 * This class handle all the statistics configurations under the option keyword in the yaml
 */
class StatisticsOptions {
private:
    // Default values for configurations
    long stepFreq_;
    long timeStep_;
    bool solverSendInitStep_;
    bool initDataPassTrough_;
    bool readRestart_;
    bool writeRestart_;
    bool debugRestart_;
    bool useDateTime_;
    bool clientSideStatistics_;
    std::string restartTime_;

    std::string restartPath_;
    std::string restartPrefix_;
    std::string restartLib_;
    std::string logPrefix_;
    std::string windowType_;
    std::string accumulatedFieldsResetFreqency_;

    std::optional<long> valueCountThreshold_;

private:
    void parseUseDateTime(const eckit::LocalConfiguration& cfg);
    void parseCheckMissingValues(const eckit::LocalConfiguration& cfg);
    void parseStepFrequency(const eckit::LocalConfiguration& cfg);
    void parseTimeStep(const eckit::LocalConfiguration& cfg);
    void parseInitialConditionPresent(const eckit::LocalConfiguration& cfg);
    void parseInitialDataPassThrough(const eckit::LocalConfiguration& cfg);
    void parseWriteRestart(const eckit::LocalConfiguration& cfg);
    void parseDebugRestart(const eckit::LocalConfiguration& cfg);
    void parseClientSideStatistics(const eckit::LocalConfiguration& cfg);
    void parseReadRestart(const eckit::LocalConfiguration& cfg);
    void parseRestartPath(const config::ComponentConfiguration& compConf, const eckit::LocalConfiguration& cfg);
    void parseRestartPrefix(const config::ComponentConfiguration& compConf, const eckit::LocalConfiguration& cfg);
    void parseRestartTime(const config::ComponentConfiguration& compConf, const eckit::LocalConfiguration& cfg);
    void parseRestartLib(const eckit::LocalConfiguration& cfg);
    void parseLogPrefix(const config::ComponentConfiguration& compConf, const eckit::LocalConfiguration& cfg);
    void parseWindowType(const config::ComponentConfiguration& compConf, const eckit::LocalConfiguration& cfg);
    void parseSolverResetAccumulatedFields(const config::ComponentConfiguration& compConf,
                                           const eckit::LocalConfiguration& cfg);
    void parseValueCountThreshold(const config::ComponentConfiguration& compConf, const eckit::LocalConfiguration& cfg);

    void dumpOptions();
    void usage();

public:
    StatisticsOptions(const config::ComponentConfiguration& compConf);

    const std::string& logPrefix() const;
    const std::string& windowType() const;
    bool useDateTime() const { return useDateTime_; };

    // Handle missing value
    const std::string& bitmapPresentKey() const;
    const std::string& missingValueKey() const;

    // Default values
    long stepFreq() const;
    long timeStep() const;
    bool solver_send_initial_condition() const;
    bool readRestart() const;
    bool writeRestart() const;
    bool debugRestart() const;
    bool clientSideStatistics() const;
    bool initialDataPassThrough() const;

    const std::string& restartTime() const;
    const std::string& restartPath() const;
    const std::string& restartPrefix() const;
    const std::string& restartLib() const;
    const std::string& solverResetAccumulatedFields() const;

    std::optional<long> valueCountThreshold() const;
};

}  // namespace multio::action

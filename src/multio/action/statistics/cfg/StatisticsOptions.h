#pragma once

#include <string>

#include "eckit/config/LocalConfiguration.h"
#include "multio/config/ComponentConfiguration.h"
#include "multio/message/Message.h"
#include "multio/action/Action.h"


namespace multio::action {

/*
 * This class handle all the statistics configurations under the option keyword in the yaml
 */
class StatisticsOptions{
private:

    // Metadata keys
    std::string dateKey_;
    std::string timeKey_;
    std::string paramKey_;
    std::string levelKey_;
    std::string levTypeKey_;
    std::string gridTypeKey_;
    std::string precisionKey_;
    std::string timeStepKey_;
    std::string stepFreqKey_;
    std::string stepKey_;
    std::string bitmapPresentKey_;
    std::string missingValueKey_;
    std::string accumulatedFieldsResetFreqency_;

    // Default values for configurations
    long stepFreq_;
    long timeStep_;
    bool solverSendInitStep_;
    bool readRestart_;
    bool writeRestart_;
    std::string restartTime_;

    std::string restartPath_;
    std::string restartPrefix_;
    std::string restartLib_;
    std::string logPrefix_;

private:

    // Configure the metadata
    void parseDateKey(const eckit::LocalConfiguration& cfg);
    void parseTimeKey(const eckit::LocalConfiguration& cfg);
    void parseParamKey(const eckit::LocalConfiguration& cfg);
    void parseLevelKey(const eckit::LocalConfiguration& cfg);
    void parseLevTypeKey(const eckit::LocalConfiguration& cfg);
    void parseGridTypeKey(const eckit::LocalConfiguration& cfg);
    void parsePrecisionKey(const eckit::LocalConfiguration& cfg);
    void parseTimeStepKey(const eckit::LocalConfiguration& cfg);
    void parseStepFreqKey(const eckit::LocalConfiguration& cfg);
    void parseStepKey(const eckit::LocalConfiguration& cfg);
    void parseBitmapPresentKey(const eckit::LocalConfiguration& cfg);
    void parseMissingValueKey(const eckit::LocalConfiguration& cfg);

    void parseCheckMissingValues(const eckit::LocalConfiguration& cfg);
    void parseStepFrequency(const eckit::LocalConfiguration& cfg);
    void parseTimeStep(const eckit::LocalConfiguration& cfg);
    void parseInitialConditionPresent(const eckit::LocalConfiguration& cfg);
    void parseWriteRestart(const eckit::LocalConfiguration& cfg);
    void parseReadRestart(const eckit::LocalConfiguration& cfg);
    void parseRestartPath(const config::ComponentConfiguration& compConf, const eckit::LocalConfiguration& cfg);
    void parseRestartPrefix(const config::ComponentConfiguration& compConf, const eckit::LocalConfiguration& cfg);
    void parseRestartTime(const config::ComponentConfiguration& compConf, const eckit::LocalConfiguration& cfg);
    void parseRestartLib(const eckit::LocalConfiguration& cfg);
    void parseLogPrefix(const config::ComponentConfiguration& compConf, const eckit::LocalConfiguration& cfg);
    void parseSolverResetAccumulatedFields(const config::ComponentConfiguration& compConf,
                                           const eckit::LocalConfiguration& cfg);

    void dumpOptions();
    void usage();

public:

    StatisticsOptions( const config::ComponentConfiguration& compConf );

    // Metadata keys
    const std::string& dateKey() const;
    const std::string& timeKey()  const;
    const std::string& paramKey() const;
    const std::string& levelKey() const;
    const std::string& levTypeKey() const;
    const std::string& gridTypeKey() const;
    const std::string& precisionKey() const;
    const std::string& timeStepKey() const;
    const std::string& stepFreqKey() const;
    const std::string& stepKey() const;
    const std::string& logPrefix() const;

    // Handle missing value
    const std::string& bitmapPresentKey() const;
    const std::string& missingValueKey() const;

    // Default values
    long stepFreq() const;
    long timeStep() const;
    bool solver_send_initial_condition() const;
    bool readRestart() const;
    bool writeRestart() const;

    const std::string& restartTime() const;
    const std::string& restartPath() const;
    const std::string& restartPrefix() const;
    const std::string& restartLib() const;
    const std::string& solverResetAccumulatedFields() const;

};

}

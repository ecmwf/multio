#pragma once

#include <string>

#include "eckit/config/LocalConfiguration.h"
#include "multio/config/ComponentConfiguration.h"
#include "multio/message/Message.h"


namespace multio::action {


class StatisticsConfiguration {

private:
    bool useDateTime_;
    long stepFreq_;
    long timeStep_;
    long startDate_;
    long startTime_;
    bool restart_;
    bool readRestart_;
    bool writeRestart_;
    long step_;
    long restartStep_;
    bool solverSendInitStep_;

    int haveMissingValue_;
    double missingValue_;

    std::string restartPath_;
    std::string restartPrefix_;
    std::string restartLib_;
    std::string logPrefix_;

public:
    StatisticsConfiguration(const config::ComponentConfiguration& compConf);
    StatisticsConfiguration(const StatisticsConfiguration& cfg, const message::Message& msg);

    bool useDateTime() const;
    long stepFreq() const;
    long timeStep() const;
    long startDate() const;
    long startTime() const;
    bool writeRestart() const;
    bool readRestart() const;
    long step() const;
    long restartStep() const;
    bool solver_send_initial_condition() const;
    const std::string& restartPath() const;
    const std::string& restartPrefix() const;
    const std::string& restartLib() const;
    const std::string& logPrefix() const;

    bool haveMissingValue() const;
    double missingValue() const;

private:
    void parseUseDateTime(const eckit::LocalConfiguration& cfg);
    void parseStepFrequency(const eckit::LocalConfiguration& cfg);
    void parseTimeStep(const eckit::LocalConfiguration& cfg);
    void parseInitialConditionPresent(const eckit::LocalConfiguration& cfg);
    void parseRestartActivation(const eckit::LocalConfiguration& cfg);
    void parseRestartPath(const config::ComponentConfiguration& compConf, const eckit::LocalConfiguration& cfg);
    void parseRestartPrefix(const config::ComponentConfiguration& compConf, const eckit::LocalConfiguration& cfg);
    void parseRestartLib(const eckit::LocalConfiguration& cfg);
    void parseLogPrefix(const config::ComponentConfiguration& compConf, const eckit::LocalConfiguration& cfg);


    void readStartTime(const message::Message& msg);
    void readStartDate(const message::Message& msg);
    void readStep(const message::Message& msg);
    void readRestartStep(const message::Message& msg);
    void readTimeStep(const message::Message& msg);
    void readStepFrequency(const message::Message& msg);
    void readMissingValue(const message::Message& msg);
    void createLoggingPrefix(const StatisticsConfiguration& cfg, const message::Message& msg);

    void dumpConfiguration();
    void usage();
};

}  // namespace multio::action

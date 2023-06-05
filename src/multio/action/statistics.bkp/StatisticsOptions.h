#pragma once

#include <string>

#include "eckit/config/LocalConfiguration.h"
#include "multio/config/ComponentConfiguration.h"
#include "multio/message/Message.h"

namespace multio::action {


class StatisticsOptions {

private:
    bool useDateTime_;
    long stepFreq_;
    long timeStep_;
    long startDate_;
    long startTime_;
    bool restart_;
    long step_;
    bool solverSendInitStep_;

    std::string restartPath_;
    std::string restartPrefix_;

public:
    StatisticsOptions(const config::ComponentConfiguration& compConf);
    StatisticsOptions(const StatisticsOptions& statOpts, const message::Message& msg);


    bool useDateTime() const;
    long stepFreq() const;
    long timeStep() const;
    long startDate() const;
    long startTime() const;
    bool restart() const;
    long step() const;
    bool solver_send_initial_condition() const;
    const std::string& restartPath() const;
    const std::string& restartPrefix() const;
};

}  // namespace multio::action

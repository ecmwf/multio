#pragma once

#include <string>

#include "eckit/config/LocalConfiguration.h"
#include "multio/message/Message.h"

namespace multio {
namespace action {


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
    StatisticsOptions(const eckit::LocalConfiguration& confCtx);
    StatisticsOptions(const StatisticsOptions& confCtx, const message::Message& msg);


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

}  // namespace action
}  // namespace multio

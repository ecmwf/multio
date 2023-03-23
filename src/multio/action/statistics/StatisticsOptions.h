#pragma once

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

public:
    StatisticsOptions(const eckit::LocalConfiguration& confCtx);
    StatisticsOptions(const StatisticsOptions& confCtx, const message::Message& msg);

    bool useDateTime() const;
    long stepFreq() const;
    long timeStep() const;
    long startDate() const;
    long startTime() const;
};

}  // namespace action
}  // namespace multio

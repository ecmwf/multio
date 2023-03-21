#pragma once

#include "eckit/config/LocalConfiguration.h"

namespace multio {
namespace action {


class StatisticsOptions {

private:
    bool useDateTimeCfg_;
    long stepFreqCfg_;
    long timeStepCfg_;

public:
    StatisticsOptions(const eckit::LocalConfiguration& confCtx);

    bool useDateTime() const;
    long stepFreq() const;
    long timeStep() const;
};

}  // namespace action
}  // namespace multio
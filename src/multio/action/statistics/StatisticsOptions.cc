#include "StatisticsOptions.h"
namespace multio {
namespace action {

StatisticsOptions::StatisticsOptions(const eckit::LocalConfiguration& confCtx) :
    useDateTimeCfg_{false}, stepFreqCfg_{1}, timeStepCfg_{3600} {

    if (!confCtx.has("options")) {
        return;
    }

    const auto& opt = confCtx.getSubConfiguration("options");

    useDateTimeCfg_ = opt.getBool("use-current-time", false);
    stepFreqCfg_ = opt.getLong("step-frequency", 1L);
    timeStepCfg_ = opt.getLong("time-step", 3600L);

    return;
};

bool StatisticsOptions::useDateTime() const {
    return useDateTimeCfg_;
};

long StatisticsOptions::stepFreq() const {
    return stepFreqCfg_;
};
long StatisticsOptions::timeStep() const {
    return timeStepCfg_;
};

}  // namespace action
}  // namespace multio
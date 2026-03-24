#include "TimeUtils.h"

namespace multio::action::statistics_mtg2 {

eckit::DateTime epochDateTime(const message::Message& msg, const StatisticsConfiguration& cfg) {
    eckit::Date startDate{cfg.date()};
    long startTime = cfg.time();
    auto hour = startTime / 10000;
    auto minute = (startTime % 10000) / 100;
    return eckit::DateTime{startDate, eckit::Time{hour, minute, 0}};
}


eckit::DateTime currentDateTime(const message::Message& msg, const StatisticsConfiguration& cfg) {
    return epochDateTime(msg, cfg) + static_cast<eckit::Second>(cfg.step() * cfg.timeIncrementInSeconds());
}


}  // namespace multio::action::statistics_mtg2

#include "TimeUtils.h"

namespace multio::action::statistics_mtg2 {

eckit::DateTime dateTime(std::int64_t date, std::int64_t time, std::int64_t stepInSeconds) {
    eckit::Date startDate{date};
    auto hour = time / 10000;
    auto minute = (time % 10000) / 100;
    return eckit::DateTime{startDate, eckit::Time{hour, minute, 0}} + static_cast<eckit::Second>(stepInSeconds);
}

eckit::DateTime epochDateTime(const message::Message& msg, const StatisticsConfiguration& cfg) {
    return dateTime(cfg.date(), cfg.time(), 0);
}


eckit::DateTime currentDateTime(const message::Message& msg, const StatisticsConfiguration& cfg) {
    return cfg.curr();
}


}  // namespace multio::action::statistics_mtg2

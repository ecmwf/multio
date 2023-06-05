#include "TimeUtils.h"

namespace multio::action {

eckit::DateTime epochDateTime(const message::Message& msg, const StatisticsConfiguration& cfg) {
    eckit::Date startDate{cfg.startDate()};
    long startTime = cfg.startTime();
    auto hour = startTime / 10000;
    auto minute = (startTime % 10000) / 100;
    return eckit::DateTime{startDate, eckit::Time{hour, minute, 0}};
}


eckit::DateTime prevDateTime(const message::Message& msg, const StatisticsConfiguration& cfg) {
    return epochDateTime(msg, cfg) + static_cast<eckit::Second>(std::max((cfg.step() - 1L), 0L) * cfg.timeStep());
}


eckit::DateTime currentDateTime(const message::Message& msg, const StatisticsConfiguration& cfg) {
    return epochDateTime(msg, cfg) + static_cast<eckit::Second>(cfg.step() * cfg.timeStep());
}

eckit::DateTime nextDateTime(const message::Message& msg, const StatisticsConfiguration& cfg) {
    return epochDateTime(msg, cfg) + static_cast<eckit::Second>((cfg.step() + 1) * cfg.timeStep());
}

eckit::DateTime winStartDateTime(const message::Message& msg, const StatisticsConfiguration& cfg) {
    return cfg.solver_send_initial_condition() ? currentDateTime(msg, cfg) : prevDateTime(msg, cfg);
}

}  // namespace multio::action
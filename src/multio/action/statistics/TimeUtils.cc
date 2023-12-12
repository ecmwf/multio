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


bool isBeginningOfYear(const message::Message& msg, const StatisticsConfiguration& cfg) {

    // Get the current time
    eckit::DateTime now = currentDateTime(msg, cfg);

    long month = now.date().month();
    long day = now.date().day();
    long hour = now.time().hours();
    long min = now.time().minutes();
    long sec = now.time().seconds();

    return month == 1 && day == 1 && hour == 0 && min == 0 && sec == 0;
}


bool isBeginningOfMonth(const message::Message& msg, const StatisticsConfiguration& cfg) {

    // Get the current time
    eckit::DateTime now = currentDateTime(msg, cfg);


    long day = now.date().day();
    long hour = now.time().hours();
    long min = now.time().minutes();
    long sec = now.time().seconds();

    return day == 1 && hour == 0 && min == 0 && sec == 0;
}


bool isBeginningOfDay(const message::Message& msg, const StatisticsConfiguration& cfg) {

    // Get the current time
    eckit::DateTime now = currentDateTime(msg, cfg);

    long hour = now.time().hours();
    long min = now.time().minutes();
    long sec = now.time().seconds();

    return hour == 0 && min == 0 && sec == 0;
}


bool isBeginningOfHour(const message::Message& msg, const StatisticsConfiguration& cfg) {

    // Get the current time
    eckit::DateTime now = currentDateTime(msg, cfg);

    long min = now.time().minutes();
    long sec = now.time().seconds();

    return min == 0 && sec == 0;
}


}  // namespace multio::action
#pragma once

#include "eckit/types/DateTime.h"
#include "multio/action/statistics/cfg/StatisticsConfiguration.h"
#include "multio/message/Message.h"


namespace multio::action {

eckit::DateTime epochDateTime(const message::Message& msg, const StatisticsConfiguration& cfg);
eckit::DateTime prevDateTime(const message::Message& msg, const StatisticsConfiguration& cfg);
eckit::DateTime currentDateTime(const message::Message& msg, const StatisticsConfiguration& cfg);
eckit::DateTime nextDateTime(const message::Message& msg, const StatisticsConfiguration& cfg);
eckit::DateTime winStartDateTime(const message::Message& msg, const StatisticsConfiguration& cfg);


bool isBeginningOfYear(const message::Message& msg, const StatisticsConfiguration& cfg);
bool isBeginningOfMonth(const message::Message& msg, const StatisticsConfiguration& cfg);
bool isBeginningOfDay(const message::Message& msg, const StatisticsConfiguration& cfg);
bool isBeginningOfHour(const message::Message& msg, const StatisticsConfiguration& cfg);

}  // namespace multio::action
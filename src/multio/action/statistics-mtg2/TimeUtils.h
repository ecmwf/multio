#pragma once

#include <cstdint>

#include "eckit/types/DateTime.h"
#include "multio/action/statistics-mtg2/cfg/StatisticsConfiguration.h"
#include "multio/message/Message.h"


namespace multio::action::statistics_mtg2 {

eckit::DateTime dateTime(std::int64_t date, std::int64_t time, std::int64_t stepInSeconds);
eckit::DateTime epochDateTime(const message::Message& msg, const StatisticsConfiguration& cfg);
eckit::DateTime currentDateTime(const message::Message& msg, const StatisticsConfiguration& cfg);

}  // namespace multio::action::statistics_mtg2

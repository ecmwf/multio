
#pragma once

#include "multio/action/statistics/cfg/StatisticsConfiguration.h"

#include "multio/action/statistics/period-updaters/PeriodUpdater.h"
#include "multio/action/statistics/period-updaters/HourPeriodUpdater.h"
#include "multio/action/statistics/period-updaters/DayPeriodUpdater.h"
#include "multio/action/statistics/period-updaters/MonthPeriodUpdater.h"

#include "multio/action/statistics/TimeUtils.h"
#include "eckit/types/DateTime.h"
#include "multio/message/Message.h"

#include "OperationWindow.h"

namespace multio::action {

std::unique_ptr<PeriodUpdater> make_period_updater(std::string const& output_freq, const StatisticsConfiguration& cfg );
std::unique_ptr<PeriodUpdater> load_period_updater(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsOptions& opt );

}  // namespace multio::action

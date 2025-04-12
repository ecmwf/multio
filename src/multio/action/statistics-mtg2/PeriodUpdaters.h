
#pragma once

#include "multio/action/statistics-mtg2/cfg/StatisticsConfiguration.h"

#include "multio/action/statistics-mtg2/period-updaters/DayPeriodUpdater.h"
#include "multio/action/statistics-mtg2/period-updaters/HourPeriodUpdater.h"
#include "multio/action/statistics-mtg2/period-updaters/MonthPeriodUpdater.h"
#include "multio/action/statistics-mtg2/period-updaters/PeriodUpdater.h"

#include "eckit/types/DateTime.h"
#include "multio/action/statistics-mtg2/TimeUtils.h"
#include "multio/message/Message.h"

#include "OperationWindow.h"

namespace multio::action::statistics_mtg2 {

std::unique_ptr<PeriodUpdater> make_period_updater(std::string const& output_freq, const StatisticsConfiguration& cfg);
std::unique_ptr<PeriodUpdater> load_period_updater(std::shared_ptr<StatisticsIO>& IOmanager,
                                                   const StatisticsOptions& opt);

}  // namespace multio::action::statistics_mtg2

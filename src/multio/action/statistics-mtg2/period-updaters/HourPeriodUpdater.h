#pragma once


#include <iomanip>  // For setw
#include <iostream>

#include "eckit/types/DateTime.h"
#include "multio/action/statistics-mtg2/TimeUtils.h"
#include "multio/action/statistics-mtg2/cfg/StatisticsConfiguration.h"
#include "multio/action/statistics-mtg2/period-updaters/PeriodUpdater.h"
#include "multio/message/Message.h"


namespace multio::action::statistics_mtg2 {

class HourPeriodUpdater final : public PeriodUpdater {
public:
    HourPeriodUpdater(long span) : PeriodUpdater{span} {};
    HourPeriodUpdater(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsOptions& opt) :
        PeriodUpdater{timeUnit(), IOmanager, opt} {};

    void dump(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsOptions& opt) const {
        std::ostringstream os;
        os << timeUnit() << "_" << std::setw(4) << std::setfill('0') << span_;
        PeriodUpdater::baseDump(timeUnit(), IOmanager, opt);
    };


    const std::string name() const {
        std::ostringstream os;
        os << std::setw(4) << std::setfill('0') << span_ << "-" << timeUnit();
        return os.str();
    };

    const std::string timeUnit() const {
        std::ostringstream os;
        os << "hour";
        return os.str();
    };

    eckit::DateTime computeWinStartTime(const eckit::DateTime& nextTime) const {
        const auto& d = nextTime.date();
        const auto& t = nextTime.time();
        return eckit::DateTime{d, eckit::Time{t.hours(), 0, 0}};
    };

    eckit::DateTime updateWinEndTime(const eckit::DateTime& startPoint) const {
        eckit::DateTime tmp = startPoint + static_cast<eckit::Second>(3600 * span_);
        return eckit::DateTime{tmp.date(), eckit::Time{tmp.time().hours(), 0, 0}};
    };
};

}  // namespace multio::action::statistics_mtg2

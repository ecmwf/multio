#pragma once


#include "eckit/types/DateTime.h"
#include "multio/action/statistics/TimeUtils.h"
#include "multio/action/statistics/cfg/StatisticsConfiguration.h"
#include "multio/action/statistics/period-updaters/PeriodUpdater.h"
#include "multio/message/Message.h"


namespace multio::action {

class MonthPeriodUpdater final : public PeriodUpdater {
public:
    MonthPeriodUpdater(long span) : PeriodUpdater{span} {};
    MonthPeriodUpdater(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsOptions& opt) :
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
        os << "month";
        return os.str();
    };

    eckit::DateTime computeWinStartTime(const eckit::DateTime& nextTime) const {
        const auto& d = nextTime.date();
        const auto& t = nextTime.time();
        auto year = d.year();
        auto month = d.month();
        return eckit::DateTime{eckit::Date{year, month, 1}, eckit::Time{0}};
    };


    eckit::DateTime updateWinEndTime(const eckit::DateTime& startPoint) const {
        auto totalSpan = startPoint.date().month() + span_ - 1;
        auto endYear = startPoint.date().year() + totalSpan / 12;
        auto endMonth = totalSpan % 12 + 1;
        return eckit::DateTime{eckit::Date{endYear, endMonth, 1}, eckit::Time{0}};
    };
};

}  // namespace multio::action

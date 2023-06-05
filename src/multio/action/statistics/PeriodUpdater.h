#pragma once

#include "StatisticsConfiguration.h"
#include "eckit/types/DateTime.h"
#include "multio/message/Message.h"

#include "MovingWindow.h"

namespace multio::action {

class PeriodUpdater {
public:
    PeriodUpdater(long span);
    virtual const std::string name() const = 0;
    eckit::DateTime updatePeriodStart(const message::Message& msg, const StatisticsConfiguration& cfg);
    eckit::DateTime updatePeriodEnd(const message::Message& msg, const StatisticsConfiguration& cfg);
    MovingWindow initPeriod(const message::Message& msg, std::shared_ptr<StatisticsIO>& IOmanager,
                            const StatisticsConfiguration& cfg);

private:
    eckit::DateTime computeWinCreationTime(const eckit::DateTime& currentTime);
    eckit::DateTime computeWinEndTime(const eckit::DateTime& currentTime);
    virtual eckit::DateTime computeWinStartTime(const eckit::DateTime& currentTime) = 0;
    virtual eckit::DateTime updateWinEndTime(const eckit::DateTime& startPoint) = 0;

protected:
    const long span_;
};


// -------------------------------------------------------------------------------------------------------------------


class HourPeriodUpdater : public PeriodUpdater {
public:
    HourPeriodUpdater(long span);
    const std::string name() const;

private:
    eckit::DateTime computeWinStartTime(const eckit::DateTime& currentTime);
    eckit::DateTime updateWinEndTime(const eckit::DateTime& startPoint);
};


// -------------------------------------------------------------------------------------------------------------------


class DayPeriodUpdater : public PeriodUpdater {
public:
    DayPeriodUpdater(long span);
    const std::string name() const;

private:
    eckit::DateTime computeWinStartTime(const eckit::DateTime& currentTime);
    eckit::DateTime updateWinEndTime(const eckit::DateTime& startPoint);
};


// -------------------------------------------------------------------------------------------------------------------


class MonthPeriodUpdater : public PeriodUpdater {
public:
    MonthPeriodUpdater(long span);
    const std::string name() const;

private:
    eckit::DateTime computeWinStartTime(const eckit::DateTime& currentTime);
    eckit::DateTime updateWinEndTime(const eckit::DateTime& startPoint);
};


// -------------------------------------------------------------------------------------------------------------------


std::shared_ptr<PeriodUpdater> make_period_updater(const std::string& periodKind, long span);

}  // namespace multio::action
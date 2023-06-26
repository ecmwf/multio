
#pragma once

#include "StatisticsConfiguration.h"
#include "eckit/types/DateTime.h"
#include "multio/message/Message.h"

#include "OperationWindow.h"

namespace multio::action {

class PeriodUpdater {
public:
    PeriodUpdater(long span);
    virtual const std::string name() const = 0;
    virtual const std::string timeUnit() const = 0;
    long timeSpan() const;
    eckit::DateTime updatePeriodStart(const message::Message& msg, const StatisticsConfiguration& cfg);
    eckit::DateTime updatePeriodEnd(const message::Message& msg, const StatisticsConfiguration& cfg);
    OperationWindow initPeriod(const message::Message& msg, std::shared_ptr<StatisticsIO>& IOmanager,
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


class HourPeriodUpdater final : public PeriodUpdater {
public:
    HourPeriodUpdater(long span);
    const std::string name() const;
    const std::string timeUnit() const;

private:
    eckit::DateTime computeWinStartTime(const eckit::DateTime& currentTime);
    eckit::DateTime updateWinEndTime(const eckit::DateTime& startPoint);
};


// -------------------------------------------------------------------------------------------------------------------


class DayPeriodUpdater final : public PeriodUpdater {
public:
    DayPeriodUpdater(long span);
    const std::string name() const;
    const std::string timeUnit() const;

private:
    eckit::DateTime computeWinStartTime(const eckit::DateTime& currentTime);
    eckit::DateTime updateWinEndTime(const eckit::DateTime& startPoint);
};


// -------------------------------------------------------------------------------------------------------------------


class MonthPeriodUpdater final : public PeriodUpdater {
public:
    MonthPeriodUpdater(long span);
    const std::string name() const;
    const std::string timeUnit() const;

private:
    eckit::DateTime computeWinStartTime(const eckit::DateTime& currentTime);
    eckit::DateTime updateWinEndTime(const eckit::DateTime& startPoint);
};


// -------------------------------------------------------------------------------------------------------------------

std::shared_ptr<PeriodUpdater> make_period_updater(std::string const& output_freq);

}  // namespace multio::action

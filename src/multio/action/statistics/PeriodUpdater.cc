#include "PeriodUpdater.h"

#include <iomanip>
#include <regex>

#include "TimeUtils.h"

namespace multio::action {

PeriodUpdater::PeriodUpdater(long span) : span_{span} {};


eckit::DateTime PeriodUpdater::updatePeriodStart(const message::Message& msg, const StatisticsConfiguration& cfg) {
    return computeWinStartTime(nextDateTime(msg, cfg));
};

eckit::DateTime PeriodUpdater::updatePeriodEnd(const message::Message& msg, const StatisticsConfiguration& cfg) {
    return updateWinEndTime(computeWinStartTime(nextDateTime(msg, cfg)));
};

OperationWindow PeriodUpdater::initPeriod(const message::Message& msg, std::shared_ptr<StatisticsIO>& IOmanager,
                                          const StatisticsConfiguration& cfg) {
    if (cfg.readRestart()) {
        IOmanager->setSuffix(name());
        return OperationWindow{IOmanager, cfg};
    }
    else {
        eckit::DateTime epochPoint{epochDateTime(msg, cfg)};
        eckit::DateTime startPoint{computeWinStartTime(winStartDateTime(msg, cfg))};
        eckit::DateTime creationPoint{computeWinCreationTime(winStartDateTime(msg, cfg))};
        eckit::DateTime endPoint{computeWinEndTime(startPoint)};
        return OperationWindow{epochPoint, startPoint, creationPoint, endPoint, cfg.timeStep()};
    }
};

long PeriodUpdater::timeSpan() const {
    return span_;
}

eckit::DateTime PeriodUpdater::computeWinCreationTime(const eckit::DateTime& currentTime) {
    return currentTime;
};

eckit::DateTime PeriodUpdater::computeWinEndTime(const eckit::DateTime& startPoint) {
    return eckit::DateTime{updateWinEndTime(startPoint)};
};


// -------------------------------------------------------------------------------------------------------------------

HourPeriodUpdater::HourPeriodUpdater(long span) : PeriodUpdater{span} {};


const std::string HourPeriodUpdater::name() const {
    std::ostringstream os;
    os << std::setw(4) << std::setfill('0') << span_ << "-"
       << "hour";
    return os.str();
};

const std::string HourPeriodUpdater::timeUnit() const {
    std::ostringstream os;
    os << "hour";
    return os.str();
};

eckit::DateTime HourPeriodUpdater::computeWinStartTime(const eckit::DateTime& currentTime) {
    return eckit::DateTime{currentTime.date(), eckit::Time{currentTime.time().hours(), 0, 0}};
};

eckit::DateTime HourPeriodUpdater::updateWinEndTime(const eckit::DateTime& startPoint) {
    eckit::DateTime tmp = startPoint + static_cast<eckit::Second>(3600 * span_);
    return eckit::DateTime{tmp.date(), eckit::Time{tmp.time().hours(), 0, 0}};
};

// -------------------------------------------------------------------------------------------------------------------


DayPeriodUpdater::DayPeriodUpdater(long span) : PeriodUpdater{span} {};


const std::string DayPeriodUpdater::name() const {
    std::ostringstream os;
    os << std::setw(4) << std::setfill('0') << span_ << "-"
       << "day";
    return os.str();
};

const std::string DayPeriodUpdater::timeUnit() const {
    std::ostringstream os;
    os << "day";
    return os.str();
};

eckit::DateTime DayPeriodUpdater::computeWinStartTime(const eckit::DateTime& currentTime) {
    return eckit::DateTime{currentTime.date(), eckit::Time{0}};
};

eckit::DateTime DayPeriodUpdater::updateWinEndTime(const eckit::DateTime& startPoint) {
    eckit::DateTime tmp = startPoint + static_cast<eckit::Second>(3600 * 24 * span_);
    return eckit::DateTime{tmp.date(), eckit::Time{0}};
};

// -------------------------------------------------------------------------------------------------------------------


MonthPeriodUpdater::MonthPeriodUpdater(long span) : PeriodUpdater{span} {};


const std::string MonthPeriodUpdater::name() const {
    std::ostringstream os;
    os << std::setw(4) << std::setfill('0') << span_ << "-"
       << "month";
    return os.str();
};

const std::string MonthPeriodUpdater::timeUnit() const {
    std::ostringstream os;
    os << "month";
    return os.str();
};

eckit::DateTime MonthPeriodUpdater::computeWinStartTime(const eckit::DateTime& currentTime) {
    auto year = currentTime.date().year();
    auto month = currentTime.date().month();
    return eckit::DateTime{eckit::Date{year, month, 1}, eckit::Time{0}};
};

eckit::DateTime MonthPeriodUpdater::updateWinEndTime(const eckit::DateTime& startPoint) {
    auto totalSpan = startPoint.date().month() + span_ - 1;
    auto endYear = startPoint.date().year() + totalSpan / 12;
    auto endMonth = totalSpan % 12 + 1;
    return eckit::DateTime{eckit::Date{endYear, endMonth, 1}, eckit::Time{0}};
};


// -------------------------------------------------------------------------------------------------------------------

std::shared_ptr<PeriodUpdater> make_period_updater(std::string const& output_freq) {
    static const std::regex period_grammar("([1-9][0-9]*)([a-z]+)");
    std::smatch match;
    if (std::regex_match(output_freq, match, period_grammar)) {
        const std::string periodKind = match[2].str();
        long span = std::stol(match[1].str());
        if (periodKind == "hour" || periodKind == "h") {
            return std::make_unique<HourPeriodUpdater>(span);
        }
        if (periodKind == "day" || periodKind == "d") {
            return std::make_unique<DayPeriodUpdater>(span);
        }
        if (periodKind == "month" || periodKind == "m") {
            return std::make_unique<MonthPeriodUpdater>(span);
        }
        throw eckit::SeriousBug("Unknown period kind : " + periodKind, Here());
    }
    else {
        throw eckit::SeriousBug("Wrong grammar in period definition : " + output_freq, Here());
    }
};

}  // namespace multio::action

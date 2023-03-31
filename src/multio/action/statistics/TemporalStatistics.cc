
#include "TemporalStatistics.h"

#include <cstring>
#include <iostream>

#include "eckit/exception/Exceptions.h"
#include "multio/LibMultio.h"
#include "multio/util/PrecisionTag.h"

namespace multio {
namespace action {

namespace {
auto reset_statistics(const std::vector<std::string>& opNames, message::Message msg, const StatisticsOptions& options) {
    return multio::util::dispatchPrecisionTag(msg.precision(), [&](auto pt) {
        using Precision = typename decltype(pt)::type;
        std::vector<OperationVar> stats;
        for (const auto& op : opNames) {
            stats.push_back(make_operation<Precision>(op, msg.size(), options));
        }
        return stats;
    });
}

eckit::DateTime currentDateTime(const message::Message& msg, const StatisticsOptions& options) {
    eckit::Date startDate{options.startDate()};
    long startTime = options.startTime();
    auto hour = startTime / 10000;
    auto minute = (startTime % 10000) / 100;
    eckit::DateTime startDateTime{startDate, eckit::Time{hour, minute, 0}};

    return startDateTime + static_cast<eckit::Second>(msg.metadata().getLong("step") * options.timeStep());
}

eckit::DateTime nextDateTime(const message::Message& msg, const StatisticsOptions& options) {
    return currentDateTime(msg, options) + static_cast<eckit::Second>(options.stepFreq() * options.timeStep());
}

}  // namespace

std::unique_ptr<TemporalStatistics> TemporalStatistics::build(const std::string& unit, long span,
                                                              const std::vector<std::string>& operations,
                                                              const message::Message& msg,
                                                              const StatisticsOptions& options ) {
    long step = msg.metadata().getLong("step");

    if (unit == "month") {
        return std::make_unique<MonthlyStatistics>(operations, span, msg, options, step);
    }

    if (unit == "day") {
        return std::make_unique<DailyStatistics>(operations, span, msg, options, step );
    }

    if (unit == "hour") {
        return std::make_unique<HourlyStatistics>(operations, span, msg, options, step );
    }

    throw eckit::SeriousBug{"Temporal statistics for base period " + unit + " is not defined"};
}

TemporalStatistics::TemporalStatistics(const std::vector<std::string>& operations, const DateTimePeriod& period,
                                       const message::Message& msg, const StatisticsOptions& options, long step) :
    name_{msg.name()},
    current_{period},
    options_{options},
    opNames_{operations},
    statistics_{reset_statistics(operations, msg, options)},
    prevStep_{step} {}


bool TemporalStatistics::process(message::Message& msg) {
    return process_next(msg);
}

void TemporalStatistics::updateStatistics(const message::Message& msg) {
    for (auto const& stat : statistics_) {
        std::visit(Overloaded{[&msg](const std::unique_ptr<Operation<double>>& arg) {
                                  return arg->update(msg.payload().data(), msg.size());
                              },
                              [&msg](const std::unique_ptr<Operation<float>>& arg) {
                                  return arg->update(msg.payload().data(), msg.size());
                              }},
                   stat);
    }
}

bool TemporalStatistics::process_next(message::Message& msg) {
    ASSERT(name_ == msg.name());

    LOG_DEBUG_LIB(multio::LibMultio) << *this << std::endl;
    LOG_DEBUG_LIB(multio::LibMultio) << " *** Current ";

    auto dateTime = currentDateTime(msg, options_);
    if (!current_.isWithin(dateTime)) {
        std::ostringstream os;
        os << dateTime << " is outside of current period " << current_ << std::endl;
        throw eckit::UserError(os.str(), Here());
    }

    updateStatistics(msg);

    LOG_DEBUG_LIB(multio::LibMultio) << " *** Next    ";
    return current_.isWithin(nextDateTime(msg, options_));
}

void TemporalStatistics::resetPeriod(const message::Message& msg) {
    current_.reset(currentDateTime(msg, options_));
}

eckit::DateTime computeMonthStart(const eckit::DateTime& currentTime) {
    return currentTime;
};

eckit::DateTime computeDayStart(const eckit::DateTime& currentTime) {
    return eckit::DateTime{currentTime.date(), eckit::Time{0}};
};

eckit::DateTime computeMonthEnd(const eckit::DateTime& startPoint, long span) {
    auto totalSpan = startPoint.date().month() + span - 1;  // Make it zero-based
    auto endYear = startPoint.date().year() + totalSpan / 12;
    auto endMonth = totalSpan % 12 + 1;
    return eckit::DateTime{eckit::Date{endYear, endMonth, 1}, eckit::Time{0}};
};

void MonthlyStatistics::resetPeriod(const message::Message& msg) {
    eckit::DateTime startPoint = computeMonthStart(currentDateTime(msg, options_));
    eckit::DateTime endPoint = computeMonthEnd(startPoint, span_);
    current_.reset(startPoint, endPoint);
}


std::map<std::string, eckit::Buffer> TemporalStatistics::compute(const message::Message& msg) {
    std::map<std::string, eckit::Buffer> retStats;
    for (auto const& stat : statistics_) {

        auto buf = std::visit([](auto&& arg) { return arg->compute(); }, stat);

        const auto& operation = std::visit([](auto&& arg) { return arg->operation(); }, stat);

        retStats.emplace(operation, std::move(buf));
    }
    return retStats;
}

std::string TemporalStatistics::stepRange(long step) {
    auto ret = std::to_string(prevStep_) + "-" + std::to_string(step);
    prevStep_ = step;
    LOG_DEBUG_LIB(multio::LibMultio) << " *** Setting step range: " << ret << std::endl;
    return ret;
}

const DateTimePeriod& TemporalStatistics::current() const {
    return current_;
}

void TemporalStatistics::reset(const message::Message& msg) {
    statistics_ = reset_statistics(opNames_, msg, options_);
    resetPeriod(msg);
    LOG_DEBUG_LIB(::multio::LibMultio) << " ------ Resetting statistics for temporal type " << *this << std::endl;
}

//-------------------------------------------------------------------------------------------------

HourlyStatistics::HourlyStatistics(const std::vector<std::string> operations, long span, message::Message msg,
                                   const StatisticsOptions& options, long step) :
    TemporalStatistics{operations,
                       DateTimePeriod{currentDateTime(msg, options), static_cast<eckit::Second>(3600 * span)}, msg,
                       options, step} {}

void HourlyStatistics::print(std::ostream& os) const {
    os << "Hourly Statistics(" << current_ << ")";
}

//-------------------------------------------------------------------------------------------------

DailyStatistics::DailyStatistics(const std::vector<std::string> operations, long span, message::Message msg,
                                 const StatisticsOptions& options, long step) :
    TemporalStatistics{
        operations,
        DateTimePeriod{computeDayStart(currentDateTime(msg, options)), static_cast<eckit::Second>(24 * 3600 * span)},
        msg, options, step} {}


void DailyStatistics::print(std::ostream& os) const {
    os << "Daily Statistics(" << current_ << ")";
}

//-------------------------------------------------------------------------------------------------

namespace {
DateTimePeriod setMonthlyPeriod(long span, const message::Message& msg, const StatisticsOptions& options) {
    eckit::DateTime startPoint{computeMonthStart(currentDateTime(msg, options))};
    eckit::DateTime endPoint{computeMonthEnd(startPoint, span)};
    return DateTimePeriod{startPoint, endPoint};
}
}  // namespace

MonthlyStatistics::MonthlyStatistics(const std::vector<std::string> operations, long span, message::Message msg,
                                     const StatisticsOptions& options,long step) :
    TemporalStatistics{operations, setMonthlyPeriod(span, msg, options), msg, options,step}, span_(span) {}

void MonthlyStatistics::print(std::ostream& os) const {
    os << "Monthly Statistics(" << current_ << ")";
}

//-------------------------------------------------------------------------------------------------

}  // namespace action
}  // namespace multio

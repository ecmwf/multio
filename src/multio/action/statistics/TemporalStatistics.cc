
#include "TemporalStatistics.h"

#include <cstring>
#include <iostream>

#include "eckit/exception/Exceptions.h"
#include "multio/LibMultio.h"
#include "multio/util/PrecisionTag.h"

namespace multio {
namespace action {

namespace {
auto reset_statistics(const std::vector<std::string>& opNames, message::Message msg) {
    return multio::util::dispatchPrecisionTag(msg.precision(), [&](auto pt) {
        using Precision = typename decltype(pt)::type;
        std::vector<OperationVar> stats;
        for (const auto& op : opNames) {
            stats.push_back(make_operation<Precision>(op, msg.size()));
        }
        return stats;
    });
}

eckit::DateTime currentDateTime(const message::Message& msg) {

    eckit::Date startDate{eckit::Date{msg.metadata().getLong("date")}};
    auto startTime = msg.metadata().getLong("time");
    auto hour = startTime / 10000;
    auto minute = (startTime % 10000) / 100;
    eckit::DateTime startDateTime{startDate, eckit::Time{hour, minute, 0}};

    return startDateTime
         + static_cast<eckit::Second>(msg.metadata().getLong("step") * msg.metadata().getLong("timeStep"));
}

eckit::DateTime nextDateTime(const message::Message& msg) {
    return currentDateTime(msg)
         + static_cast<eckit::Second>(msg.metadata().getLong("step-frequency") * msg.metadata().getLong("timeStep"));
}

}  // namespace

std::unique_ptr<TemporalStatistics> TemporalStatistics::build(const std::string& unit, long span,
                                                              const std::vector<std::string>& operations,
                                                              const message::Message& msg) {
    if (unit == "month") {
        return std::make_unique<MonthlyStatistics>(operations, span, msg);
    }

    if (unit == "day") {
        return std::make_unique<DailyStatistics>(operations, span, msg);
    }

    if (unit == "hour") {
        return std::make_unique<HourlyStatistics>(operations, span, msg);
    }

    throw eckit::SeriousBug{"Temporal statistics for base period " + unit + " is not defined"};
}

TemporalStatistics::TemporalStatistics(const std::vector<std::string>& operations, const DateTimePeriod& period,
                                       const message::Message& msg) :
    name_{msg.name()}, current_{period}, opNames_{operations}, statistics_{reset_statistics(operations, msg)} {}


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

    LOG_DEBUG_LIB(LibMultio) << *this << std::endl;
    LOG_DEBUG_LIB(LibMultio) << " *** Current ";

    auto dateTime = currentDateTime(msg);
    if (!current_.isWithin(dateTime)) {
        std::ostringstream os;
        os << dateTime << " is outside of current period " << current_ << std::endl;
        throw eckit::AssertionFailed(os.str());
    }

    updateStatistics(msg);

    LOG_DEBUG_LIB(LibMultio) << " *** Next    ";
    return current_.isWithin(nextDateTime(msg));
}

void TemporalStatistics::resetPeriod(const message::Message& msg) {
    current_.reset(currentDateTime(msg));
}

eckit::DateTime computeMonthStart(const eckit::DateTime& currentTime) {
    return eckit::DateTime{eckit::Date{currentTime.date().year(), currentTime.date().month(), 1}, eckit::Time{0}};
};

eckit::DateTime computeMonthEnd(const eckit::DateTime& startPoint, long span) {
    auto totalSpan = startPoint.date().month() + span - 1;  // Make it zero-based
    auto endYear = startPoint.date().year() + totalSpan / 12;
    auto endMonth = totalSpan % 12 + 1;
    return eckit::DateTime{eckit::Date{endYear, endMonth, 1}, eckit::Time{0}};
};

void MonthlyStatistics::resetPeriod(const message::Message& msg) {
    eckit::DateTime startPoint = computeMonthStart(currentDateTime(msg));
    eckit::DateTime endPoint = computeMonthEnd(startPoint, span_);
    current_.reset(startPoint, endPoint);
}


std::map<std::string, eckit::Buffer> TemporalStatistics::compute(const message::Message& msg) {
    std::map<std::string, eckit::Buffer> retStats;
    for (auto const& stat : statistics_) {

        auto buf = std::visit([](auto&& arg) { return arg->compute(); }, stat);

        const auto& name = std::visit([](auto&& arg) { return arg->name(); }, stat);

        retStats.emplace(name, std::move(buf));
    }
    return retStats;
}

std::string TemporalStatistics::stepRange(long step) {
    auto ret = std::to_string(prevStep_) + "-" + std::to_string(step);
    prevStep_ = step;
    LOG_DEBUG_LIB(LibMultio) << " *** Setting step range: " << ret << std::endl;
    return ret;
}

const DateTimePeriod& TemporalStatistics::current() const {
    return current_;
}

void TemporalStatistics::reset(const message::Message& msg) {
    statistics_ = reset_statistics(opNames_, msg);
    resetPeriod(msg);
    LOG_DEBUG_LIB(LibMultio) << " ------ Resetting statistics for temporal type " << *this << std::endl;
}

//-------------------------------------------------------------------------------------------------

HourlyStatistics::HourlyStatistics(const std::vector<std::string> operations, long span, message::Message msg) :
    TemporalStatistics{operations,
                       DateTimePeriod{eckit::DateTime{eckit::Date{msg.metadata().getLong("date")}, eckit::Time{0}},
                                      static_cast<eckit::Second>(3600 * span)},
                       msg} {}

void HourlyStatistics::print(std::ostream& os) const {
    os << "Hourly Statistics(" << current_ << ")";
}

//-------------------------------------------------------------------------------------------------

DailyStatistics::DailyStatistics(const std::vector<std::string> operations, long span, message::Message msg) :
    TemporalStatistics{operations,
                       DateTimePeriod{eckit::DateTime{eckit::Date{msg.metadata().getLong("date")}, eckit::Time{0}},
                                      static_cast<eckit::Second>(24 * 3600 * span)},
                       msg} {}

void DailyStatistics::print(std::ostream& os) const {
    os << "Daily Statistics(" << current_ << ")";
}

//-------------------------------------------------------------------------------------------------

namespace {
DateTimePeriod setMonthlyPeriod(long span, const message::Message& msg) {
    eckit::DateTime startPoint{computeMonthStart(currentDateTime(msg))};
    eckit::DateTime endPoint{computeMonthEnd(startPoint, span)};
    return DateTimePeriod{startPoint, endPoint};
}
}  // namespace

MonthlyStatistics::MonthlyStatistics(const std::vector<std::string> operations, long span, message::Message msg) :
    TemporalStatistics{operations, setMonthlyPeriod(span, msg), msg}, span_(span) {}

void MonthlyStatistics::print(std::ostream& os) const {
    os << "Monthly Statistics(" << current_ << ")";
}

//-------------------------------------------------------------------------------------------------

}  // namespace action
}  // namespace multio

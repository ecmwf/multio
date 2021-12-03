
#include "TemporalStatistics.h"

#include <cstring>
#include <iostream>

#include "eckit/exception/Exceptions.h"
#include "multio/LibMultio.h"

namespace multio {
namespace action {

namespace  {
std::vector<std::unique_ptr<Operation>> reset_statistics(const std::vector<std::string>& opNames,
                                                         long sz) {
    std::vector<std::unique_ptr<Operation>> stats;
    for (const auto& op : opNames) {
        stats.push_back(make_operation(op, sz));
    }
    return stats;
}

eckit::DateTime currentDateTime(const message::Message& msg) {
    eckit::Date startDate{eckit::Date{msg.metadata().getLong("date")}};
    eckit::DateTime startDateTime{startDate, eckit::Time{0}};
    return startDateTime + static_cast<eckit::Second>(msg.metadata().getLong("step") *
                                                      msg.metadata().getLong("timeStep"));
}
}  // namespace

std::unique_ptr<TemporalStatistics> TemporalStatistics::build(
    const std::string& unit, long span, const std::vector<std::string>& operations,
    const message::Message& msg) {

    if (unit == "month") {
        return std::unique_ptr<TemporalStatistics>{new MonthlyStatistics{operations, span, msg}};
    }

    if (unit == "day") {
        return std::unique_ptr<TemporalStatistics>{new DailyStatistics{operations, span, msg}};
    }

    if (unit == "hour") {
        return std::unique_ptr<TemporalStatistics>{new HourlyStatistics{operations, span, msg}};
    }

    throw eckit::SeriousBug{"Temporal statistics for base period " + unit + " is not defined"};
}

TemporalStatistics::TemporalStatistics(const std::string& name, const DateTimePeriod& period,
                                       const std::vector<std::string>& operations, size_t sz) :
    name_{name},
    current_{period},
    opNames_{operations},
    statistics_{reset_statistics(operations, sz)} {}

bool TemporalStatistics::process(message::Message& msg) {
    return process_next(msg);
}

void TemporalStatistics::updateStatistics(const message::Message& msg) {
    auto data_ptr = static_cast<const double*>(msg.payload().data());
    for(auto const& stat : statistics_) {
        stat->update(data_ptr, msg.size() / sizeof(double));
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

    dateTime = dateTime + static_cast<eckit::Second>(msg.metadata().getLong("timeStep"));

    LOG_DEBUG_LIB(LibMultio) << " *** Next    ";
    return current_.isWithin(dateTime);
}

void TemporalStatistics::resetPeriod(const message::Message& msg) {
    current_.reset(currentDateTime(msg));
}

std::map<std::string, eckit::Buffer> TemporalStatistics::compute(const message::Message& msg) {
    std::map<std::string, eckit::Buffer> retStats;
    for (auto const& stat : statistics_) {
        eckit::Buffer buf{msg.size()};
        const auto& res = stat->compute();
        std::memcpy(buf, res.data(), msg.size());
        retStats.emplace(stat->name(), std::move(buf));
    }
    return retStats;
}

std::string TemporalStatistics::stepRange(long step) {
    auto ret = std::to_string(prevStep_) + "-" + std::to_string(step);
    prevStep_ = step;
    LOG_DEBUG_LIB(LibMultio) << " *** Setting step range: " << ret << std::endl;
    return ret;
}

void TemporalStatistics::reset(const message::Message& msg) {
    statistics_ = reset_statistics(opNames_, msg.size() / sizeof(double));
    resetPeriod(msg);
    LOG_DEBUG_LIB(LibMultio) << " ------ Resetting statistics for temporal type " << *this
                             << std::endl;
}

//-------------------------------------------------------------------------------------------------

HourlyStatistics::HourlyStatistics(const std::vector<std::string> operations, long span,
                                   message::Message msg) :
    TemporalStatistics{msg.name(),
                       DateTimePeriod{eckit::DateTime{eckit::Date{msg.metadata().getString("date")},
                                                      eckit::Time{0}},
                                      static_cast<eckit::Second>(3600 * span)},
                       operations, msg.size() / sizeof(double)} {}

void HourlyStatistics::print(std::ostream &os) const {
    os << "Hourly Statistics(" << current_ << ")";
}

//-------------------------------------------------------------------------------------------------

DailyStatistics::DailyStatistics(const std::vector<std::string> operations, long span,
                                 message::Message msg) :
    TemporalStatistics{msg.name(),
                       DateTimePeriod{eckit::DateTime{eckit::Date{msg.metadata().getString("date")},
                                                      eckit::Time{0}},
                                      static_cast<eckit::Second>(24 * 3600 * span)},
                       operations, msg.size() / sizeof(double)} {}

void DailyStatistics::print(std::ostream &os) const {
    os << "Daily Statistics(" << current_ << ")";
}

//-------------------------------------------------------------------------------------------------

namespace  {
DateTimePeriod setMonthlyPeriod(long span, const message::Message& msg) {
    eckit::DateTime startPoint{eckit::Date{msg.metadata().getString("date")}, eckit::Time{0}};

    auto totalSpan = startPoint.date().month() + span - 1; // Make it zero-based

    auto endYear = startPoint.date().year() + totalSpan / 12;
    auto endMonth = totalSpan % 12 + 1;
    auto endDay = startPoint.date().day();

    ASSERT_MSG(endDay < 29, "No support for monthly period starting beyond day 28");

    eckit::DateTime endPoint{eckit::Date{endYear, endMonth, endDay}, eckit::Time{0}};

    return DateTimePeriod{startPoint, endPoint};
}
}  // namespace

MonthlyStatistics::MonthlyStatistics(const std::vector<std::string> operations, long span,
                                     message::Message msg) :
    TemporalStatistics{msg.name(), setMonthlyPeriod(span, msg), operations,
                       msg.size() / sizeof(double)} {}

void MonthlyStatistics::print(std::ostream& os) const {
    os << "Monthly Statistics(" << current_ << ")";
}

//-------------------------------------------------------------------------------------------------

}  // namespace action
}  // namespace multio

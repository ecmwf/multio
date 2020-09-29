
#include "TemporalStatistics.h"

#include "eckit/exception/Exceptions.h"

#include <cstring>
#include <iostream>

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
}

std::unique_ptr<TemporalStatistics> TemporalStatistics::build(
    const std::string& unit, const std::vector<std::string>& operations,
    const message::Message& msg) {
    const auto& nm = msg.name();
    const auto& date = msg.metadata().getString("date");

    if (unit == "month") {
        return std::unique_ptr<TemporalStatistics>{new MonthlyStatistics{operations, nm, date}};
    }

    if (unit == "day") {
        return std::unique_ptr<TemporalStatistics>{new DailyStatistics{operations, nm, date}};
    }

    if (unit == "hour") {
        return std::unique_ptr<TemporalStatistics>{new HourlyStatistics{operations, nm, date}};
    }

    throw eckit::SeriousBug{"Temporal statistics for base period " + unit + " is not defined"};
}

TemporalStatistics::TemporalStatistics(const std::vector<std::string>& operations) :
    opNames_{operations} {}

bool TemporalStatistics::process(message::Message& msg) {
    return process_next(msg);
}

void TemporalStatistics::updateStatistics(const message::Message& msg) {
    auto data_ptr = static_cast<const double*>(msg.payload().data());
    for(auto const& stat : statistics_) {
        stat->update(data_ptr, msg.size() / sizeof(double));
    }
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

void TemporalStatistics::reset(const message::Message& msg) {
    reset_statistics(opNames_, msg.globalSize());
}

//-------------------------------------------------------------------------------------------------

namespace  {
bool sameYearMonth(const eckit::Date& date1, const eckit::Date& date2) {
    return date1.month() == date2.month() && date1.year() == date2.year();
}
}  // namespace

MonthlyStatistics::MonthlyStatistics(const std::vector<std::string> operations,
                                     const std::string& name, const std::string& date) :
    TemporalStatistics{operations}, name_{name}, current_{eckit::Date{date}} {}

bool MonthlyStatistics::process_next(message::Message& msg) {
    ASSERT(name_ == msg.name());

    // Update current date
    eckit::Date startDate{eckit::Date{msg.metadata().getString("date")}};
    eckit::DateTime startDateTime{startDate, eckit::Time{0}};
    eckit::DateTime dateTime =
        startDateTime + static_cast<eckit::Second>(msg.metadata().getLong("step") *
                                                   msg.metadata().getLong("timeStep"));
    current_ = dateTime.date();

    ASSERT(sameYearMonth(current_, dateTime.date()));

    updateStatistics(msg);

    dateTime = dateTime + static_cast<eckit::Second>(msg.metadata().getLong("timeStep"));

    return sameYearMonth(current_, dateTime.date());
}

void MonthlyStatistics::print(std::ostream& os) const {
    os << "Monthly Statistics(" << current_ << ")" << std::endl;
}

//-------------------------------------------------------------------------------------------------

namespace  {
bool sameYearMonthDay(const eckit::Date& date1, const eckit::Date& date2) {
    return date1.day() == date2.day() && date1.month() == date2.month() &&
           date1.year() == date2.year();
}
}  // namespace

DailyStatistics::DailyStatistics(const std::vector<std::string> operations, const std::string& name,
                                 const std::string& date) :
    TemporalStatistics{operations}, name_{name}, current_{eckit::Date{date}} {}

bool DailyStatistics::process_next(message::Message& msg) {
    ASSERT(name_ == msg.name());

    // Update current date
    eckit::Date startDate{eckit::Date{msg.metadata().getString("date")}};
    eckit::DateTime startDateTime{startDate, eckit::Time{0}};
    eckit::DateTime dateTime =
        startDateTime + static_cast<eckit::Second>(msg.metadata().getLong("step") *
                                                   msg.metadata().getLong("timeStep"));
    current_ = dateTime.date();

    updateStatistics(msg);

    ASSERT(sameYearMonthDay(current_, dateTime.date()));

    dateTime = dateTime + static_cast<eckit::Second>(msg.metadata().getLong("timeStep"));

    return sameYearMonthDay(current_, dateTime.date());
}

void DailyStatistics::print(std::ostream &os) const {
    os << "Daily Statistics(" << current_ << ")" << std::endl;
}

//-------------------------------------------------------------------------------------------------

namespace  {
bool sameHour(const eckit::DateTime& dt1, const eckit::DateTime& dt2) {
    return sameYearMonthDay(dt1.date(), dt2.date()) && dt1.time().hours() == dt2.time().hours();
}
}  // namespace

HourlyStatistics::HourlyStatistics(const std::vector<std::string> operations,
                                   const std::string& name, const std::string& date) :
    TemporalStatistics{operations}, name_{name}, current_{eckit::DateTime{eckit::Date{date}}} {}

bool HourlyStatistics::process_next(message::Message &msg) {
    ASSERT(name_ == msg.name());

    // Update current date
    eckit::Date startDate{eckit::Date{msg.metadata().getString("date")}};
    eckit::DateTime startDateTime{startDate, eckit::Time{0}};
    eckit::DateTime dateTime =
        startDateTime + static_cast<eckit::Second>(msg.metadata().getLong("step") *
                                                   msg.metadata().getLong("timeStep"));
    current_ = dateTime;

    updateStatistics(msg);

    ASSERT(sameHour(current_, dateTime));

    dateTime = dateTime + static_cast<eckit::Second>(msg.metadata().getLong("timeStep"));

    return sameHour(current_, dateTime);
}

void HourlyStatistics::print(std::ostream &os) const {
    os << "Hourly Statistics(" << current_ << ")" << std::endl;
}

//-------------------------------------------------------------------------------------------------

}  // namespace action
}  // namespace multio

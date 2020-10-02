
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

eckit::DateTime currentDateTime(const message::Message& msg) {
    eckit::Date startDate{eckit::Date{msg.metadata().getString("date")}};
    eckit::DateTime startDateTime{startDate, eckit::Time{0}};
    return startDateTime + static_cast<eckit::Second>(msg.metadata().getLong("step") *
                                                      msg.metadata().getLong("timeStep"));
}
}

std::unique_ptr<TemporalStatistics> TemporalStatistics::build(
    const std::string& unit, long span, const std::vector<std::string>& operations,
    const message::Message& msg) {
    const auto& nm = msg.name();
    const auto& date = msg.metadata().getString("date");

    if (unit == "month") {
        return std::unique_ptr<TemporalStatistics>{
            new MonthlyStatistics{operations, nm, date, span}};
    }

    if (unit == "day") {
        return std::unique_ptr<TemporalStatistics>{new DailyStatistics{operations, nm, date, span}};
    }

    if (unit == "hour") {
        return std::unique_ptr<TemporalStatistics>{
            new HourlyStatistics{operations, nm, date, span}};
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
    resetPeriod(msg);
}

//-------------------------------------------------------------------------------------------------

MonthlyStatistics::MonthlyStatistics(const std::vector<std::string> operations,
                                     const std::string& name, const std::string& date, long span) :
    TemporalStatistics{operations}, name_{name}, current_{eckit::Date{date}, span} {}

bool MonthlyStatistics::process_next(message::Message& msg) {
    ASSERT(name_ == msg.name());

    auto dateTime = currentDateTime(msg);

    ASSERT(current_.samePeriod(dateTime.date()));

    updateStatistics(msg);

    dateTime = dateTime + static_cast<eckit::Second>(msg.metadata().getLong("timeStep"));

    return current_.samePeriod(dateTime.date());
}

void MonthlyStatistics::resetPeriod(const message::Message& msg) {
    current_.reset(currentDateTime(msg).date());
}

void MonthlyStatistics::print(std::ostream& os) const {
    os << "Monthly Statistics(" << current_ << ")";
}

//-------------------------------------------------------------------------------------------------

DailyStatistics::DailyStatistics(const std::vector<std::string> operations, const std::string& name,
                                 const std::string& date, long span) :
    TemporalStatistics{operations}, name_{name}, current_{eckit::Date{date}, span} {}

bool DailyStatistics::process_next(message::Message& msg) {
    ASSERT(name_ == msg.name());

    auto dateTime = currentDateTime(msg);

    ASSERT(current_.samePeriod(dateTime.date()));

    updateStatistics(msg);

    dateTime = dateTime + static_cast<eckit::Second>(msg.metadata().getLong("timeStep"));

    return current_.samePeriod(dateTime.date());
}

void DailyStatistics::resetPeriod(const message::Message& msg) {
    current_.reset(currentDateTime(msg).date());
}

void DailyStatistics::print(std::ostream &os) const {
    os << "Daily Statistics(" << current_ << ")" << std::endl;
}

//-------------------------------------------------------------------------------------------------

HourlyStatistics::HourlyStatistics(const std::vector<std::string> operations,
                                   const std::string& name, const std::string& date, long span) :
    TemporalStatistics{operations},
    name_{name},
    current_{eckit::DateTime{eckit::Date{date}}, 3600 * static_cast<eckit::Second>(span)} {}

bool HourlyStatistics::process_next(message::Message &msg) {
    ASSERT(name_ == msg.name());

    auto dateTime = currentDateTime(msg);

    ASSERT(current_.samePeriod(dateTime));

    updateStatistics(msg);

    dateTime = dateTime + static_cast<eckit::Second>(msg.metadata().getLong("timeStep"));

    return current_.samePeriod(dateTime);
}

void HourlyStatistics::resetPeriod(const message::Message& msg) {
    current_.reset(currentDateTime(msg));
}

void HourlyStatistics::print(std::ostream &os) const {
    os << "Hourly Statistics(" << current_ << ")" << std::endl;
}

//-------------------------------------------------------------------------------------------------

}  // namespace action
}  // namespace multio


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

TemporalStatistics::TemporalStatistics(const std::vector<std::string>& operations, size_t sz) :
    opNames_{operations}, statistics_{reset_statistics(operations, sz)} {}

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
    statistics_ = reset_statistics(opNames_, msg.globalSize());
    resetPeriod(msg);
    LOG_DEBUG_LIB(LibMultio) << " ======== Resetting statistics for temporal type " << *this
                             << std::endl;
}

//-------------------------------------------------------------------------------------------------

MonthlyStatistics::MonthlyStatistics(const std::vector<std::string> operations, long span,
                                     message::Message msg) :
    TemporalStatistics{operations, msg.size() / sizeof(double)},
    name_{msg.name()},
    current_{eckit::Date{msg.metadata().getString("date")}, span} {}

bool MonthlyStatistics::process_next(message::Message& msg) {
    ASSERT(name_ == msg.name());

    auto dateTime = currentDateTime(msg);

    ASSERT(current_.isWithin(dateTime.date()));

    updateStatistics(msg);

    dateTime = dateTime + static_cast<eckit::Second>(msg.metadata().getLong("timeStep"));

    return current_.isWithin(dateTime.date());
}

void MonthlyStatistics::resetPeriod(const message::Message& msg) {
    current_.reset(currentDateTime(msg).date());
}

void MonthlyStatistics::print(std::ostream& os) const {
    os << "Monthly Statistics(" << current_ << ")";
}

//-------------------------------------------------------------------------------------------------

DailyStatistics::DailyStatistics(const std::vector<std::string> operations, long span,
                                 message::Message msg) :
    TemporalStatistics{operations, msg.size() / sizeof(double)},
    name_{msg.name()},
    current_{eckit::Date{msg.metadata().getString("date")}, span} {}

bool DailyStatistics::process_next(message::Message& msg) {
    ASSERT(name_ == msg.name());

    auto dateTime = currentDateTime(msg);

    ASSERT(current_.isWithin(dateTime.date()));

    updateStatistics(msg);

    dateTime = dateTime + static_cast<eckit::Second>(msg.metadata().getLong("timeStep"));

    return current_.isWithin(dateTime.date());
}

void DailyStatistics::resetPeriod(const message::Message& msg) {
    current_.reset(currentDateTime(msg).date());
}

void DailyStatistics::print(std::ostream &os) const {
    os << "Daily Statistics(" << current_ << ")";
}

//-------------------------------------------------------------------------------------------------

HourlyStatistics::HourlyStatistics(const std::vector<std::string> operations, long span,
                                   message::Message msg) :
    TemporalStatistics{operations, msg.size() / sizeof(double)},
    name_{msg.name()},
    current_{eckit::DateTime{eckit::Date{msg.metadata().getString("date")}, eckit::Time{0}},
             3600 * static_cast<eckit::Second>(span)} {}

bool HourlyStatistics::process_next(message::Message &msg) {
    ASSERT(name_ == msg.name());

    LOG_DEBUG_LIB(LibMultio) << *this << std::endl;

    auto dateTime = currentDateTime(msg);

    std::ostringstream os;
    os << dateTime << " is outside of current period " << current_ << std::endl;
    ASSERT_MSG(current_.isWithin(dateTime), os.str());

    updateStatistics(msg);

    dateTime = dateTime + static_cast<eckit::Second>(msg.metadata().getLong("timeStep"));

    return current_.isWithin(dateTime);
}

void HourlyStatistics::resetPeriod(const message::Message& msg) {
    current_.reset(currentDateTime(msg));
}

void HourlyStatistics::print(std::ostream &os) const {
    os << "Hourly Statistics(" << current_ << ")";
}

//-------------------------------------------------------------------------------------------------

}  // namespace action
}  // namespace multio

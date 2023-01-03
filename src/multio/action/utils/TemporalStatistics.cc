
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
    std::vector<OperationVar> stats;
    if (msg.metadata().has("precision")) {
        switch (multio::util::decodePrecisionTag(msg.metadata().getString("precision"))) {
            case multio::util::PrecisionTag::Float: {
                for (const auto& op : opNames) {
                    stats.push_back(make_operation<float>(op, msg.size()));
                }
            }; break;
            case multio::util::PrecisionTag::Double: {
                for (const auto& op : opNames) {
                    stats.push_back(make_operation<double>(op, msg.size()));
                }
            }; break;
            default:
                std::ostringstream oss;
                oss << "ERROR :: TemporalStatistics :: Unsupported datatype for "
                       "input message"
                    << std::endl
                    << "    file.....: " << __FILE__ << std::endl
                    << "    function.: " << __FUNCTION__ << std::endl
                    << "    line.....: " << __LINE__ << std::endl
                    << std::endl;
                throw eckit::BadValue{oss.str()};
        }
    }
    else {
        std::ostringstream oss;
        oss << "ERROR :: TemporalStatistics :: Unable to find \"precision\" "
               "keyword in input metadata"
            << std::endl
            << "    file.....: " << __FILE__ << std::endl
            << "    function.: " << __FUNCTION__ << std::endl
            << "    line.....: " << __LINE__ << std::endl
            << std::endl;
        throw eckit::SeriousBug{oss.str()};
    }
    return stats;
}

eckit::DateTime currentDateTime(const message::Message& msg) {
    eckit::Date startDate{eckit::Date{msg.metadata().getLong("startDate")}};
    auto startTime = msg.metadata().getLong("startTime");
    auto hour = startTime / 10000;
    auto minute = (startTime % 10000) / 100;
    eckit::DateTime startDateTime{startDate, eckit::Time{hour, minute, 0}};
    return startDateTime
         + static_cast<eckit::Second>(msg.metadata().getLong("step") * msg.metadata().getLong("timeStep"));
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
        std::visit(overloaded{[&msg](const std::unique_ptr<Operation<double>>& arg) {
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

        auto buf = std::visit(overloaded{[](const std::unique_ptr<Operation<double>>& arg) { return arg->compute(); },
                                         [](const std::unique_ptr<Operation<float>>& arg) { return arg->compute(); }},
                              stat);


        const auto& name
            = std::visit(overloaded{[](const std::unique_ptr<Operation<double>>& arg) { return arg->name(); },
                                    [](const std::unique_ptr<Operation<float>>& arg) { return arg->name(); }},
                         stat);

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
                       DateTimePeriod{eckit::DateTime{eckit::Date{msg.metadata().getLong("startDate")}, eckit::Time{0}},
                                      static_cast<eckit::Second>(3600 * span)},
                       msg} {}

void HourlyStatistics::print(std::ostream& os) const {
    os << "Hourly Statistics(" << current_ << ")";
}

//-------------------------------------------------------------------------------------------------

DailyStatistics::DailyStatistics(const std::vector<std::string> operations, long span, message::Message msg) :
    TemporalStatistics{operations,
                       DateTimePeriod{eckit::DateTime{eckit::Date{msg.metadata().getLong("startDate")}, eckit::Time{0}},
                                      static_cast<eckit::Second>(24 * 3600 * span)},
                       msg} {}

void DailyStatistics::print(std::ostream& os) const {
    os << "Daily Statistics(" << current_ << ")";
}

//-------------------------------------------------------------------------------------------------

namespace {
DateTimePeriod setMonthlyPeriod(long span, const message::Message& msg) {
    eckit::DateTime startPoint{eckit::Date{msg.metadata().getLong("startDate")}, eckit::Time{0}};

    auto totalSpan = startPoint.date().month() + span - 1;  // Make it zero-based

    auto endYear = startPoint.date().year() + totalSpan / 12;
    auto endMonth = totalSpan % 12 + 1;
    auto endDay = startPoint.date().day();

    ASSERT_MSG(endDay < 29, "No support for monthly period starting beyond day 28");

    eckit::DateTime endPoint{eckit::Date{endYear, endMonth, endDay}, eckit::Time{0}};

    return DateTimePeriod{startPoint, endPoint};
}
}  // namespace

MonthlyStatistics::MonthlyStatistics(const std::vector<std::string> operations, long span, message::Message msg) :
    TemporalStatistics{operations, setMonthlyPeriod(span, msg), msg} {}

void MonthlyStatistics::print(std::ostream& os) const {
    os << "Monthly Statistics(" << current_ << ")";
}

//-------------------------------------------------------------------------------------------------

}  // namespace action
}  // namespace multio

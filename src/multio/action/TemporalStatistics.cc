
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

TemporalStatistics::TemporalStatistics(const std::vector<std::string>& operations, long fld_sz) :
    opNames_{operations}, statistics_{reset_statistics(opNames_, fld_sz)} {}

void TemporalStatistics::updateStatistics(const message::Message& msg) {
    auto data_ptr = static_cast<const double*>(msg.payload().data());
    for(auto const& stat : statistics_) {
        stat->update(data_ptr, msg.size() / sizeof(double));
    }
}

eckit::Buffer TemporalStatistics::retrieveStatistics(const message::Message& msg) {
    // Place all statistics into a new buffer
    eckit::Buffer buf{msg.size() * statistics_.size()};
    char* ptr = buf;
    for(auto const& stat : statistics_) {
        auto const& res = stat->compute();
        std::memcpy(ptr, res.data(), msg.size());
        ptr += msg.size();
    }

    return buf;
}

//-------------------------------------------------------------------------------------------------

namespace  {
bool sameYearMonth(const eckit::Date& date1, const eckit::Date& date2) {
    return date1.month() == date2.month() && date1.year() == date2.year();
}
}

MonthlyStatistics::MonthlyStatistics(const std::vector<std::string> operations, long fld_sz) :
    TemporalStatistics(operations, fld_sz) {}

void MonthlyStatistics::process_next(message::Message& msg) {
    ASSERT(name_ == msg.name());

    if (sameYearMonth(current_, eckit::Date{msg.metadata().getString("date")})) {
        updateStatistics(msg);
    } else { // We start a new month

        // Place all statistics into a new buffer
        eckit::Buffer buf{retrieveStatistics(msg)};

        // Reset operations
        statistics_ = reset_statistics(opNames_, msg.size());

        // Update it with incoming fields
        updateStatistics(msg);

        // Update metadata
        auto metadata{msg.metadata()};
        metadata.set("operations", opNames_);

        // Reset message
        msg = message::Message{message::Message::Header{
                                   message::Message::Tag::Statistics, msg.source(),
                                   msg.destination(), msg.name(), msg.category(), msg.domainCount(),
                                   msg.globalSize(), msg.domain(), message::to_string(metadata)},
                               std::move(buf)};
    }
}

void MonthlyStatistics::print(std::ostream& os) const {
    os << "Monthly Statistics(" << current_ << ")" << std::endl;
}
}
}  // namespace multio

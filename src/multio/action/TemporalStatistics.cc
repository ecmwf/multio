
#include "TemporalStatistics.h"

#include "eckit/exception/Exceptions.h"

#include <cstring>
#include <iostream>

namespace multio {
namespace action {

TemporalStatistics::TemporalStatistics(const std::vector<std::string>& operations, long fld_sz) :
    opNames_{operations} {
    for (const auto& op : opNames_) {
        statistics_.push_back(make_operation(op, fld_sz));
    }
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

    double* data_ptr = static_cast<double*>(msg.payload().data());

    if (sameYearMonth(current_, eckit::Date{msg.metadata().getString("date")})) {
        for(auto const& stat : statistics_) {
            stat->update(data_ptr, msg.size() / sizeof(double));
        }
    } else {
        eckit::Buffer buf{msg.size() * statistics_.size()};
        for(auto const& stat : statistics_) {
            auto const& res = stat->compute();
            std::memcpy(buf.data(), res.data(), msg.size());
        }
        auto metadata{msg.metadata()};
        metadata.set("operations", opNames_);
        message::Message outMsg{};
        // Reset operations
    }
}

void MonthlyStatistics::print(std::ostream& os) const {
    os << "Monthly Statistics(" << current_ << ")" << std::endl;
}
}
}  // namespace multio

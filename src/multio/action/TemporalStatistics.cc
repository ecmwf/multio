
#include "TemporalStatistics.h"

#include "eckit/exception/Exceptions.h"

#include <iostream>

namespace multio {
namespace action {

TemporalStatistics::TemporalStatistics(const std::vector<std::string> operations, long fld_sz) {
    for (const auto& ops : operations) {
        statistics_[ops] = std::vector<double>(fld_sz);
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

void MonthlyStatistics::process_next(message::Message msg) {
    ASSERT(name_ == msg.name());

    if (sameYearMonth(current_, eckit::Date{msg.metadata().getString("date")})) {
        for(auto const& stat : statistics_) {
        }
    }
}

void MonthlyStatistics::print(std::ostream& os) const {
    os << "Monthly Statistics(" << current_ << ")" << std::endl;
}
}
}  // namespace multio

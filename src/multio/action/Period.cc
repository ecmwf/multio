
#include "Period.h"

namespace multio {
namespace action {

eckit::Date DatePeriod::endPoint() const {
    return startPoint_ + duration_;
}

void DatePeriod::print(std::ostream& os) const {
    os << "Period(" << startPoint_ << " to " << endPoint() << ")";
}

std::ostream& operator<<(std::ostream& os, const DatePeriod& a) {
    a.print(os);
    return os;
}

DatePeriod::DatePeriod(const eckit::Date& startPoint, long duration) :
    startPoint_{startPoint}, duration_{duration} {}

void DatePeriod::reset(const eckit::Date& startPoint) {
    startPoint_ = startPoint;
}

bool DatePeriod::samePeriod(const eckit::Date& dt) {
    ASSERT(startPoint_ <= dt);
    return dt <= endPoint();
}

//-------------------------------------------------------------------------------------------------

eckit::DateTime DateTimePeriod::endPoint() const {
    return startPoint_ + duration_;
}

void DateTimePeriod::print(std::ostream& os) const {
    os << "Period(" << startPoint_ << " to " << endPoint() << ")";
}

std::ostream& operator<<(std::ostream& os, const DateTimePeriod& a) {
    a.print(os);
    return os;
}

DateTimePeriod::DateTimePeriod(const eckit::DateTime& startPoint, eckit::Second duration) :
    startPoint_{startPoint}, duration_{duration} {
    std::cout << " ======== Date-time period created with duration " << duration_ << std::endl;
}

void DateTimePeriod::reset(const eckit::DateTime& current) {
    startPoint_ = current;
}

bool DateTimePeriod::samePeriod(const eckit::DateTime& dt) {
    std::cout << "Comparing current: " << dt << " with end point: " << endPoint() << std::endl;
    ASSERT(startPoint_ <= dt);
    return dt <= endPoint();
}

}  // namespace action
}  // namespace multio

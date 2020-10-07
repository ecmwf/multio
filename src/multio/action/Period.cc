
#include "Period.h"

namespace multio {
namespace action {

DatePeriod::DatePeriod(const eckit::Date& startPoint, long duration) :
    startPoint_{startPoint}, duration_{duration} {}

void DatePeriod::reset(const eckit::Date& startPoint) {
    startPoint_ = startPoint;
}

bool DatePeriod::isWithin(const eckit::Date& dt) {
    ASSERT(startPoint_ <= dt);
    return dt <= endPoint();
}

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

//-------------------------------------------------------------------------------------------------

DateTimePeriod::DateTimePeriod(const eckit::DateTime& startPoint, eckit::Second duration) :
    startPoint_{startPoint}, duration_{duration} {}

void DateTimePeriod::reset(const eckit::DateTime& current) {
    startPoint_ = current;
}

bool DateTimePeriod::isWithin(const eckit::DateTime& dt) {
    ASSERT(startPoint_ <= dt);
    return dt <= endPoint();
}

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

}  // namespace action
}  // namespace multio

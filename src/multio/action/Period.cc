
#include "Period.h"

#include "multio/LibMultio.h"

namespace multio {
namespace action {

DatePeriod::DatePeriod(const eckit::Date& startPoint, long duration) :
    startPoint_{startPoint}, duration_{duration} {}

void DatePeriod::reset(const eckit::Date& startPoint) {
    startPoint_ = startPoint;
}

bool DatePeriod::isWithin(const eckit::Date& dt) {
    ASSERT(startPoint_ <= dt);
    auto ret = (dt <= endPoint());
    eckit::Log::info() << " ------ Is " << dt << " within " << *this << "? -- "
                             << (ret ? "yes" : "no") << std::endl;
    return ret;
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
    startPoint_{startPoint}, endPoint_{startPoint_ + duration} {}

DateTimePeriod::DateTimePeriod(const eckit::DateTime& startPoint, const eckit::DateTime& endPoint) :
    startPoint_{startPoint}, endPoint_{endPoint} {}

void DateTimePeriod::reset(const eckit::DateTime& current) {
    auto duration = endPoint_ - startPoint_;
    startPoint_ = current;
    endPoint_ = startPoint_ + duration;
}

void DateTimePeriod::reset(const eckit::DateTime& startPoint, const eckit::DateTime& endPoint) {
    startPoint_ = startPoint;
    endPoint_ = endPoint;
}

bool DateTimePeriod::isWithin(const eckit::DateTime& dt) {
    ASSERT(startPoint_ <= dt);
    auto ret = (dt <= endPoint() + eckit::Second{1.0});
    eckit::Log::info() << " ------ Is " << dt << " within " << *this << "? -- "
                       << (ret ? "yes" : "no") << std::endl;
    return ret;
}

eckit::DateTime DateTimePeriod::endPoint() const {
    return endPoint_;
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

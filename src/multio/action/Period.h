
#ifndef multio_server_actions_Period_H
#define multio_server_actions_Period_H

#include "eckit/types/DateTime.h"

namespace multio {
namespace action {

class DatePeriod {
    eckit::Date startPoint_;
    long duration_;

    eckit::Date endPoint() const;
    void print(std::ostream& os) const;

    friend std::ostream& operator<<(std::ostream& os, const DatePeriod& a);

public:
    DatePeriod(const eckit::Date& startPoint, long duration);
    void reset(const eckit::Date& startPoint);
    bool samePeriod(const eckit::Date& dt);
};

class DateTimePeriod {
    eckit::DateTime startPoint_;
    const eckit::Second duration_;

    eckit::DateTime endPoint() const;
    void print(std::ostream& os) const;

    friend std::ostream& operator<<(std::ostream& os, const DateTimePeriod& a);

public:
    DateTimePeriod(const eckit::DateTime& startPoint, eckit::Second duration);
    void reset(const eckit::DateTime& current);
    bool samePeriod(const eckit::DateTime& dt);
};

}
}  // namespace multio

#endif // multio_server_actions_Period_H

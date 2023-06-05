#pragma once

#include "eckit/types/DateTime.h"

namespace multio::action {
class DateTimePeriod {
public:
    DateTimePeriod(const std::string& name);
    DateTimePeriod(const eckit::DateTime& startPoint, eckit::Second duration);
    DateTimePeriod(const eckit::DateTime& startPoint, const eckit::DateTime& endPoint);


    void reset(const eckit::DateTime& current);
    void reset(const eckit::DateTime& startPoint, const eckit::DateTime& endPoint);

    bool isWithin(const eckit::DateTime& dt);

    long timeSpanInSeconds() const;
    eckit::DateTime endPoint() const;
    eckit::DateTime startPoint() const;
    void dump(const std::string& name) const;

private:
    eckit::DateTime startPoint_;
    eckit::DateTime endPoint_;

    void print(std::ostream& os) const;

    friend std::ostream& operator<<(std::ostream& os, const DateTimePeriod& a);
};

}  // namespace multio::action

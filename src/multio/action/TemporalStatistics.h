
#ifndef multio_server_actions_TemporalStatistics_H
#define multio_server_actions_TemporalStatistics_H

#include <map>
#include <string>

#include "eckit/types/DateTime.h"

#include "multio/action/Operation.h"
#include "multio/message/Message.h"

namespace multio {
namespace action {

class DatePeriod {
    eckit::Date startPoint_;
    long duration_;
    eckit::Date endPoint() const {
        return startPoint_ + duration_;
    }
    void print(std::ostream& os) const {
        os << "Period(" << startPoint_ << " to " << endPoint() << ")";
    }
    friend std::ostream& operator<<(std::ostream& os, const DatePeriod& a) {
        a.print(os);
        return os;
    }

public:
    DatePeriod(const eckit::Date& startPoint, long duration) :
        startPoint_{startPoint}, duration_{duration} {}
    void reset(const eckit::Date& startPoint) { startPoint_ = startPoint; }
    bool samePeriod(const eckit::Date& dt) {
        ASSERT(startPoint_ <= dt);
        return dt <= endPoint();
    }
};

class DateTimePeriod {
    eckit::DateTime startPoint_;
    const eckit::Second duration_;
    eckit::DateTime endPoint() const {
        return startPoint_ + duration_;
    }
    void print(std::ostream& os) const {
        os << "Period(" << startPoint_ << " to " << endPoint() << ")";
    }
    friend std::ostream& operator<<(std::ostream& os, const DateTimePeriod& a) {
        a.print(os);
        return os;
    }

public:
    DateTimePeriod(const eckit::DateTime& startPoint, eckit::Second duration) :
        startPoint_{startPoint}, duration_{duration} {
        std::cout << " ======== Date-time period created with duration " << duration_ << std::endl;
    }
    void reset(const eckit::DateTime& current) {
        startPoint_ = current;
    }
    bool samePeriod(const eckit::DateTime& dt) {
        std::cout << "Comparing current: " << dt << " with end point: " << endPoint() << std::endl;
        ASSERT(startPoint_ <= dt);
        return dt <= endPoint();
    }
};

class TemporalStatistics {
public:
    static std::unique_ptr<TemporalStatistics> build(const std::string& unit, long span,
                                                     const std::vector<std::string>& operations,
                                                     const message::Message& msg);

    TemporalStatistics(const std::vector<std::string>& operations, size_t sz);
    virtual ~TemporalStatistics() = default;

    bool process(message::Message& msg);
    std::map<std::string, eckit::Buffer> compute(const message::Message& msg);
    void reset(const message::Message& msg);

protected:
    std::vector<std::string> opNames_;
    std::vector<std::unique_ptr<Operation>> statistics_;

    void updateStatistics(const message::Message& msg);

private:
    virtual bool process_next(message::Message& msg) = 0;

    virtual void resetPeriod(const message::Message& msg) = 0;

    virtual void print(std::ostream& os) const = 0;

    friend std::ostream& operator<<(std::ostream& os, const TemporalStatistics& a) {
        a.print(os);
        return os;
    }
};

//-------------------------------------------------------------------------------------------------

class MonthlyStatistics : public TemporalStatistics {

    std::string name_;
    DatePeriod current_;

public:
    MonthlyStatistics(const std::vector<std::string> operations, long span, message::Message msg);

    bool process_next(message::Message &msg) override;

    void resetPeriod(const message::Message& msg) override;

    void print(std::ostream &os) const override;
};

//-------------------------------------------------------------------------------------------------

class DailyStatistics : public TemporalStatistics {

    std::string name_;
    DatePeriod current_;

public:
    DailyStatistics(const std::vector<std::string> operations, long span, message::Message msg);

    bool process_next(message::Message& msg) override;

    void resetPeriod(const message::Message& msg) override;

    void print(std::ostream &os) const override;
};

//-------------------------------------------------------------------------------------------------

class HourlyStatistics : public TemporalStatistics {

    std::string name_;
    DateTimePeriod current_;

public:
    HourlyStatistics(const std::vector<std::string> operations, long span, message::Message msg);

    bool process_next(message::Message& msg) override;

    void resetPeriod(const message::Message& msg) override;

    void print(std::ostream &os) const override;
};

}
}  // namespace multio

#endif // TEMPORALSTATISTICS_H

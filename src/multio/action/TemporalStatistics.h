
#ifndef multio_server_actions_TemporalStatistics_H
#define multio_server_actions_TemporalStatistics_H

#include <map>
#include <string>

#include "eckit/types/DateTime.h"

#include "multio/action/Operation.h"
#include "multio/message/Message.h"
#include "multio/action/Period.h"

namespace multio {
namespace action {

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

#endif // multio_server_actions_TemporalStatistics_H

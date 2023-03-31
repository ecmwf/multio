#pragma once

#include <map>
#include <memory>
#include <string>

#include "multio/action/statistics/Operation.h"
#include "multio/action/statistics/Period.h"
#include "multio/action/statistics/StatisticsOptions.h"
#include "multio/message/Message.h"

namespace multio {
namespace action {


class TemporalStatistics {
public:
    static std::unique_ptr<TemporalStatistics> build(const std::string& unit, long span,
                                                     const std::vector<std::string>& operations,
                                                     const message::Message& msg, 
                                                     const StatisticsOptions& options );

    TemporalStatistics(const std::vector<std::string>& operations, const DateTimePeriod& period,
                       const message::Message& msg, const StatisticsOptions& options, long step);

    virtual ~TemporalStatistics() = default;

    bool process(message::Message& msg);
    std::map<std::string, eckit::Buffer> compute(const message::Message& msg);
    std::string stepRange(long step);
    const DateTimePeriod& current() const;
    void reset(const message::Message& msg);
    long startStep() const { return prevStep_; };

protected:
    std::string name_;
    DateTimePeriod current_;
    const StatisticsOptions& options_;

    void updateStatistics(const message::Message& msg);

private:
    virtual bool process_next(message::Message& msg);

    virtual void resetPeriod(const message::Message& msg);

    virtual void print(std::ostream& os) const = 0;

    friend std::ostream& operator<<(std::ostream& os, const TemporalStatistics& a) {
        a.print(os);
        return os;
    }

    std::vector<std::string> opNames_;
    std::vector<OperationVar> statistics_;
    long prevStep_ = 0;
};

//-------------------------------------------------------------------------------------------------

class HourlyStatistics : public TemporalStatistics {
public:
    HourlyStatistics(const std::vector<std::string> operations, long span, message::Message msg,
                     const StatisticsOptions& options, long step);

    void print(std::ostream& os) const override;
};

//-------------------------------------------------------------------------------------------------

class DailyStatistics : public TemporalStatistics {
public:
    DailyStatistics(const std::vector<std::string> operations, long span, message::Message msg,
                    const StatisticsOptions& options, long step);

    void print(std::ostream& os) const override;
};

//-------------------------------------------------------------------------------------------------

class MonthlyStatistics : public TemporalStatistics {
    long span_;

public:
    MonthlyStatistics(const std::vector<std::string> operations, long span, message::Message msg,
                      const StatisticsOptions& options, long step);
    void resetPeriod(const message::Message& msg) override;
    void print(std::ostream& os) const override;
};

//-------------------------------------------------------------------------------------------------

}  // namespace action
}  // namespace multio

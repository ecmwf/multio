
#ifndef multio_server_actions_TemporalStatistics_H
#define multio_server_actions_TemporalStatistics_H

#include <map>
#include <string>

#include "eckit/types/DateTime.h"
#include "multio/action/utils/Operation.h"
#include "multio/action/utils/Period.h"
#include "multio/message/Message.h"

namespace multio {
namespace action {

class TemporalStatistics {
 public:
  static std::unique_ptr<TemporalStatistics> build(
      const std::string& unit, long span,
      const std::vector<std::string>& operations, const message::Message& msg);

  TemporalStatistics(const std::string& name, const DateTimePeriod& period,
                     const std::vector<std::string>& operations, size_t sz);
  virtual ~TemporalStatistics() = default;

  bool process(message::Message& msg);
  std::map<std::string, eckit::Buffer> compute(const message::Message& msg);
  std::string stepRange(long step);
  const DateTimePeriod& current() const;
  void reset(const message::Message& msg);

 protected:
  std::string name_;
  DateTimePeriod current_;

  void updateStatistics(const message::Message& msg);

 private:
  virtual bool process_next(message::Message& msg);

  virtual void resetPeriod(const message::Message& msg);

  virtual void print(std::ostream& os) const = 0;

  friend std::ostream& operator<<(std::ostream& os,
                                  const TemporalStatistics& a) {
    a.print(os);
    return os;
  }

  std::vector<std::string> opNames_;
  std::vector<std::unique_ptr<Operation>> statistics_;
  long prevStep_ = 0;
};

//-------------------------------------------------------------------------------------------------

class HourlyStatistics : public TemporalStatistics {
 public:
  HourlyStatistics(const std::vector<std::string> operations, long span,
                   message::Message msg);

  void print(std::ostream& os) const override;
};

//-------------------------------------------------------------------------------------------------

class DailyStatistics : public TemporalStatistics {
 public:
  DailyStatistics(const std::vector<std::string> operations, long span,
                  message::Message msg);

  void print(std::ostream& os) const override;
};

//-------------------------------------------------------------------------------------------------

class MonthlyStatistics : public TemporalStatistics {
 public:
  MonthlyStatistics(const std::vector<std::string> operations, long span,
                    message::Message msg);

  void print(std::ostream& os) const override;
};

//-------------------------------------------------------------------------------------------------

}  // namespace action
}  // namespace multio

#endif  // multio_server_actions_TemporalStatistics_H

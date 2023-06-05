#pragma once

#include <string>
#include "eckit/types/DateTime.h"

#include "MovingWindow.h"
#include "Operations.h"
#include "PeriodUpdater.h"
#include "StatisticsConfiguration.h"
#include "StatisticsIO.h"
#include "multio/message/Message.h"


namespace multio ::action {

class TemporalStatistics {
public:
    using op = std::vector<std::unique_ptr<OperationBase>>;

    op::iterator begin() { return statistics_.begin(); };
    op::iterator end() { return statistics_.end(); };

    static std::unique_ptr<TemporalStatistics> build(const std::shared_ptr<PeriodUpdater>& periodUpdater,
                                                     const std::vector<std::string>& operations,
                                                     const message::Message& msg,
                                                     std::shared_ptr<StatisticsIO>& IOmanager,
                                                     const StatisticsConfiguration& cfg);

    TemporalStatistics(const std::shared_ptr<PeriodUpdater>& periodUpdater, const std::vector<std::string>& operations,
                       const message::Message& msg, std::shared_ptr<StatisticsIO>& IOmanager,
                       const StatisticsConfiguration& cfg);

    bool isEndOfWindow(message::Message& msg, const StatisticsConfiguration& cfg);

    void updateData(message::Message& msg, const StatisticsConfiguration& cfg);
    void updateWindow(const message::Message& msg, const StatisticsConfiguration& cfg);

    void dump(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsConfiguration& cfg) const;

    const MovingWindow& win() const;

    void print(std::ostream& os) const;

private:
    const std::shared_ptr<PeriodUpdater>& periodUpdater_;
    MovingWindow window_;
    std::vector<std::unique_ptr<OperationBase>> statistics_;

    friend std::ostream& operator<<(std::ostream& os, const TemporalStatistics& a) {
        a.print(os);
        return os;
    }
};

}  // namespace multio::action

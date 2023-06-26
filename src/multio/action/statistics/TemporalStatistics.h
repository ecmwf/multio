#pragma once

#include <string>

#include "multio/message/Message.h"

#include "OperationWindow.h"
#include "Operations.h"
#include "PeriodUpdater.h"
#include "StatisticsConfiguration.h"
#include "StatisticsIO.h"


namespace multio::action {

class TemporalStatistics {
public:
    using op = std::vector<std::unique_ptr<Operation>>;

    op::iterator begin() { return statistics_.begin(); };
    op::iterator end() { return statistics_.end(); };

    TemporalStatistics(const std::shared_ptr<PeriodUpdater>& periodUpdater, const std::vector<std::string>& operations,
                       const message::Message& msg, std::shared_ptr<StatisticsIO>& IOmanager,
                       const StatisticsConfiguration& cfg);

    bool isEndOfWindow(message::Message& msg, const StatisticsConfiguration& cfg);

    void updateData(message::Message& msg, const StatisticsConfiguration& cfg);
    void updateWindow(const message::Message& msg, const StatisticsConfiguration& cfg);

    void dump(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsConfiguration& cfg) const;

    const OperationWindow& cwin() const;
    OperationWindow& win();

    void print(std::ostream& os) const;

private:
    const std::shared_ptr<PeriodUpdater>& periodUpdater_;
    OperationWindow window_;
    std::vector<std::unique_ptr<Operation>> statistics_;

    friend std::ostream& operator<<(std::ostream& os, const TemporalStatistics& a) {
        a.print(os);
        return os;
    }
};

}  // namespace multio::action

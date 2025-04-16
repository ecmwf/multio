#pragma once

#include <string>
#include <optional>

#include "multio/message/Message.h"

#include "OperationWindow.h"
#include "Operations.h"
#include "PeriodUpdaters.h"
#include "StatisticsIO.h"
#include "multio/action/statistics-mtg2/cfg/StatisticsConfiguration.h"


namespace multio::action::statistics_mtg2 {

class TemporalStatistics {
public:
    using op = std::vector<std::unique_ptr<Operation>>;

    op::iterator begin() { return statistics_.begin(); };
    op::iterator end() { return statistics_.end(); };

    TemporalStatistics(const std::string& output_freq, const std::vector<std::string>& operations,
                       const message::Message& msg, std::shared_ptr<StatisticsIO>& IOmanager,
                       const StatisticsConfiguration& cfg);

    TemporalStatistics(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsOptions& opt);

    bool isOutsideWindow(message::Message& msg, const StatisticsConfiguration& cfg);

    void updateData(message::Message& msg, const StatisticsConfiguration& cfg);
    void updateWindow(const message::Message& msg, const StatisticsConfiguration& cfg);

    void dump(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsOptions& opt) const;

    const OperationWindow& cwin() const;
    OperationWindow& win();

    StatisticsConfiguration& config();
    message::Metadata& metadata();

    void print(std::ostream& os) const;

private:
    std::unique_ptr<PeriodUpdater> periodUpdater_;
    OperationWindow window_;
    std::vector<std::unique_ptr<Operation>> statistics_;
    std::optional<StatisticsConfiguration> config_;
    std::optional<message::Metadata> metadata_;

    friend std::ostream& operator<<(std::ostream& os, const TemporalStatistics& a) {
        a.print(os);
        return os;
    }
};

}  // namespace multio::action::statistics_mtg2

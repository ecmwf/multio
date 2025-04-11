#include "TemporalStatistics.h"

#include <iostream>

#include "eckit/types/DateTime.h"

#include "multio/LibMultio.h"

#include "TimeUtils.h"

namespace multio::action::statistics {


TemporalStatistics::TemporalStatistics(const std::string& output_freq, const std::vector<std::string>& operations,
                                       const message::Message& msg, std::shared_ptr<StatisticsIO>& IOmanager,
                                       const StatisticsConfiguration& cfg) :
    periodUpdater_{make_period_updater(output_freq, cfg)},
    window_{make_window(periodUpdater_, cfg)},
    statistics_{make_operations(operations, msg, IOmanager, window_, cfg)} {}

TemporalStatistics::TemporalStatistics(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsOptions& opt) :
    periodUpdater_{load_period_updater(IOmanager, opt)},
    window_{load_window(IOmanager, opt)},
    statistics_{load_operations(IOmanager, window_, opt)} {
    LOG_DEBUG_LIB(LibMultio) << opt.logPrefix() << " *** Load restart files" << std::endl;
}


void TemporalStatistics::dump(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsOptions& opt) const {
    LOG_DEBUG_LIB(LibMultio) << opt.logPrefix() << " *** Dump restart files" << std::endl;
    IOmanager->pushDir("periodUpdater");
    IOmanager->createCurrentDir();
    periodUpdater_->dump(IOmanager, opt);
    IOmanager->popDir();
    IOmanager->pushDir("operationWindow");
    IOmanager->createCurrentDir();
    window_.dump(IOmanager, opt);
    IOmanager->popDir();
    IOmanager->pushDir("operations");
    IOmanager->createCurrentDir();
    for (auto& stat : statistics_) {
        stat->dump(IOmanager, opt);
    }
    IOmanager->popDir();
}

void TemporalStatistics::updateData(message::Message& msg, const StatisticsConfiguration& cfg) {
    LOG_DEBUG_LIB(multio::LibMultio) << cfg.logPrefix() << " *** Update Data" << std::endl;
    window_.updateData(currentDateTime(msg, cfg));
    for (auto& stat : statistics_) {
        stat->updateData(msg.payload().data(), msg.size(), cfg);
    }
}

void TemporalStatistics::updateWindow(const message::Message& msg, const StatisticsConfiguration& cfg) {
    LOG_DEBUG_LIB(multio::LibMultio) << cfg.logPrefix() << " *** Update Window " << std::endl;
    window_.updateWindow(window_.endPoint(), periodUpdater_->updateWinEndTime(window_.endPoint()));
    for (auto& stat : statistics_) {
        stat->updateWindow(msg.payload().data(), msg.size(), msg, cfg);
    }
}

bool TemporalStatistics::isEndOfWindow(message::Message& msg, const StatisticsConfiguration& cfg) {
    LOG_DEBUG_LIB(::multio::LibMultio) << cfg.logPrefix() << " *** Check end of Window " << std::endl;
    return !window_.isWithin(nextDateTime(msg, cfg));
}

const OperationWindow& TemporalStatistics::cwin() const {
    return window_;
}

OperationWindow& TemporalStatistics::win() {
    return window_;
}

void TemporalStatistics::print(std::ostream& os) const {
    os << "Temporal Statistics";
}

}  // namespace multio::action::statistics

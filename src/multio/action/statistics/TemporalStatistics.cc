#include "TemporalStatistics.h"

#include <algorithm>
#include <cstring>
#include <fstream>
#include <iostream>

#include "TimeUtils.h"
#include "eckit/exception/Exceptions.h"
#include "multio/LibMultio.h"
#include "multio/util/PrecisionTag.h"

namespace multio::action {

std::unique_ptr<TemporalStatistics> TemporalStatistics::build(const std::shared_ptr<PeriodUpdater>& periodUpdater,
                                                              const std::vector<std::string>& operations,
                                                              const message::Message& msg,
                                                              std::shared_ptr<StatisticsIO>& IOmanager,
                                                              const StatisticsConfiguration& cfg) {


    return std::make_unique<TemporalStatistics>(periodUpdater, operations, msg, IOmanager, cfg);
}


TemporalStatistics::TemporalStatistics(const std::shared_ptr<PeriodUpdater>& periodUpdater,
                                       const std::vector<std::string>& operations, const message::Message& msg,
                                       std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsConfiguration& cfg) :
    periodUpdater_{periodUpdater},
    window_{periodUpdater_->initPeriod(msg, IOmanager, cfg)},
    statistics_{make_operations(operations, msg, IOmanager, window_, cfg)} {}


void TemporalStatistics::dump(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsConfiguration& cfg) const {
    LOG_DEBUG_LIB(LibMultio) << cfg.logPrefix() << " *** Dump restart file" << std::endl;
    IOmanager->setSuffix(periodUpdater_->name());
    window_.dump(IOmanager, cfg);
    for (auto& stat : statistics_) {
        stat->dump(IOmanager, cfg);
    }
    return;
}

void TemporalStatistics::updateData(message::Message& msg, const StatisticsConfiguration& cfg) {
    LOG_DEBUG_LIB(multio::LibMultio) << cfg.logPrefix() << " *** Update ";
    window_.updateData(currentDateTime(msg, cfg));
    for (auto& stat : statistics_) {
        stat->updateData(msg.payload().data(), msg.size());
    }
    return;
}

void TemporalStatistics::updateWindow(const message::Message& msg, const StatisticsConfiguration& cfg) {
    LOG_DEBUG_LIB(::multio::LibMultio) << cfg.logPrefix() << " *** Reset window " << std::endl;
    eckit::DateTime startPoint = periodUpdater_->updatePeriodStart(msg, cfg);
    eckit::DateTime endPoint = periodUpdater_->updatePeriodEnd(msg, cfg);
    window_.updateWindow(startPoint, endPoint);
    for (auto& stat : statistics_) {
        stat->updateWindow(msg.payload().data(), msg.size());
    }
    return;
}

bool TemporalStatistics::isEndOfWindow(message::Message& msg, const StatisticsConfiguration& cfg) {
    LOG_DEBUG_LIB(::multio::LibMultio) << cfg.logPrefix() << " *** Check end of window " << std::endl;
    return !window_.isWithin(nextDateTime(msg, cfg));
}

const MovingWindow& TemporalStatistics::win() const {
    return window_;
}

void TemporalStatistics::print(std::ostream& os) const {
    os << "Temporal Statistics";
}

}  // namespace multio::action

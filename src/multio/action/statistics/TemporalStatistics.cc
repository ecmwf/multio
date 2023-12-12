#include "TemporalStatistics.h"

#include <iostream>

#include "eckit/types/DateTime.h"

#include "multio/LibMultio.h"

#include "TimeUtils.h"

namespace multio::action {

namespace {
bool solverResetAccumulatedFields(const message::Message& msg, const StatisticsConfiguration& cfg) {

    if (cfg.solverResetAccumulatedFields() == "hour") {
        return isBeginningOfHour(msg, cfg);
    }
    if (cfg.solverResetAccumulatedFields() == "day") {
        return isBeginningOfDay(msg, cfg);
    }
    if (cfg.solverResetAccumulatedFields() == "month") {
        return isBeginningOfMonth(msg, cfg);
    }
    if (cfg.solverResetAccumulatedFields() == "year") {
        return isBeginningOfYear(msg, cfg);
    }
    if (cfg.solverResetAccumulatedFields() == "never") {
        return false;
    }

    std::ostringstream os;
    os << "Invalid reset period of accumulated fields :: " << cfg.solverResetAccumulatedFields() << std::endl;
    throw eckit::UserError(os.str(), Here());
}
}  // namespace


TemporalStatistics::TemporalStatistics(const std::shared_ptr<PeriodUpdater>& periodUpdater,
                                       const std::vector<std::string>& operations, const message::Message& msg,
                                       std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsConfiguration& cfg) :
    periodUpdater_{periodUpdater},
    window_{periodUpdater_->initPeriod(msg, IOmanager, cfg)},
    statistics_{make_operations(operations, msg, IOmanager, window_, cfg)} {}


void TemporalStatistics::dump(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsConfiguration& cfg) const {
    LOG_DEBUG_LIB(LibMultio) << cfg.logPrefix() << " *** Dump restart files" << std::endl;
    window_.dump(IOmanager, cfg);
    for (auto& stat : statistics_) {
        stat->dump(IOmanager, cfg);
    }
    IOmanager->flush();
    return;
}

void TemporalStatistics::updateData(message::Message& msg, const StatisticsConfiguration& cfg) {
    LOG_DEBUG_LIB(multio::LibMultio) << cfg.logPrefix() << " *** Update Data" << std::endl;
    window_.updateData(currentDateTime(msg, cfg));
    for (auto& stat : statistics_) {
        stat->updateData(msg.payload().data(), msg.size());
    }
    return;
}

void TemporalStatistics::updateWindow(const message::Message& msg, const StatisticsConfiguration& cfg) {
    LOG_DEBUG_LIB(::multio::LibMultio) << cfg.logPrefix() << " *** Update Window " << std::endl;
    bool sraf = solverResetAccumulatedFields(msg, cfg);
    window_.updateWindow(periodUpdater_->updatePeriodStart(msg, cfg), periodUpdater_->updatePeriodEnd(msg, cfg));
    for (auto& stat : statistics_) {
        sraf ? stat->updateWindow() : stat->updateWindow(msg.payload().data(), msg.size());
    }
    return;
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

}  // namespace multio::action

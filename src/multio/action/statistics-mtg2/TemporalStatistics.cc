#include "TemporalStatistics.h"

#include <iostream>

#include "eckit/types/DateTime.h"

#include "multio/LibMultio.h"

#include "TimeUtils.h"

namespace multio::action::statistics_mtg2 {


TemporalStatistics::TemporalStatistics(const std::string& output_freq, const std::vector<std::string>& operations,
                                       const message::Message& msg, std::shared_ptr<StatisticsIO>& IOmanager,
                                       const StatisticsConfiguration& cfg) :
    periodUpdater_{make_period_updater(output_freq, cfg)},
    window_{make_window(periodUpdater_, cfg)},
    statistics_{make_operations(operations, msg, IOmanager, window_, cfg)},
    metadata_{msg.metadata()} {}

// TODO: Dump and load the (relevant) metadata as well!
TemporalStatistics::TemporalStatistics(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsOptions& opt) :
    periodUpdater_{load_period_updater(IOmanager, opt)},
    window_{load_window(IOmanager, opt)},
    statistics_{load_operations(IOmanager, window_, opt)} {

    // Read the metadata from disk!
    IOmanager->pushDir("metadata");
    std::string mdFilename = IOmanager->getCurrentDir() + "/metadata.json";
    std::ifstream mdFile(mdFilename);
    std::stringstream buffer;
    buffer << mdFile.rdbuf();
    metadata_ = multio::message::metadataFromYAML(buffer.str());
    mdFile.close();
    IOmanager->popDir();

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

    // Write the metadata to disk!
    IOmanager->pushDir("metadata");
    IOmanager->createCurrentDir();
    std::string mdFilename = IOmanager->getCurrentDir() + "/metadata.json";
    std::ofstream mdFile(mdFilename);
    mdFile << *metadata_;
    mdFile.close();
    IOmanager->popDir();
    return;
}

void TemporalStatistics::updateData(message::Message& msg, const StatisticsConfiguration& cfg) {
    LOG_DEBUG_LIB(multio::LibMultio) << cfg.logPrefix() << " *** Update Data" << std::endl;
    window_.updateData(currentDateTime(msg, cfg));
    for (auto& stat : statistics_) {
        stat->updateData(msg.payload().data(), msg.size(), cfg);
    }
    return;
}

void TemporalStatistics::updateWindow(const message::Message& msg, const StatisticsConfiguration& cfg) {
    LOG_DEBUG_LIB(multio::LibMultio) << cfg.logPrefix() << " *** Update Window " << std::endl;
    window_.updateWindow(window_.endPoint(), periodUpdater_->updateWinEndTime(window_.endPoint()));
    for (auto& stat : statistics_) {
        stat->updateWindow(msg.payload().data(), msg.size(), msg, cfg);
    }

    return;
}

bool TemporalStatistics::isOutsideWindow(message::Message& msg, const StatisticsConfiguration& cfg) {
    LOG_DEBUG_LIB(::multio::LibMultio) << cfg.logPrefix() << " *** Check outside Window " << std::endl;
    return !window_.isWithin(currentDateTime(msg, cfg));
}

const OperationWindow& TemporalStatistics::cwin() const {
    return window_;
}

OperationWindow& TemporalStatistics::win() {
    return window_;
}

message::Metadata& TemporalStatistics::metadata() {
    if (metadata_) {
        return *metadata_;
    }
    throw eckit::SeriousBug("Metadata is not set!", Here());
}

void TemporalStatistics::print(std::ostream& os) const {
    os << "Temporal Statistics";
}

}  // namespace multio::action::statistics_mtg2

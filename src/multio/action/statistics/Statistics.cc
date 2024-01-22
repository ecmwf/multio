/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#include "Statistics.h"

#include <algorithm>
#include <unordered_map>


#include "TemporalStatistics.h"
#include "eckit/exception/Exceptions.h"
#include "multio/LibMultio.h"
#include "multio/message/Message.h"
#include "multio/util/ScopedTimer.h"

namespace multio::action {

Statistics::Statistics(const ComponentConfiguration& compConf) :
    ChainedAction{compConf},
    cfg_{compConf},
    operations_{compConf.parsedConfig().getStringVector("operations")},
    periodUpdater_{make_period_updater(compConf.parsedConfig().getString("output-frequency"))},
    IOmanager_{StatisticsIOFactory::instance().build(cfg_.restartLib(), cfg_.restartPath(), cfg_.restartPrefix())} {}


void Statistics::DumpRestart() {
    if (cfg_.writeRestart()) {
        IOmanager_->reset();
        IOmanager_->setSuffix(periodUpdater_->name());
        for (auto it = fieldStats_.begin(); it != fieldStats_.end(); it++) {
            LOG_DEBUG_LIB(LibMultio) << "Restart for field with key :: " << it->first << ", "
                                     << it->second->cwin().currPointInSteps() << std::endl;
            IOmanager_->setCurrStep(it->second->cwin().currPointInSteps());
            IOmanager_->setPrevStep(it->second->cwin().lastFlushInSteps());
            IOmanager_->setKey(it->first);
            it->second->dump(IOmanager_, cfg_);
            it->second->win().updateFlush();
        }
    }
}


std::string Statistics::generateKey(const message::Message& msg) const {
    std::ostringstream os;
    os << msg.metadata().getString("param", "") << "-" << msg.metadata().getString("paramId", "") << "-"
       << msg.metadata().getLong("level", 0) << "-" << msg.metadata().getLong("levelist", 0) << "-"
       << msg.metadata().getString("levtype", "unknown") << "-" << msg.metadata().getString("gridType", "unknown")
       << "-" << msg.metadata().getString("precision", "unknown") << "-"
       << std::to_string(std::hash<std::string>{}(msg.source()));
    LOG_DEBUG_LIB(LibMultio) << "Generating key for the field :: " << os.str() << std::endl;
    return os.str();
}


message::Metadata Statistics::outputMetadata(const message::Metadata& inputMetadata, const StatisticsConfiguration& cfg,
                                             const std::string& key) const {
    auto& win = fieldStats_.at(key)->cwin();
    if (win.endPointInSeconds() % 3600 != 0L) {
        std::ostringstream os;
        os << "Step in seconds needs to be a multiple of 3600 :: " << fieldStats_.at(key)->win().endPointInSeconds()
           << std::endl;
        throw eckit::SeriousBug(os.str(), Here());
    }
    auto md = inputMetadata;

    // util::DateTimeDiff lastPointsDiff = win.lastPointsDiff();

    // md.set("sampleIntervalUnit", std::string{util::timeUnitToChar(lastPointsDiff.unit)});
    // md.set("sampleInterval", lastPointsDiff.diff);

    md.set("sampleIntervalInSeconds", win.lastPointsDiffInSeconds());

    md.set("startDate", win.epochPoint().date().yyyymmdd());
    md.set("startTime", win.epochPoint().time().hhmmss());
    md.set("step-frequency", win.timeSpanInSteps());

    // md.set("timeSpanInHours", win.timeSpanInHours());
    // md.set("stepRange", win.stepRange());

    md.set("previousDate", win.creationPoint().date().yyyymmdd());
    md.set("previousTime", win.creationPoint().time().hhmmss());
    md.set("currentDate", win.endPoint().date().yyyymmdd());
    md.set("currentTime", win.endPoint().time().hhmmss());

    // md.set("stepInHours", win.endPointInHours());
    // md.set("stepRangeInHours", win.stepRangeInHours());

    return md;
}


void Statistics::executeImpl(message::Message msg) {

    if (msg.tag() == message::Message::Tag::Flush) {
        DumpRestart();
        executeNext(msg);
        return;
    }

    if (msg.tag() != message::Message::Tag::Field) {
        executeNext(msg);
        return;
    }

    std::string key = generateKey(msg);
    StatisticsConfiguration cfg{cfg_, msg};
    IOmanager_->reset();
    IOmanager_->setCurrStep(cfg.restartStep());
    IOmanager_->setKey(key);

    util::ScopedTiming timing{statistics_.localTimer_, statistics_.actionTiming_};

    if (fieldStats_.find(key) == fieldStats_.end()) {
        fieldStats_[key] = std::make_unique<TemporalStatistics>(periodUpdater_, operations_, msg, IOmanager_, cfg);
        if (cfg.solver_send_initial_condition()) {
            util::ScopedTiming timing{statistics_.localTimer_, statistics_.actionTiming_};
            return;
        }
    }

    fieldStats_.at(key)->updateData(msg, cfg);

    if (fieldStats_.at(key)->isEndOfWindow(msg, cfg)) {
        auto md = outputMetadata(msg.metadata(), cfg, key);
        for (auto it = fieldStats_.at(key)->begin(); it != fieldStats_.at(key)->end(); ++it) {
            eckit::Buffer payload;
            payload.resize((*it)->byte_size());
            payload.zero();
            md.set("operation", (*it)->operation());
            md.set("operation-frequency", compConf_.parsedConfig().getString("output-frequency"));
            (*it)->compute(payload);
            executeNext(message::Message{message::Message::Header{message::Message::Tag::Field, msg.source(),
                                                                  msg.destination(), message::Metadata{md}},
                                         std::move(payload)});
        }

        util::ScopedTiming timing{statistics_.localTimer_, statistics_.actionTiming_};

        fieldStats_.at(key)->updateWindow(msg, cfg);
    }

    return;
}


void Statistics::print(std::ostream& os) const {
    os << "Statistics(output frequency = " << periodUpdater_->timeSpan() << ", unit = " << periodUpdater_->timeUnit()
       << ", operations = ";
    bool first = true;
    for (const auto& ops : operations_) {
        os << (first ? "" : ", ");
        os << ops;
        first = false;
    }
    os << ")";
}


static ActionBuilder<Statistics> StatisticsBuilder("statistics");

}  // namespace multio::action

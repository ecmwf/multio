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

#include "multio/action/statistics/cfg/StatisticsConfiguration.h"


namespace multio::action {

Statistics::Statistics(const ComponentConfiguration& compConf) :
    ChainedAction{compConf},
    lastDateTime_{""},
    opt_{compConf},
    operations_{compConf.parsedConfig().getStringVector("operations")},
    outputFrequency_{compConf.parsedConfig().getString("output-frequency")},
    IOmanager_{StatisticsIOFactory::instance().build(opt_.restartLib(), opt_.restartPath(), opt_.restartPrefix())} {
    if ( opt_.readRestart() ) {
        LoadRestart();
    }
    return;
}


void Statistics::DumpRestart() {
    if (opt_.writeRestart()) {
        for (auto it = fieldStats_.begin(); it != fieldStats_.end(); it++) {
            LOG_DEBUG_LIB(LibMultio) << "Restart for field with key :: " << it->first << ", "
                                     << it->second->cwin().currPointInSteps() << std::endl;
            IOmanager_->pushDir(it->first);
            it->second->dump(IOmanager_, opt_);
            IOmanager_->popDir();
        }
    }
}


void Statistics::LoadRestart() {
    if (opt_.readRestart()) {
        IOmanager_->setDateTime(opt_.restartTime());
        std::ostringstream logos;
        std::cout << logos.str() << std::endl;
        std::vector<eckit::PathName> dirs = IOmanager_->getDirs();
        for ( const auto& dir : dirs ) {    std::ostringstream logos;
            IOmanager_->pushDir(dir.baseName());
            fieldStats_[dir.baseName()] = std::make_unique<TemporalStatistics>( IOmanager_, opt_ );
            IOmanager_->popDir();
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

    md.set("previousDate", win.creationPoint().date().yyyymmdd());
    md.set("previousTime", win.creationPoint().time().hhmmss());
    md.set("currentDate", win.endPoint().date().yyyymmdd());
    md.set("currentTime", win.endPoint().time().hhmmss());

    md.set("startStepInHours", win.creationPointInHours());
    md.set("endStepInHours", win.endPointInHours());

    return md;
}


void Statistics::executeImpl(message::Message msg) {

    if (msg.tag() == message::Message::Tag::Flush) {
        if ( msg.metadata().has( "flushKind" )  ) {
            std::string flushKind = msg.metadata().getString( "flushKind" );
            if ( flushKind == "step-and-restart" || flushKind == "last-step" ) {
                std::cout << "Performing a Dump :: Flush kind :: " << flushKind << "Last DateTime :: " << lastDateTime_ <<  std::endl;
                IOmanager_->setDateTime( lastDateTime_ );
                DumpRestart();
            }
        }
        executeNext(msg);
        return;
    }

    if (msg.tag() != message::Message::Tag::Field) {
        executeNext(msg);
        return;
    }


    StatisticsConfiguration cfg{msg, opt_};
    std::string key = cfg.key();
    std::ostringstream tmp;
    tmp << std::setw(8) << std::setfill('0') << cfg.curr().date().yyyymmdd() << "-"
        << std::setw(6) << std::setfill('0') << cfg.curr().time().hhmmss();
    lastDateTime_ = tmp.str();


    util::ScopedTiming timing{statistics_.localTimer_, statistics_.actionTiming_};


    auto stat = fieldStats_.find(key);
    if ( stat == fieldStats_.end() ) {
        fieldStats_[key] = std::make_unique<TemporalStatistics>(outputFrequency_, operations_, msg, IOmanager_, cfg);
        stat = fieldStats_.find(key);
        if (opt_.solver_send_initial_condition()) {
            util::ScopedTiming timing{statistics_.localTimer_, statistics_.actionTiming_};
            return;
        }
    }
    else {
        // A simulation that sends step 0, send also step 0 in the restarted simulations.
        // Because of this if the windos was already present, we need check if the current time is
        // the same as the current point in the window due to a restarted window. And in this case just exit.
        // Obviously this check is not meaningful for newly created windows
        auto& ts = *(stat->second);
        if ( cfg.curr() == ts.cwin().currPoint() && opt_.solver_send_initial_condition() ) {
            util::ScopedTiming timing{statistics_.localTimer_, statistics_.actionTiming_};
            return;
        }
    }

    auto& ts = *(stat->second);

    // std::ostringstream os;
    // os << "Current time vs current point in the  window :: "
    //    << cfg.curr() << " " << ts.cwin().currPoint()
    //    << std::endl;
    // std::cout << os.str() << std::endl;

    // In any case if the current time is greater than the current point in the window, we have a problem
    if ( cfg.curr() <= ts.cwin().currPoint() ) {
        std::ostringstream os;
        os << "Current time is greater than the current point in the window :: " << cfg.curr().date().yyyymmdd() << " "
           << cfg.curr().time().hhmmss() << " " << ts.cwin().currPoint().date().yyyymmdd() << " " << ts.cwin().currPoint().time().hhmmss()
           << std::endl;
        throw eckit::SeriousBug(os.str(), Here());
    }

    // Update data
    ts.updateData(msg, cfg);

    // Decide to emit statistics
    if (ts.isEndOfWindow(msg, cfg)) {
        auto md = outputMetadata(msg.metadata(), cfg, key);
        for (auto it = ts.begin(); it != ts.end(); ++it) {
            eckit::Buffer payload;
            payload.resize((*it)->byte_size());
            payload.zero();
            md.set("operation", (*it)->operation());
            md.set("operation-frequency", compConf_.parsedConfig().getString("output-frequency"));
            (*it)->compute(payload, cfg);
            executeNext(message::Message{message::Message::Header{message::Message::Tag::Field, msg.source(),
                                                                  msg.destination(), message::Metadata{md}},
                                         std::move(payload)});
        }

        util::ScopedTiming timing{statistics_.localTimer_, statistics_.actionTiming_};

        ts.updateWindow(msg, cfg);
    }

    return;
}


void Statistics::print(std::ostream& os) const {
    // os << "Statistics(output frequency = " << periodUpdater_->timeSpan() << ", unit = " << periodUpdater_->timeUnit()
    //    << ", operations = ";
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

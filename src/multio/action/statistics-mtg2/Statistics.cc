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

#include <unistd.h>  //currently needed because ECKIT PathName only has hardlinks for for the directories we need a symlink
#include <algorithm>
#include <unordered_map>


#include "TemporalStatistics.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/types/DateTime.h"
#include "multio/LibMultio.h"
#include "multio/message/Glossary.h"
#include "multio/message/Message.h"
#include "multio/util/Timing.h"

#include "multio/action/statistics-mtg2/cfg/StatisticsConfiguration.h"


namespace multio::action::statistics_mtg2 {

using message::glossary;

Statistics::Statistics(const ComponentConfiguration& compConf) :
    ChainedAction{compConf},
    needRestart_{false},
    lastDateTime_{""},
    opt_{compConf},
    operations_{compConf.parsedConfig().getStringVector("operations")},
    outputFrequency_{compConf.parsedConfig().getString("output-frequency")},
    remapParamID_{compConf},
    IOmanager_{StatisticsIOFactory::instance().build(opt_.restartLib(), opt_.restartPath(), opt_.restartPrefix())} {}

std::string Statistics::generateRestartNameFromFlush(const message::Message& msg) const {

    std::string folderName;

    // Restart flush directly provides the folderName
    auto restartDateTime = msg.metadata().getOpt<std::string>("restartDateTime");
    auto step = msg.metadata().getOpt<std::int64_t>(glossary().step);
    auto timeStep = msg.metadata().getOpt<std::int64_t>(glossary().timeStep);
    auto date = msg.metadata().getOpt<std::int64_t>(glossary().date);
    auto time = msg.metadata().getOpt<std::int64_t>(glossary().time);

    if (restartDateTime) {
        folderName = *restartDateTime;
    }

    // Restart flush provides the step, timeStep, date and time
    else if (step && timeStep && date && time) {

        std::int64_t flushStep = *step;
        std::int64_t flushTimeStep = *timeStep;
        std::int64_t flushDate = *date;
        std::int64_t flushTime = *time;

        // Compute the date and time from the step
        // TODO: Remove this multiple times repeated code
        long D = static_cast<long>(flushDate % 100);
        long M = static_cast<long>((flushDate % 10000) / 100);
        long Y = static_cast<long>((flushDate % 100000000) / 10000);
        long s = static_cast<long>(flushTime % 100);
        long m = static_cast<long>((flushTime % 10000) / 100);
        long h = static_cast<long>((flushTime % 1000000) / 10000);
        eckit::DateTime epoch{eckit::Date{Y, M, D}, eckit::Time{h, m, s}};

        // TODO: Loop over the TemporalStatistics windows and check that the date/time in the flush
        // is the same as the epoch date/time of the window. If not throw an error.
        //
        // for (auto ts : fieldStats_) {
        //     if (ts->cwin().epochPoint() != epoch) {
        //         std::ostringstream os;
        //         os << "Epoch point of the window is not the same as the flush date/time" << std::endl;
        //         throw eckit::SeriousBug(os.str(), Here());
        //     }
        // }

        eckit::DateTime dt = epoch + static_cast<eckit::Second>(flushStep * flushTimeStep);
        std::ostringstream tmp;
        tmp << std::setw(8) << std::setfill('0') << dt.date().yyyymmdd() << "-" << std::setw(6) << std::setfill('0')
            << dt.time().hhmmss();
        folderName = tmp.str();
    }

    // Restart flush does not provide anything (Fallback)
    else {
        folderName = lastDateTime_;
    }

    return folderName;
}

void Statistics::CreateMainRestartDirectory(const std::string& restartFolderName, bool is_master) {

    // Create the main restart directory
    // TODO: if statistics are client side opt_.clientSideStatistics() then
    // the restart directory should be created with appended the mpi-rank of
    // processor that is creating the directory and all following login should
    // be skipped since every processor will create its own directory.
    IOmanager_->setDateTime(restartFolderName);

    // Only master create the directory
    if (!IOmanager_->currentDirExists()) {
        if (is_master) {
            IOmanager_->createCurrentDir();
        }
        else {
            long cnt = 0;
            while (!IOmanager_->currentDirExists() && cnt < 100) {
                cnt++;
                usleep(1000);
                LOG_DEBUG_LIB(LibMultio) << "Waiting for Dump directory to be created by master: " << restartFolderName
                                         << std::endl;
            }
            if (cnt >= 100) {
                std::ostringstream os;
                os << "Unable to create the restart directory: " << restartFolderName << std::endl;
                throw eckit::SeriousBug(os.str(), Here());
            }
        }
    }

    return;
}

void Statistics::DumpTemporalStatistics() {
    for (auto it = fieldStats_.begin(); it != fieldStats_.end(); it++) {
        LOG_DEBUG_LIB(LibMultio) << "   - Restart for field with key :: " << it->first << ", "
                                 << it->second->cwin().currPointInSteps() << std::endl;
        IOmanager_->pushDir(it->first);
        if (IOmanager_->currentDirExists()) {
            std::ostringstream os;
            os << "Current restart already exists (this means that two mpi tasks has the same field): "
               << IOmanager_->getCurrentDir() << std::endl;
            throw eckit::SeriousBug(os.str(), Here());
        }
        IOmanager_->createCurrentDir();
        it->second->dump(IOmanager_, opt_);
        IOmanager_->popDir();
    }
    return;
}

enum class FlushKind: std::size_t{
    Default,FirstStep,StepAndRestart, LastStep, EndOfSimulation, CloseConnection
};

FlushKind parseFlushKind(const std::string & str) {
    static const std::unordered_map <std::string,FlushKind> map{
        { "first-step", FlushKind::FirstStep},
        { "default", FlushKind::Default},
        { "step-and-restart", FlushKind::StepAndRestart},
        { "last-step", FlushKind::LastStep },
        { "end-of-simulation", FlushKind::EndOfSimulation},
        { "close-connection", FlushKind::CloseConnection}
    };
    if(auto search = map.find(str); search != map.end()){
        return search->second;
    }
    throw message::MetadataException(std::string("Unknown FlushKind: ") + str, Here());
}


FlushKind parseFlushKind(std::int64_t val) {
    static const std::unordered_map <std::int64_t,FlushKind> map{
        { 0, FlushKind::FirstStep},
        { 1, FlushKind::Default},
        { 2, FlushKind::StepAndRestart},
        { 3, FlushKind::LastStep },
        { 4, FlushKind::EndOfSimulation},
        { 5, FlushKind::CloseConnection}
    };
    if(auto search = map.find(val); search != map.end()){
        return search->second;
    }
    throw message::MetadataException(std::string("Unknown FlushKind: ") + std::to_string(val), Here());
}


void Statistics::TryDumpRestart(const message::Message& msg) {

    FlushKind flushKind{FlushKind::Default};
    if(auto search = msg.metadata().find("flushKind"); search != msg.metadata().end()){
        search->second.visit(eckit::Overloaded{
            [&](const std::string & str){
                flushKind = parseFlushKind(str);
            },
            [&](const std::int64_t val){
                flushKind = parseFlushKind(val);
            },
            [&](const auto &){
                throw message::MetadataException("FlushKind needs to be either string or integer.",Here());
            }
        });
    }


    if (opt_.writeRestart() && needRestart_) {

        // Check the kind of flush
        // NOTE: This is a bit of a hack, in case no serverRank is provided, we
        // assume that all processors are "master" and rely on atomicity of
        // filesystem operation to avoid race conditions
        auto is_master = msg.metadata().getOpt<bool>("serverRank").value_or(true);
        switch(flushKind){
            case FlushKind::StepAndRestart: 
            case FlushKind::LastStep: 
            case FlushKind::EndOfSimulation: 
            case FlushKind::CloseConnection:
            {
                // Generate name of the main restart directory
                std::string restartFolderName = generateRestartNameFromFlush(msg);

                // Log for Dump operation
                LOG_DEBUG_LIB(LibMultio) << "Performing a Dump :: Flush kind :: " << "Last DateTime :: " << restartFolderName << std::endl;

                // Delete the latest symlink as soon as possible
                if (is_master) {
                    DeleteLatestSymLink();
                }

                // Create the main restart directory: <rundir>/<UniqueID>/<DateTime>
                CreateMainRestartDirectory(restartFolderName, is_master);

                // Dump the temporal statistics restart directories
                DumpTemporalStatistics();

                // Create the latest symlink to the latest restart directory
                if (is_master) {
                    CreateLatestSymLink();
                }
            } 
            break;
            default: {} break;
            
        }

    }
    return;
}

void Statistics::DeleteLatestSymLink() {

    std::string latestPath = IOmanager_->getRestartSymLink();
    std::string currentDateTime = IOmanager_->getDateTime();
    // Even though eckit::PathName::link is a hardlink and does not work blow exists() follows the link and isLink also
    // works for symlinks and hence the eckit calls can be used here
    if (eckit::PathName{latestPath}.exists() && eckit::PathName{latestPath}.isLink()) {
        eckit::PathName{latestPath}.unlink();
        LOG_DEBUG_LIB(LibMultio) << "Deleted old Symlink from " << currentDateTime << " to " << latestPath << std::endl;
    }
}

void Statistics::CreateLatestSymLink() {
    std::string latestPath = IOmanager_->getRestartSymLink();
    std::string currentDateTime = IOmanager_->getDateTime();
    // create latest symlink
    // TODO If eckit allows symlinks for directories instead of hard links it would be good to use eckit
    // //eckit::PathName::link(eckit::PathName{latestPath},eckit::PathName{IOmanager_->getCurrentDir()});
    symlink(currentDateTime.c_str(), latestPath.c_str());
    LOG_DEBUG_LIB(LibMultio) << "Created Symlink from " << currentDateTime << " to " << latestPath << std::endl;
}

std::unique_ptr<TemporalStatistics> Statistics::LoadTemporalStatisticsFromKey(const std::string& key) {
    IOmanager_->setDateTime(opt_.restartTime());
    IOmanager_->pushDir(key);
    if (!IOmanager_->currentDirExists()) {
        std::ostringstream os;
        os << "Unable to find the restart field" << std::endl;
        throw eckit::SeriousBug(os.str(), Here());
    }
    std::unique_ptr<TemporalStatistics> tmp = std::make_unique<TemporalStatistics>(IOmanager_, opt_);
    IOmanager_->popDir();
    return tmp;
}

bool Statistics::HasMainRestartDir() {
    IOmanager_->setDateTime(opt_.restartTime());
    bool has_restart = IOmanager_->currentDirExists() ? true : false;
    return has_restart;
}

bool Statistics::HasRestartKey(const std::string& key) {
    IOmanager_->setDateTime(opt_.restartTime());
    IOmanager_->pushDir(key);
    bool has_restart = IOmanager_->currentDirExists() ? true : false;
    IOmanager_->popDir();
    return has_restart;
}


message::Metadata Statistics::outputMetadata(const message::Metadata& inputMetadata, const StatisticsConfiguration& cfg,
                                             const std::string& key) const {
    auto& win = fieldStats_.at(key)->cwin();
    // if (win.endPointInSeconds() % 3600 != 0L) {
    //     std::ostringstream os;
    //     os << "Step in seconds needs to be a multiple of 3600 :: " << fieldStats_.at(key)->win().endPointInSeconds()
    //        << std::endl;
    //     throw eckit::SeriousBug(os.str(), Here());
    // }
    auto md = inputMetadata;

    // util::DateTimeDiff lastPointsDiff = win.lastPointsDiff();

    // md.set("sampleIntervalUnit", std::string{util::timeUnitToChar(lastPointsDiff.unit)});
    // md.set("sampleInterval", lastPointsDiff.diff);

    md.set(glossary().sampleIntervalInSeconds, win.lastPointsDiffInSeconds());

    md.set(glossary().startDate, win.epochPoint().date().yyyymmdd());
    md.set(glossary().startTime, win.epochPoint().time().hhmmss());
    md.set(glossary().stepFrequency, win.timeSpanInSteps());

    md.set(glossary().previousDate, win.creationPoint().date().yyyymmdd());
    md.set(glossary().previousTime, win.creationPoint().time().hhmmss());
    md.set(glossary().currentDate, win.endPoint().date().yyyymmdd());
    md.set(glossary().currentTime, win.endPoint().time().hhmmss());

    md.set("startStepInHours", win.creationPointInHours());
    md.set("endStepInHours", win.endPointInHours());

    return md;
}


void Statistics::updateLatestDateTime(const StatisticsConfiguration& cfg) {

    std::ostringstream tmp;
    tmp << std::setw(8) << std::setfill('0') << cfg.curr().date().yyyymmdd() << "-" << std::setw(6) << std::setfill('0')
        << cfg.curr().time().hhmmss();
    lastDateTime_ = tmp.str();
    needRestart_ = true;

    return;
}


void Statistics::executeImpl(message::Message msg) {

    // Handle flush
    if (msg.tag() == message::Message::Tag::Flush) {
        TryDumpRestart(msg);
        executeNext(msg);
        return;
    }

    // Handle other non fields
    if (msg.tag() != message::Message::Tag::Field) {
        executeNext(msg);
        return;
    }

    // Handle fields
    // -------------

    // Initialize local variables
    StatisticsConfiguration cfg{msg, opt_};
    std::string key = cfg.key();
    updateLatestDateTime(cfg);

    // Check if the main restart directory exists
    if (opt_.readRestart() && !HasMainRestartDir()) {
        std::ostringstream os;
        os << "Main restart directory does not exist :: " << opt_.restartPath() << std::endl;
        throw eckit::SeriousBug(os.str(), Here());
    }

    util::ScopedTiming timing{statistics_.actionTiming_};

    // Access or create the temporal statistics object
    auto stat = fieldStats_.find(key);
    if (stat == fieldStats_.end()) {
        if (opt_.readRestart() && HasRestartKey(key)) {
            fieldStats_[key] = LoadTemporalStatisticsFromKey(key);
        }
        else {
            fieldStats_[key]
                = std::make_unique<TemporalStatistics>(outputFrequency_, operations_, msg, IOmanager_, cfg);
        }
        // TODO: Reorganize the code to avoid this second search
        // which is not efficient
        stat = fieldStats_.find(key);
    }

    // Exit if the current time is the same as the current point in the
    // window and the solver does not send the initial condition.
    // This can happen when the solver is sending the initial condition
    // and and the same point is already present in the restart
    auto& ts = *(stat->second);
    if (cfg.curr() == ts.cwin().currPoint() && opt_.solver_send_initial_condition()) {
        return;
    }

    // std::ostringstream os;
    // os << "Current time vs current point in the  window :: "
    //    << cfg.curr() << " " << ts.cwin().currPoint()
    //    << std::endl;
    // std::cout << os.str() << std::endl;

    // In any case if the current time is greater than the current point in the window, we have a problem
    if (cfg.curr() <= ts.cwin().currPoint()) {
        std::ostringstream os;
        os << "Current time is greater than the current point in the window :: " << cfg.curr().date().yyyymmdd() << " "
           << cfg.curr().time().hhmmss() << " " << ts.cwin().currPoint().date().yyyymmdd() << " "
           << ts.cwin().currPoint().time().hhmmss() << std::endl;
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
            std::string opname = (*it)->operation();
            std::string outputFrequency = compConf_.parsedConfig().getString("output-frequency");
            md.set("operation", opname);
            md.set("operation-frequency", outputFrequency);
            remapParamID_.ApplyRemap(md, opname, outputFrequency);
            (*it)->compute(payload, cfg);
            executeNext(message::Message{message::Message::Header{message::Message::Tag::Field, msg.source(),
                                                                  msg.destination(), message::Metadata{md}},
                                         std::move(payload)});
        }

        ts.updateWindow(msg, cfg);
    }

    return;
}


void Statistics::print(std::ostream& os) const {
    os << "Statistics( output-frequency:" << outputFrequency_ << ", operations:{";
    bool first = true;
    for (const auto& ops : operations_) {
        os << (first ? "" : ", ");
        os << ops;
        first = false;
    }
    os << "})";
}


static ActionBuilder<Statistics> StatisticsBuilder("statistics-mtg2");

}  // namespace multio::action::statistics_mtg2

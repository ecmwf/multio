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

namespace {

const std::map<const char, const std::string> symbol_to_unit{{'h', "hour"}, {'d', "day"}, {'m', "month"}};

std::string set_unit(std::string const& output_freq) {
    const auto& symbol = output_freq.back();

    if (symbol_to_unit.find(symbol) == end(symbol_to_unit)) {
        throw eckit::SeriousBug{"Time unit for symbol " + std::string{symbol} + " is not supported", Here()};
    }
    return symbol_to_unit.at(symbol);
}

long set_frequency(const std::string& output_freq) {
    auto freq = output_freq.substr(0, output_freq.size() - 1);
    return std::stol(freq);
}

}  // namespace

Statistics::Statistics(const ComponentConfiguration& compConf) :
    ChainedAction{compConf},
    timeUnit_{set_unit(compConf.parsedConfig().getString("output-frequency"))},
    timeSpan_{set_frequency(compConf.parsedConfig().getString("output-frequency"))},
    periodUpdater_{make_period_updater(timeUnit_, timeSpan_)},
    operations_{compConf.parsedConfig().getStringVector("operations")},
    cfg_{compConf} {}


void Statistics::DumpRestart() const {
    try {
        std::shared_ptr<StatisticsIO> IOmanager{
            StatisticsIOFactory::instance().build(cfg_.restartLib(), cfg_.restartPath(), cfg_.restartPrefix(), 0)};
        if (cfg_.writeRestart()) {
            LOG_DEBUG_LIB(LibMultio) << "Writing statistics checkpoint..." << std::endl;
            for (auto it = fieldStats_.begin(); it != fieldStats_.end(); it++) {
                LOG_DEBUG_LIB(LibMultio) << "Restart for field with key :: " << it->first << ", "
                                         << it->second->win().currPointInSteps() << std::endl;
                IOmanager->setStep(it->second->win().currPointInSteps());
                IOmanager->setKey(it->first);
                it->second->dump(IOmanager, cfg_);
            }
        }
    }
    catch (...) {
        std::ostringstream os;
        os << "Failed to write restart :: " << std::endl;
        throw eckit::SeriousBug(os.str(), Here());
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
    auto& win = fieldStats_.at(key)->win();
    if (win.endPointInSeconds() % 3600 != 0L) {
        std::ostringstream os;
        os << "Step in seconds needs to be a multiple of 3600 :: " << fieldStats_.at(key)->win().endPointInSeconds()
           << std::endl;
        throw eckit::SeriousBug(os.str(), Here());
    }
    auto md = inputMetadata;
    md.set("timeUnit", timeUnit_);
    md.set("startDate", win.epochPoint().date().yyyymmdd());
    md.set("startTime", win.epochPoint().time().hhmmss());
    md.set("timeSpanInHours", win.timeSpanInHours());
    md.set("stepRange", win.stepRange());
    md.set("previousDate", win.creationPoint().date().yyyymmdd());
    md.set("previousTime", win.creationPoint().time().hhmmss());
    md.set("currentDate", win.endPoint().date().yyyymmdd());
    md.set("currentTime", win.endPoint().time().hhmmss());
    md.set("stepInHours", win.endPointInHours());
    md.set("stepRangeInHours", win.stepRangeInHours());
    return md;
}


void Statistics::executeImpl(message::Message msg) {

    if (msg.tag() != message::Message::Tag::Field) {
        if (msg.tag() == message::Message::Tag::Flush) {
            LOG_DEBUG_LIB(multio::LibMultio) << "statistics  :: Flush received" << std::endl;
            DumpRestart();
        }
        executeNext(msg);
        return;
    }

    std::string key = generateKey(msg);
    StatisticsConfiguration cfg{cfg_, msg};
    std::shared_ptr<StatisticsIO> IOmanager{StatisticsIOFactory::instance().build(
        cfg_.restartLib(), cfg.restartPath(), cfg.restartPrefix(), cfg.restartStep())};
    IOmanager->setKey(key);

    {
        util::ScopedTiming timing{statistics_.localTimer_, statistics_.actionTiming_};

        LOG_DEBUG_LIB(multio::LibMultio) << "*** " << msg.destination() << " -- metadata: " << msg.metadata()
                                         << std::endl;
        if (fieldStats_.find(key) == fieldStats_.end()) {
            fieldStats_[key] = TemporalStatistics::build(periodUpdater_, operations_, msg, IOmanager, cfg);
            if (cfg.solver_send_initial_condition()) {
                LOG_DEBUG_LIB(LibMultio) << "Exiting because of initial condition :: " << key << std::endl;
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
                (*it)->compute(payload);
                executeNext(message::Message{message::Message::Header{message::Message::Tag::Field, msg.source(),
                                                                      msg.destination(), message::Metadata{md}},
                                             std::move(payload)});
            }

            util::ScopedTiming timing{statistics_.localTimer_, statistics_.actionTiming_};

            fieldStats_.at(key)->updateWindow(msg, cfg);
        }
    }
    return;
}

void Statistics::print(std::ostream& os) const {
    os << "Statistics(output frequency = " << timeSpan_ << ", unit = " << timeUnit_ << ", operations = ";
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

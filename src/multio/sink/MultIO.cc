/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Tiago Quintino
/// @author Simon Smart
/// @date Dec 2015

#include "MultIO.h"

#include <sys/types.h>
#include <unistd.h>
#include <functional>

#include "eckit/exception/Exceptions.h"
#include "eckit/runtime/Main.h"
#include "eckit/utils/Translator.h"
#include "eckit/value/Value.h"


#include <multio/LibMultio.h>
#include <multio/util/Substitution.h>

using namespace eckit;

namespace multio::sink {

namespace {

class StatsTimer {
    eckit::Timer& timer_;
    std::function<void(eckit::Timer&)> fun_;

public:
    explicit StatsTimer(eckit::Timer& t, std::function<void(eckit::Timer&)> fn) : timer_(t), fun_(fn) {
        timer_.start();
    }
    ~StatsTimer() {
        timer_.stop();
        fun_(timer_);
    }
};
}  // namespace

//--------------------------------------------------------------------------------------------------

using namespace std::placeholders;

MultIO::MultIO(const ComponentConfiguration& compConf) :
    DataSink(compConf),
    stats_(std::string("Multio ") + Main::hostname() + ":" + Translator<int, std::string>()(::getpid())),
    trigger_(compConf_) {

    for (auto&& subComp : compConf.subComponents("sinks")) {
        auto sinkId = sinks_.size();
        auto enabled = util::parseEnabled(subComp.parsedConfig(), true);
        if (!enabled) {
            throw eckit::UserError("bool expected in sink enabling", Here());
        }
        if (*enabled) {
            sinks_
                .emplace_back(
                    DataSinkFactory::instance().build(subComp.parsedConfig().getString("type"), std::move(subComp)))
                ->setId(sinkId);
        }
    }
}

bool MultIO::ready() const {
    std::lock_guard<std::mutex> lock(mutex_);

    decltype(sinks_)::const_iterator it = sinks_.begin();
    decltype(sinks_)::const_iterator end = sinks_.end();
    for (; it != end; ++it) {
        if (!(*it)->ready()) {
            return false;
        }
    }
    return true;
}

void MultIO::write(eckit::message::Message message) {

    std::lock_guard<std::mutex> lock(mutex_);

    StatsTimer stTimer{timer_, std::bind(&IOStats::logWrite, &stats_, message.length(), _1)};
    for (const auto& sink : sinks_) {
        sink->write(message);
    }

    LOG_DEBUG_LIB(LibMultio) << "Trigger events for message " << message << std::endl;

    trigger_.events(message);
}

void MultIO::trigger(const eckit::StringDict& metadata) const {
    trigger_.events(metadata);
}

void MultIO::flush() {
    std::lock_guard<std::mutex> lock(mutex_);

    StatsTimer stTimer{timer_, std::bind(&IOStats::logFlush, &stats_, _1)};
    for (const auto& sink : sinks_) {
        sink->flush();
    }
}

void MultIO::report(std::ostream& s) {
    stats_.report(s);
}

void MultIO::print(std::ostream& os) const {
    std::lock_guard<std::mutex> lock(mutex_);
    os << "MultIO(";
    bool first = true;
    for (const auto& sink : sinks_) {
        os << (first ? "" : ", ");
        os << *(sink);
        first = false;
    }
    os << ")";
}

static DataSinkBuilder<MultIO> DataSinkSinkBuilder("multio");

}  // namespace multio::sink

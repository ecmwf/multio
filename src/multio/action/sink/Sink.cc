/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "Sink.h"

#include <fstream>

#include "eckit/exception/Exceptions.h"
#include "eckit/message/Message.h"

#include "multio/LibMultio.h"
#include "multio/util/ScopedTimer.h"
#include "multio/util/logfile_name.h"

namespace multio {
namespace action {

Sink::Sink(const ConfigurationContext& confCtx) :
    Action(confCtx), report_{confCtx.config().getBool("report", true)}, mio_{confCtx} {}

Sink::~Sink() {
    if (report_) {
        std::ofstream logFile{util::logfile_name(), std::ios_base::app};
        mio_.report(logFile);
    }
}

void Sink::executeImpl(Message msg) {

    switch (msg.tag()) {
        case Message::Tag::Field:
        case Message::Tag::Grib:
            write(msg);
            return;

        case Message::Tag::Flush:
            flush();
            return;

        case Message::Tag::Notification:
            trigger(msg);
            return;

        default:
            throw eckit::SeriousBug("Cannot handle message <" + Message::tag2str(msg.tag()) + ">");
    }
}

void Sink::write(Message msg) {
    util::ScopedTiming timing{statistics_.localTimer_, statistics_.actionTiming_};

    eckit::message::Message blob = to_eckit_message(msg);

    mio_.write(blob);
}

void Sink::flush() {
    util::ScopedTiming timing{statistics_.localTimer_, statistics_.actionTiming_};
    mio_.flush();
}

void Sink::trigger(const Message& msg) {
    util::ScopedTiming timing{statistics_.localTimer_, statistics_.actionTiming_};

    eckit::StringDict metadata;

    if(!msg.metadata().has("trigger")) {
        throw message::MetadataMissingKeyException("trigger", Here()); 
    }
    const std::string triggerKey = msg.metadata().getString("trigger");
    
    if(!msg.metadata().has(triggerKey)) {
        throw message::MetadataMissingKeyException(triggerKey, Here()); 
    }
    // TODO - handle type correctly once metadata is refactored
    metadata[triggerKey] = msg.metadata().getString(triggerKey);

    eckit::Log::debug<LibMultio>() << "Trigger " << triggerKey << " with value " << metadata[triggerKey]
                                   << " is being called..." << std::endl;

    mio_.trigger(metadata);
}

void Sink::print(std::ostream& os) const {
    os << "Sink(DataSink=" << mio_ << ")";
}

static ActionBuilder<Sink> SinkBuilder("sink");

}  // namespace action
}  // namespace multio

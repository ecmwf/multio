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
#include <sstream>

#include "eckit/exception/Exceptions.h"
#include "eckit/message/Message.h"

#include "multio/LibMultio.h"

namespace multio::action::sink {

Sink::Sink(const ComponentConfiguration& compConf) : Action(compConf), mio_{compConf} {}

void Sink::executeImpl(Message msg) {

    switch (msg.tag()) {
        case Message::Tag::Field:
            write(msg);
            return;

        case Message::Tag::Flush:
            flush();
            return;

        case Message::Tag::Notification:
            trigger(msg);
            return;

        case Message::Tag::Parametrization:
            eckit::Log::debug<LibMultio>() << "Parametrization message: " << msg << std::endl;
            return;

        default:
            throw eckit::SeriousBug("Cannot handle message <" + Message::tag2str(msg.tag()) + ">");
    }
}

void Sink::write(Message msg) {
    util::ScopedTiming timing{statistics_.actionTiming_};

    eckit::message::Message blob = to_eckit_message(msg);

    mio_.write(blob);
}

void Sink::flush() {
    util::ScopedTiming timing{statistics_.actionTiming_};
    mio_.flush();
}

void Sink::trigger(const Message& msg) {
    util::ScopedTiming timing{statistics_.actionTiming_};

    eckit::StringDict metadata;

    auto triggerKey = msg.metadata().getOpt<std::string>("trigger");
    if (!triggerKey) {
        throw message::MetadataMissingKeyException("trigger", Here());
    }
    auto searchTriggerKey = msg.metadata().find(*triggerKey);
    if (searchTriggerKey == msg.metadata().end()) {
        throw message::MetadataMissingKeyException(*triggerKey, Here());
    }

    auto triggerKeyVal = util::visitTranslate<std::string>(searchTriggerKey->second);
    if (!triggerKeyVal) {
        std::ostringstream oss;
        oss << "Sink::trigger: Value for triggerKey \"" << *triggerKey << "\" can not be translated to string: ";
        oss << searchTriggerKey->second;
        throw eckit::UserError(oss.str(), Here());
    }
    metadata[*triggerKey] = *triggerKeyVal;

    eckit::Log::debug<LibMultio>() << "Trigger " << *triggerKey << " with value " << metadata[*triggerKey]
                                   << " is being called..." << std::endl;

    mio_.trigger(metadata);
}

void Sink::print(std::ostream& os) const {
    os << "Sink(DataSink=" << mio_ << ")";
}

static ActionBuilder<Sink> SinkBuilder("sink");

}  // namespace multio::action::sink

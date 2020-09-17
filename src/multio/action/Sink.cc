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

#include <iostream>

#include "eccodes.h"

#include "eckit/config/Configuration.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/message/Message.h"

#include "metkit/codes/CodesContent.h"

#include "multio/LibMultio.h"
#include "multio/message/DataContent.h"


namespace multio {
namespace action {


namespace {
eckit::message::MessageContent* to_msg_content(Message msg) {
    if(msg.tag() == Message::Tag::Grib) {
        codes_handle* h = codes_handle_new_from_message(nullptr, msg.payload().data(), msg.size());
        return new metkit::codes::CodesContent{h, true};
    }

    ASSERT(msg.tag() == Message::Tag::Field);
    return new message::DataContent(msg.payload().data(), msg.size());
}

}  // namespace

Sink::Sink(const eckit::Configuration &config) : Action(config), mio_{config} {}

void Sink::execute(Message msg) const {
    ScopedTimer timer{timing_};

    switch (msg.tag()) {
        case Message::Tag::Field:
        case Message::Tag::Grib:
            write(msg);
            executeNext(msg);
            return;

        case Message::Tag::StepComplete:
            flush();
            executeNext(msg);
            return;

        case Message::Tag::StepNotification:
            trigger(msg);
            executeNext(msg);
            return;

        default:
            throw eckit::SeriousBug("Cannot handle message <" + Message::tag2str(msg.tag()) + ">");
    }
}

void Sink::write(Message msg) const {
    eckit::message::Message blob(to_msg_content(msg));

    mio_.write(blob);
}

void Sink::flush() const {
    mio_.flush();
}

void Sink::trigger(const Message& msg) const {
    eckit::StringDict metadata;

    metadata[msg.category()] = msg.name();

    eckit::Log::debug<LibMultio>() << "Trigger " << msg.category() << " with value " << msg.name()
                                   << " is being called..." << std::endl;

    mio_.trigger(metadata);
}

void Sink::print(std::ostream& os) const {
    os << "Sink(DataSink=" << mio_ << ")";
}

static ActionBuilder<Sink> SinkBuilder("Sink");

}  // namespace action
}  // namespace multio

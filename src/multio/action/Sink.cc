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

#include "eckit/config/Configuration.h"
#include "eckit/exception/Exceptions.h"

#include "multio/LibMultio.h"

namespace multio {
namespace action {


namespace {
const std::map<Message::Tag, std::string> to_blob = {{Message::Tag::Field, "plain"},
                                                     {Message::Tag::Grib, "grib"}};
}

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
    ASSERT(to_blob.find(msg.tag()) != to_blob.end());

    eckit::DataBlobPtr blob{
        eckit::DataBlobFactory::build(to_blob.at(msg.tag()), msg.payload().data(), msg.size())};

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

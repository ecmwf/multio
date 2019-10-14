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
#include "multio/server/PlainDataBlob.h"

namespace multio {
namespace server {
namespace actions {

Sink::Sink(const eckit::Configuration &config) : Action(config), mio_{config} {}

void Sink::execute(Message msg) const {
    switch (msg.tag()) {
        case Message::Tag::Field:
        case Message::Tag::Grib:
            write(msg);
            return;

        case Message::Tag::StepComplete:
            flush();
            return;

        case Message::Tag::StepNotification: {
            eckit::StringDict metadata;

            // Hijack the mapping string
            metadata[msg.category()] = msg.name();

            eckit::Log::debug<LibMultio>() << "Trigger is called..." << std::endl;
            mio_.trigger(metadata);
            return;
        }

        default:
            ASSERT(false);
    }

    if (next_) {  // May want to assert not next_
        next_->execute(msg);
    }
}

void Sink::write(Message msg) const {
    eckit::DataBlobPtr blob;
    switch (msg.tag()) {
        case Message::Tag::Field:
            blob.reset(eckit::DataBlobFactory::build("plain", msg.payload().data(), msg.size()));
            break;

        case Message::Tag::Grib:
            blob.reset(eckit::DataBlobFactory::build("grib", msg.payload().data(), msg.size()));
            break;

        default:
            ASSERT(false);
    }

    mio_.write(blob);
}

void Sink::flush() const {
    mio_.flush();
}

void Sink::print(std::ostream& os) const {
    os << "Sink(DataSink=" << mio_ << ")";
}

static ActionBuilder<Sink> SinkBuilder("Sink");

}  // namespace actions
}  // namespace server
}  // namespace multio

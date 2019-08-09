/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "SingleFieldSink.h"

#include <iostream>

#include "eckit/config/Configuration.h"
#include "eckit/exception/Exceptions.h"

#include "multio/DataSink.h"
#include "multio/server/PlainDataBlob.h"

namespace multio {
namespace server {
namespace actions {

SingleFieldSink::SingleFieldSink(const eckit::Configuration &config) : Action(config) {}

void SingleFieldSink::execute(Message msg) const {
    switch (msg.tag()) {
        case Message::Tag::Field:
        case Message::Tag::Grib:
            write(msg);
            return;

        case Message::Tag::StepComplete:
            flush();
            return;

        default:
            ASSERT(false);
    }

    if (next_) {  // May want to assert not next_
        next_->execute(msg);
    }
}

void SingleFieldSink::write(Message msg) const {
    std::ostringstream oss;
    oss << msg.metadata().getString("igrib") << "::" << msg.metadata().getUnsigned("ilevg")
        << "::" << msg.metadata().getUnsigned("istep");
    eckit::LocalConfiguration config;

    config.set("path", oss.str());
    dataSink_.reset(DataSinkFactory::instance().build("file", config));

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

    dataSink_->write(blob);
}

void SingleFieldSink::flush() const {
    dataSink_->flush();
}

void SingleFieldSink::print(std::ostream& os) const {
    os << "Sink(DataSink=" << *dataSink_ << ")";
}

static ActionBuilder<SingleFieldSink> SinkBuilder("SingleFieldSink");

}  // namespace actions
}  // namespace server
}  // namespace multio

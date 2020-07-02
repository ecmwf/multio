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

#include "multio/sink/DataSink.h"
#include "multio/LibMultio.h"

namespace multio {
namespace action {

SingleFieldSink::SingleFieldSink(const eckit::Configuration& config) :
    Action{config},
    rootPath_{config.getString("root_path", "")} {}

void SingleFieldSink::execute(Message msg) const {
    switch (msg.tag()) {
        case Message::Tag::Field:
        case Message::Tag::Grib:
            write(msg);
            break;

        case Message::Tag::StepComplete:
            flush();
            break;

        default:
            ASSERT(false);
    }

    executeNext(msg);
}

void SingleFieldSink::write(Message msg) const {
    std::ostringstream oss;
    oss << rootPath_ << msg.metadata().getUnsigned("level")
        << "::" << msg.metadata().getString("param")
        << "::" << msg.metadata().getUnsigned("step");
    eckit::LocalConfiguration config;

    NOTIMP;
#if 0 // FINDME
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
#endif
}

void SingleFieldSink::flush() const {
    eckit::Log::debug<LibMultio>()
        << "*** Executing single-field flush for data sink... " << std::endl;
    if (dataSink_) {
        dataSink_->flush();
    }
}

void SingleFieldSink::print(std::ostream& os) const {
    if (dataSink_) {
        os << "Sink(DataSink=" << *dataSink_ << ")";
    } else {
        os << "Sink(DataSink=NULL)";
    }
}

static ActionBuilder<SingleFieldSink> SinkBuilder("SingleFieldSink");

}  // namespace action
}  // namespace multio

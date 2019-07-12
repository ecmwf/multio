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

#include "multio/server/PlainDataBlob.h"

namespace multio {
namespace server {
namespace actions {

Sink::Sink(const eckit::Configuration& config) : Action(config), mio_{config} {
    auto configs = config.getSubConfigurations("sinks");
    if (configs[0].getString("type") == "file" && !configs[0].has("path")) {
        ASSERT(configs.size() == 1);
        setFilePath_ = true;
        return;
    }

    dataSink_.reset(DataSinkFactory::instance().build("multio", config));
}

void Sink::execute(Message msg) const {
    switch (msg.tag()) {
        case Message::Tag::Field:
        case Message::Tag::GribTemplate:
            write(msg);
            return;

        case Message::Tag::StepComplete: {
            flush();

            eckit::StringDict metadata;

            // Hijack the mapping string
            metadata["step"] = msg.mapping();

            eckit::Log::info() << "Trigger is called..." << std::endl;
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
    if (setFilePath_) {
        std::ostringstream oss;
        oss << msg.metadata().getString("param") << "::" << msg.metadata().getUnsigned("level")
            << "::" << msg.metadata().getUnsigned("step");
        eckit::LocalConfiguration config;

        config.set("path", oss.str());
        dataSink_.reset(DataSinkFactory::instance().build("file", config));
    }

    eckit::DataBlobPtr blob;
    switch (msg.tag()) {
        case Message::Tag::Field:
            blob.reset(eckit::DataBlobFactory::build("plain", msg.payload().data(), msg.size()));
            break;

        case Message::Tag::GribTemplate:
            blob.reset(eckit::DataBlobFactory::build("grib", msg.payload().data(), msg.size()));
            break;

        default:
            ASSERT(false);
    }

    dataSink_->write(blob);
}

void Sink::flush() const {
    dataSink_->flush();
}

void Sink::print(std::ostream& os) const {
    os << "Sink(DataSink=" << *dataSink_ << ")";
}

static ActionBuilder<Sink> SinkBuilder("Sink");

}  // namespace actions
}  // namespace server
}  // namespace multio

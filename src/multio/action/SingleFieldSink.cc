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

#include "eckit/exception/Exceptions.h"
#include "eckit/message/Message.h"

#include "multio/LibMultio.h"
#include "multio/sink/DataSink.h"
#include "multio/util/ScopedTimer.h"

namespace multio {
namespace action {

SingleFieldSink::SingleFieldSink(const ConfigurationContext& confCtx) :
    Action{confCtx}, rootPath_{confCtx.config().getString("root_path", "")} {}

void SingleFieldSink::executeImpl(Message msg) const {
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

    executeNext(std::move(msg));
}

void SingleFieldSink::write(Message msg) const {
    util::ScopedTiming timing{statistics_.localTimer_, statistics_.actionTiming_};

    std::ostringstream oss;
    oss << rootPath_ << msg.metadata().getUnsigned("level")
        << "::" << msg.metadata().getString("param") << "::" << msg.metadata().getUnsigned("step");
    eckit::LocalConfiguration config;

    LOG_DEBUG_LIB(LibMultio) << "Writing output path: " << oss.str() << std::endl;
    config.set("path", oss.str());
    ConfigurationContext subCtx = confCtx_.recast(config);
    dataSink_.reset(DataSinkFactory::instance().build("file", subCtx));

    eckit::message::Message blob = to_eckit_message(msg);

    dataSink_->write(blob);
}

void SingleFieldSink::flush() const {
    util::ScopedTiming timing{statistics_.localTimer_, statistics_.actionTiming_};

    eckit::Log::debug<LibMultio>()
        << "*** Executing single-field flush for data sink... " << std::endl;
    if (dataSink_) {
        dataSink_->flush();
    }
}

void SingleFieldSink::print(std::ostream& os) const {
    if (dataSink_) {
        os << "Sink(DataSink=" << *dataSink_ << ")";
    }
    else {
        os << "Sink(DataSink=NULL)";
    }
}

static ActionBuilder<SingleFieldSink> SinkBuilder("single-field-sink");

}  // namespace action
}  // namespace multio

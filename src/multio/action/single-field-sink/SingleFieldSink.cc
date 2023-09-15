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
#include <sstream>

#include "eckit/exception/Exceptions.h"
#include "eckit/message/Message.h"

#include "multio/LibMultio.h"
#include "multio/message/Glossary.h"
#include "multio/sink/DataSink.h"

namespace multio::action {

using message::glossary;

SingleFieldSink::SingleFieldSink(const ComponentConfiguration& compConf) :
    Action{compConf}, rootPath_{compConf.parsedConfig().getString("root_path", "")} {}

void SingleFieldSink::executeImpl(Message msg) {
    switch (msg.tag()) {
        case Message::Tag::Field:
            write(msg);
            break;

        case Message::Tag::Flush:
            flush();
            break;

        default:
            ASSERT(false);
    }
}

void SingleFieldSink::write(Message msg) {
    util::ScopedTiming timing{statistics_.actionTiming_};

    std::string paramOrId;
    auto searchParam = msg.metadata().find(glossary().param);
    if (searchParam != msg.metadata().end()) {
        paramOrId = searchParam->second.get<std::string>();
    }
    else {
        auto searchParamId = msg.metadata().find(glossary().paramId);
        if (searchParamId != msg.metadata().end()) {
            paramOrId = eckit::translate<std::string>(searchParamId->second.get<std::int64_t>());
        }
        else {
            std::ostringstream oss;
            oss << "SingleFieldSink::write: No param or paramId found in metadata:";
            throw eckit::UserError(oss.str(), Here());
        }
    }


    auto paramStr = util::visitTranslate<std::string>(msg.metadata().get("param"));
    if (!paramStr) {
        std::ostringstream oss;
        oss << "Sink::trigger: Value for param can not be translated to string: ";
        oss << msg.metadata().get("param");
        throw eckit::UserError(oss.str(), Here());
    }

    std::ostringstream oss;
    oss << rootPath_ << msg.metadata().get<std::int64_t>("level") << "::" << paramOrId
        << "::" << msg.metadata().get<std::int64_t>("step");
    eckit::LocalConfiguration config;

    LOG_DEBUG_LIB(LibMultio) << "Writing output path: " << oss.str() << std::endl;
    config.set("path", oss.str());
    dataSinks_.push_back(
        sink::DataSinkFactory::instance().build("file", ComponentConfiguration(config, compConf_.multioConfig())));

    eckit::message::Message blob = to_eckit_message(msg);

    dataSinks_.back()->write(blob);
}

void SingleFieldSink::flush() const {
    util::ScopedTiming timing{statistics_.actionTiming_};

    eckit::Log::debug<LibMultio>() << "*** Executing single-field flush for data sinks... " << std::endl;

    for (const auto& sink : dataSinks_) {
        if (sink) {
            sink->flush();
        }
    }
}

void SingleFieldSink::print(std::ostream& os) const {
    for (const auto& sink : dataSinks_) {
        if (sink) {
            os << "Sink(DataSink=" << *sink << ")";
        }
        else {
            os << "Sink(DataSink=NULL)";
        }
    }
}

static ActionBuilder<SingleFieldSink> SinkBuilder("single-field-sink");

}  // namespace multio::action

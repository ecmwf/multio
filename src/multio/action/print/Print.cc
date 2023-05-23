/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "Print.h"

#include <fstream>

#include "eckit/config/Configuration.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/log/Log.h"

namespace multio::action {

Print::Print(const ComponentConfiguration& compConf) : ChainedAction(compConf) {
    stream_ = compConf.parsedConfig().getString("stream", "info");
    onlyFields_ = compConf.parsedConfig().getBool("only-fields", false);

    if (stream_ == "info") {
        os_ = &eckit::Log::info();
    }
    else if (stream_ == "error") {
        os_ = &eckit::Log::error();
    }
    else if (stream_ == "cout") {
        os_ = &std::cout;
    }
    else {
        os_ = &eckit::Log::debug();
    }

    prefix_ = compConf.parsedConfig().getString("prefix", "");
}

void Print::executeImpl(message::Message msg) {
    ASSERT(os_);
    bool doOutput = onlyFields_ ? msg.tag() == message::Message::Tag::Field ? true : false : true;
    if (doOutput) {
        if (!prefix_.empty()) {
            (*os_) << prefix_ << ": ";
        }
        (*os_) << msg << std::endl;
    }
    executeNext(std::move(msg));
}

void Print::print(std::ostream& os) const {
    os << "Print(stream=" << stream_ << ")";
}


static ActionBuilder<Print> PrintBuilder("print");

}  // namespace multio::action

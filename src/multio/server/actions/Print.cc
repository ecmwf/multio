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

#include "eckit/exception/Exceptions.h"
#include "eckit/log/Log.h"
#include "eckit/config/Configuration.h"

namespace multio {
namespace server {
namespace actions {

Print::Print(const eckit::Configuration& config) : Action(config) {
    stream_ = config.getString("stream", "info");

    if (stream_ == "info") {
        os = &eckit::Log::info();
    }
    else {
        os = &eckit::Log::error();
    }
}

void Print::execute(Message msg) const {
    ASSERT(os);
    (*os) << msg << std::endl;

    if (next_) {  // May want to assert not next_
        next_->execute(msg);
    }
}

void Print::print(std::ostream& os) const {
    os << "Print(stream=" << stream_ << ")";
}


static ActionBuilder<Print> PrintBuilder("Print");

}  // namespace actions
}  // namespace server
}  // namespace multio

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

namespace multio {
namespace sandbox {
namespace actions {

Sink::Sink(const eckit::Configuration& config) :
    Action(config),
    data_sink_(config.getString("datasink")) {}

bool Sink::execute(Message /*message*/) {
    if (data_sink_ == "file") {
        // Sink to file;
    }
    return true;
}

void Sink::print(std::ostream& os) const {
    os << "Sink(data_sink=" << data_sink_ << ")";
}

static ActionBuilder<Sink> SinkBuilder("Sink");

}  // namespace actions
}  // namespace sandbox
}  // namespace multio

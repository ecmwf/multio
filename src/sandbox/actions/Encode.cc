/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "Encode.h"

#include <iostream>

#include "eckit/config/Configuration.h"

namespace multio {
namespace sandbox {
namespace actions {

Encode::Encode(const eckit::Configuration& config) :
    Action(config),
    format_(config.getString("format")) {}

bool Encode::execute(Message /*message*/) {
    // TODO: grib encoding not yet implemented
    if (format_ == "grib") {
        // Encode to grib;
    }
    return true;
}

void Encode::print(std::ostream& os) const {
    os << "Encode(format=" << format_ << ")";
}

static ActionBuilder<Encode> EncodeBuilder("Encode");

}  // namespace actions
}  // namespace sandbox
}  // namespace multio

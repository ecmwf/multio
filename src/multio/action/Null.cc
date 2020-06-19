/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "Null.h"

#include <iostream>

#include "eckit/exception/Exceptions.h"

namespace multio {
namespace action {

Null::Null(const eckit::Configuration& config) : Action(config) {}

void Null::doExecute(message::Message /*msg*/) const {
    ASSERT(!next_);
}

void Null::print(std::ostream& os) const {
    os << "Null()";
}

static ActionBuilder<Null> NullBuilder("Null");

}  // namespace actions
}  // namespace multio

/*
 * (C) Copyright 1996-2015 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Tiago Quintino
/// @author Simon Smart
/// @date Dec 2015

#include "multio/Journal.h"

#include "eckit/log/Log.h"

using namespace eckit;

namespace multio {

//----------------------------------------------------------------------------------------------------------------------

Journal::Journal() {
}

Journal::~Journal() {
}

void Journal::print(std::ostream& os) const
{
    os << "Journal()";
}

//----------------------------------------------------------------------------------------------------------------------

}  // namespace multio


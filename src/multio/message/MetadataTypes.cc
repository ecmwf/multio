/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Philipp Geier

/// @date Sept 2023

#include "MetadataTypes.h"

namespace multio::message {

std::ostream& operator<<(std::ostream& os, const Null&) {
    os << "null";
    return os;
}

eckit::JSON& operator<<(eckit::JSON& json, const Null&) {
    json.null();
    return json;
}


}  // namespace multio::message

/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "multio/mars2grib/Mars2GribException.h"

namespace multio::mars2grib {

std::string encodingExceptionReason(const std::string& r) {
    std::string s("Mars2Grib exception: ");
    s.append(r);
    return s;
}


Mars2GribException::Mars2GribException(const std::string& r, const eckit::CodeLocation& l) :
    eckit::Exception(encodingExceptionReason(r), l) {}


}  // namespace multio::action

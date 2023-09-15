/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "multio/action/encode-grib2/Exception.h"

#include <iostream>

namespace multio::action {

namespace {

std::string encodeGrib2ExceptionReason(const std::string& r) {
    std::string s("EncodeGrib2 exception: ");
    s.append(r);
    return s;
}

}  // namespace


EncodeGrib2Exception::EncodeGrib2Exception(const std::string& r, const eckit::CodeLocation& l) :
    eckit::Exception(encodeGrib2ExceptionReason(r), l) {}

}  // namespace multio::action

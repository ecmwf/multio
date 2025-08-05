/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "multio/mars2mars/Mars2MarsException.h"

namespace multio::mars2mars {

std::string encodingExceptionReason(const std::string& r) {
    std::string s("Mars2Mars exception: ");
    s.append(r);
    return s;
}


Mars2MarsException::Mars2MarsException(const std::string& r, const eckit::CodeLocation& l) :
    eckit::Exception(encodingExceptionReason(r), l) {}


}  // namespace multio::action

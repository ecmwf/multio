/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "multio/action/encode-mtg2/EncodeMtg2Exception.h"

namespace multio::action {

std::string encodingExceptionReason(const std::string& r) {
    std::string s("Mtg2 Enocding exception: ");
    s.append(r);
    return s;
}


EncodeMtg2Exception::EncodeMtg2Exception(const std::string& r, const eckit::CodeLocation& l) :
    eckit::Exception(encodingExceptionReason(r), l) {}


}  // namespace multio::action

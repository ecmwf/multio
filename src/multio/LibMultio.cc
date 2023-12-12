/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Baudouin Raoult
/// @author Tiago Quintino
/// @author Simon Smart
/// @date   March 2017

#include <algorithm>
#include <string>

#include "multio/LibMultio.h"
#include "multio/multio_version.h"

namespace multio {

//----------------------------------------------------------------------------------------------------------------------

REGISTER_LIBRARY(LibMultio);

LibMultio::LibMultio() : Library("multio") {}


const LibMultio& LibMultio::instance() {
    static LibMultio libmultio;
    return libmultio;
}


const void* LibMultio::addr() const {
    return this;
}


std::string LibMultio::version() const {
    return multio_version_str();
}

std::string LibMultio::gitsha1(unsigned int count) const {
    std::string sha1(multio_git_sha1());
    if (sha1.empty()) {
        return "not available";
    }

    return sha1.substr(0, std::min(count, 40u));
}

//----------------------------------------------------------------------------------------------------------------------

}  // namespace multio

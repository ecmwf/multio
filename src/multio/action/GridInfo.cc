/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Domokos Sarmany

/// @date Aug 2020

#include "GridInfo.h"

#include <cstring>

#include "eckit/exception/Exceptions.h"

namespace multio {
namespace action {

GridInfo::GridInfo() {}

void GridInfo::setSubtype(const std::string& subtype) {
    if (gridSubtype_.empty()) {
        eckit::Log::info() << "*** Setting subtype " << std::endl;
        gridSubtype_ = subtype;
    }

    ASSERT(gridSubtype_ == subtype);
}

void GridInfo::setLatitudes(message::Message msg) {
    ASSERT(latitudes_.size() == 0);

    eckit::Log::info() << "*** Setting latitudes " << std::endl;

    latitudes_ = msg;
}

void GridInfo::setLongitudes(message::Message msg) {
    ASSERT(longitudes_.size() == 0);

    eckit::Log::info() << "*** Setting longitudes " << std::endl;

    longitudes_ = msg;
}

const message::Message& GridInfo::latitudes() const {
    return latitudes_;
}

const message::Message& GridInfo::longitudes() const {
    return longitudes_;
}

bool GridInfo::computeHashIfCan() {
    if ((latitudes_.payload().size() == 0) || (longitudes_.payload().size() == 0)) {
        return false;
    }

    ASSERT(not gridSubtype_.empty()); // Paranoia -- this should never happen

    // TODO: compute md5 hash here
    hash_ = gridSubtype_ + std::string{"a1b2c3d4e5"};

    return true;
}

const std::string& GridInfo::strHash() const {
    return hash_;
}

const unsigned char* GridInfo::byteHash() const {
    return reinterpret_cast<const unsigned char*>(hash_.c_str());
}

}  // namespace action
}  // namespace multio

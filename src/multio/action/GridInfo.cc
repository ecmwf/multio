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
#include "multio/action/Endian.h"

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

    addToHash(latitudes_.payload());
    addToHash(longitudes_.payload());
    hashFunction_.add(gridSubtype_.c_str(), gridSubtype_.size());

    hashFunction_.numericalDigest(hashValue_);

    eckit::Log::info() << "*** Setting hash value " << hashValue_ << " with supposed size "
                       << DIGEST_LENGTH << " and with actual size "
                       << std::strlen(reinterpret_cast<const char*>(hashValue_)) << std::endl;

    return true;
}

bool GridInfo::hashExists() const {
    return  static_cast<bool>(hashValue_[0]);
}

const unsigned char* GridInfo::hashValue() const {
    return &hashValue_[0];
}

void GridInfo::addToHash(const eckit::Buffer& buf) {
    auto ptr = reinterpret_cast<const double*>(buf.data());
    auto sz = buf.size() / sizeof(double);
    for (size_t it = 0; it != sz; ++it) {
        hashFunction_.add(Endian<double>::to_little_endian(*ptr++));
    }
}

}  // namespace action
}  // namespace multio

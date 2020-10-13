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
#include <iomanip>
#include <sstream>

#include "eckit/exception/Exceptions.h"
#include "eckit/system/SystemInfo.h"
#include "eckit/utils/ByteSwap.h"

#include "multio/LibMultio.h"

namespace multio {
namespace action {

namespace  {
template <typename T>
eckit::Buffer byteswap(const eckit::Buffer& buf) {
    eckit::Buffer ret{static_cast<const char*>(buf), buf.size()};  // Create a local copy

    auto ret_ptr = reinterpret_cast<T*>(ret.data());
    auto ret_sz = buf.size() / sizeof(T);

    eckit::byteswap(ret_ptr, ret_sz);

    return ret;
}
}  // namespace

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

    hashFunction_.add(gridSubtype_.c_str(), gridSubtype_.size());
    addToHash(latitudes_.payload());
    addToHash(longitudes_.payload());

    hashFunction_.numericalDigest(hashValue_);

    std::ostringstream oss;
    oss << "*** Computed hash value: ";
    for (int i = 0; i < DIGEST_LENGTH; ++i) {
        oss << ((i == 0) ? "" : "-") << std::hex << std::setfill('0') << std::setw(2)
            << static_cast<short>(hashValue_[i]);
    }
    LOG_DEBUG_LIB(LibMultio) << oss.str() << std::endl;

    return true;
}

bool GridInfo::hashExists() const {
    return  static_cast<bool>(hashValue_[0]);
}

const unsigned char* GridInfo::hashValue() const {
    return &hashValue_[0];
}

void GridInfo::addToHash(const eckit::Buffer& buf) {
    if (eckit::system::SystemInfo::isBigEndian()) {
        auto swappedBuf = byteswap<double>(buf);

        hashFunction_.add(swappedBuf.data(), swappedBuf.size());
    } else {
        hashFunction_.add(buf.data(), buf.size());
    }
}

}  // namespace action
}  // namespace multio

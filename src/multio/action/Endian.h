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

#ifndef multio_server_actions_Endian_H
#define multio_server_actions_Endian_H

#include "eckit/system/SystemInfo.h"

namespace multio {
namespace action {

template <typename T>
class Endian {
public:

    static T to_little_endian(T val) {
        return (eckit::system::SystemInfo::isBigEndian()) ? transform(val) : val;
    }

    static T to_big_endian(T val) {
        return (eckit::system::SystemInfo::isLittleEndian()) ? transform(val) : val;
    }

private:

    static T transform(T val) {
        auto ptr = reinterpret_cast<unsigned char*>(&val);
        for (int i = 0; i < mid_; i++) {
            std::swap(ptr[i], ptr[last_ - i]);
        }
        return val;
    }

    static const size_t mid_ = sizeof(T) >> 1;
    static const size_t last_ = sizeof(T) - 1;
};

}  // namespace action
}  // namespace multio

#endif

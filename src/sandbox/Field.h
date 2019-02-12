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
/// @author Simon Smart
/// @author Tiago Quintino

/// @date Jan 2019

#ifndef multio_sandbox_Field_H
#define multio_sandbox_Field_H

#include <cstring>
#include <type_traits>

#include "eckit/config/LocalConfiguration.h"
#include "eckit/io/Buffer.h"
#include "eckit/parser/JSON.h"

namespace multio {
namespace sandbox {

std::string pack_configuration(const eckit::LocalConfiguration& config) {
    std::stringstream ss;
    eckit::JSON json(ss);
    json << config;
    return ss.str();
}

template <typename T, typename = typename std::enable_if<std::is_arithmetic<T>::value, T>::type>
struct Field {
    eckit::LocalConfiguration metadata_;
    std::vector<T> data_;

    eckit::Buffer to_payload() const {
        auto meta_str = pack_configuration(metadata_);

        auto sz = meta_str.size();
        auto sz_bm = sizeof(decltype(sz));
        auto data_size = data_.size() * sizeof(T);

        // Create destination buffer
        auto tot_size = sz_bm + sz + data_size;
        eckit::Buffer payload(tot_size);

        char* ptr = payload;

        // Copy the size of the metadata into the destination buffer
        std::memcpy(ptr, &sz, sz_bm);
        ptr += sz_bm;

        // Copy the metadata into the destination buffer
        std::memcpy(ptr, meta_str.data(), sz);
        ptr += sz;

        // Copy the field data into the destination buffer
        std::memcpy(ptr, data_.data(), data_size);

        return payload;
    }
};

}  // namespace sandbox
}  // namespace multio

#endif

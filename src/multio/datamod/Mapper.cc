/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "multio/datamod/Mapper.h"

#include <charconv>

#include "multio/datamod/core/DataModellingException.h"


namespace multio::datamod::mapper {

std::int64_t toIntStrict(const std::string& s) {
    std::int64_t value;
    auto [ptr, ec] = std::from_chars(s.data(), s.data() + s.size(), value);
    if (ec != std::errc() || ptr != s.data() + s.size()) {
        throw DataModellingException("toIntStrict: Invalid characters in input: " + s, Here());
    }
    return value;
}

std::int64_t StringToIntMapper::parse(const std::string& v) {
    return toIntStrict(v);
}

bool BoolMapper::parse(const std::string& v) {
    return toIntStrict(v) > 0;
}

}  // namespace multio::datamod::mapper

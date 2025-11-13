/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#pragma once


#include "core/TypeParserDumper.h"

#include <cstdint>
#include <string>


namespace multio::datamod {

template <>
struct ParseType<int64_t> {
    static int64_t parse(const std::string& s);
};

template <>
struct ParseType<bool> {
    static bool parse(int64_t v) { return v > 0; };
    static bool parse(const std::string& s) { return ParseType<int64_t>::parse(s) > 0; };
};

}  // namespace multio::datamod

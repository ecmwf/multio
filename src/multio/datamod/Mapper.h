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


#include <cstdint>
#include <string>


namespace multio::datamod::mapper {

struct StringToIntMapper {
    static inline std::int64_t dump(std::int64_t v) noexcept { return v; };
    static inline std::int64_t parse(std::int64_t v) noexcept { return v; };
    static std::int64_t parse(const std::string& v);
};

struct ParamMapper {
    static std::int64_t dump(std::int64_t) noexcept;
    static std::int64_t parse(std::int64_t) noexcept;
    static std::int64_t parse(const std::string&);
};

struct BoolMapper {
    static inline bool dump(bool v) noexcept { return v; };
    static inline bool parse(bool v) noexcept { return v; };
    static inline bool parse(std::int64_t v) { return v > 0; };
    static bool parse(const std::string& v);
};


}  // namespace multio::datamod::mapper


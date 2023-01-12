/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Philipp Geier

/// @date Jan 2023

#pragma once

#include <iostream>
#include <map>
#include <optional>  // C++17 available at the point of using
#include <sstream>
#include <string>
#include <string_view>  // C++17 available at the point of using

#include "eckit/config/Configuration.h"

namespace multio {
namespace util {

static const std::string FISH_START_DELIM{"<<"};
static const std::string FISH_END_DELIM{">>"};

template <typename Func>
std::string replaceFish(std::string_view s, Func&& lookup) {
    std::ostringstream oss;

    for (;;) {
        auto pos = s.find(FISH_START_DELIM);
        if (pos == std::string_view::npos) {
            oss << s;
            return oss.str();
        }
        auto end = s.find(FISH_END_DELIM, pos + FISH_START_DELIM.size());
        if (end == std::string_view::npos) {
            oss << s;
            return oss.str();
        }
        auto const key = s.substr(pos + FISH_START_DELIM.size(), end - pos - FISH_END_DELIM.size());
        auto const subMaybe = lookup(key);
        if (subMaybe) {
            oss << (s.substr(0, pos)) << (*subMaybe);
            s = s.substr(end + FISH_END_DELIM.size());
        }
        else {
            oss << (s.substr(0, end));
            s = s.substr(end);
        }
    }
}


// Example
std::string replaceFish(std::string_view s, const std::map<std::string, std::string>& replacements);


// Example
std::string replaceFish(std::string_view s, const eckit::Configuration& replacements);

}  // namespace util
}  // namespace multio

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

/*
 * Replacement of (single) curly braces analogous to PGEN. Example: {var}
 * Note: Explicitly wrap strings in YAML with '' to avoid problems with BASH substitution
 * PGEN example template: https://git.ecmwf.int/projects/PRODGEN/repos/pgen/browse/tests/fields/naming/template.yaml
 * PGEN substitution code: https://git.ecmwf.int/projects/PRODGEN/repos/pgen/browse/src/pgen/sinks/TemplatedName.cc#89

 * TODO: Think about an escaping mechanism, i.e. \{  and \}, or {{ and }}
 */
template <typename Func>
std::string replaceCurly(std::string_view s, Func&& lookup) {
    static const std::string START_DELIM{"{"};
    static const std::string END_DELIM{"}"};
    std::ostringstream oss;

    for (;;) {
        auto pos = s.find(START_DELIM);
        if (pos == std::string_view::npos) {
            oss << s;
            return oss.str();
        }
        auto end = s.find(END_DELIM, pos + START_DELIM.size());
        if (end == std::string_view::npos) {
            oss << s;
            return oss.str();
        }
        auto const key = s.substr(pos + START_DELIM.size(), end - pos - END_DELIM.size());
        auto const subMaybe = lookup(key);
        if (subMaybe) {
            oss << (s.substr(0, pos)) << (*subMaybe);
            s = s.substr(end + END_DELIM.size());
        }
        else {
            oss << (s.substr(0, end));
            s = s.substr(end);
        }
    }
}

std::string replaceCurly(std::string_view s, const eckit::Configuration& replacements);

}  // namespace util
}  // namespace multio

/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/// @file
/// @brief Options and context utilities for `grib2grib`.

#include "multio/tools/grib2grib/OptionsUtils.h"

#include <fstream>
#include <sstream>

#include "eckit/config/YAMLConfiguration.h"
#include "eckit/exception/Exceptions.h"

namespace multio::distGrib1ToGrib2::grib2grib {

namespace {

/// @brief Apply local post-parse normalization to the raw options tree.
/// @param options Parsed YAML options mutated in place.
///
/// The current implementation is intentionally a no-op. The helper remains so
/// normalization logic can be reintroduced without changing call sites.
void normalizeOptions(eckit::LocalConfiguration& options) {
    (void)options;
}

}  // namespace

/// @brief Read an options YAML file into a single string payload.
/// @param yamlFile Path to the YAML file on disk.
/// @return Full file contents preserved as text.
/// @throw eckit exception If the file cannot be opened or read completely.
std::string readOptionsFileAsString(const std::string& yamlFile) {
    std::ifstream in(yamlFile);
    if (!in) {
        throw eckit::CantOpenFile(yamlFile, Here());
    }

    std::ostringstream out;
    out << in.rdbuf();

    if (!in.good() && !in.eof()) {
        throw eckit::ReadError("error while reading options file: " + yamlFile, Here());
    }

    return out.str();
}

/// @brief Parse a YAML text payload into an eckit local configuration.
/// @param payload YAML text payload, not an eckit debug dump.
/// @return Parsed local configuration ready for validation and context parsing.
/// @throw eckit exception If the payload is empty or not valid YAML.
eckit::LocalConfiguration parseOptionsYaml(const std::string& payload) {
    if (payload.empty()) {
        throw eckit::BadValue("empty options payload", Here());
    }

    if (payload.find("LocalConfiguration[root=") != std::string::npos) {
        throw eckit::BadValue(
            "invalid options payload: received an eckit LocalConfiguration debug dump instead of YAML", Here());
    }

    std::istringstream in(payload);
    eckit::YAMLConfiguration yaml(in);
    eckit::LocalConfiguration options(yaml);
    normalizeOptions(options);
    return options;
}

}  // namespace multio::distGrib1ToGrib2::grib2grib

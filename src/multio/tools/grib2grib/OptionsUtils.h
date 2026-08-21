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

#pragma once

#include <string>

#include "eckit/config/LocalConfiguration.h"

#include "multio/tools/grib2grib/GlobalContext.h"

namespace eckit::mpi {
class Comm;
}

namespace multio::distGrib1ToGrib2::grib2grib {

/// @brief Read an options YAML file into a single string payload.
/// @param yamlFile Path to the YAML file on disk.
/// @return Full file contents preserved as text.
/// @throw eckit exception If the file cannot be opened or read completely.
std::string readOptionsFileAsString(const std::string& yamlFile);

/// @brief Parse a YAML text payload into an eckit local configuration.
/// @param payload YAML text payload, not an eckit debug dump.
/// @return Parsed local configuration ready for validation and context parsing.
/// @throw eckit exception If the payload is empty or not valid YAML.
eckit::LocalConfiguration parseOptionsYaml(const std::string& payload);

/// @brief Load an options file on rank 0, broadcast it, and parse it on every rank.
/// @param yamlFile Path to the YAML file visible on rank 0.
/// @param comm MPI communicator used to broadcast the text payload.
/// @return Parsed local configuration reconstructed on every rank.
/// @throw eckit exception If file loading, broadcasting, or YAML parsing fail.
eckit::LocalConfiguration loadAndBroadcastOptionsAsConfiguration(const std::string& yamlFile,
                                                                 const eckit::mpi::Comm& comm);

}  // namespace multio::distGrib1ToGrib2::grib2grib

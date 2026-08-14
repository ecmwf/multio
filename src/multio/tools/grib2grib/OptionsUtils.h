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

std::string readOptionsFileAsString(const std::string& yamlFile);

eckit::LocalConfiguration parseOptionsYaml(const std::string& payload);

eckit::LocalConfiguration loadAndBroadcastOptionsAsConfiguration(const std::string& yamlFile,
                                                                 const eckit::mpi::Comm& comm);

}  // namespace multio::distGrib1ToGrib2::grib2grib

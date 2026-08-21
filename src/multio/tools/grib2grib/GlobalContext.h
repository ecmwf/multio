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
/// @brief Aggregated stage context for the isolated `grib2grib` pipeline.

#pragma once

#include "eckit/config/LocalConfiguration.h"

#include "multio/tools/grib2grib/ReaderContext.h"
#include "multio/tools/grib2grib/stages/Grib2Fdb5.h"
#include "multio/tools/grib2grib/stages/GribBasedFilter.h"
#include "multio/tools/grib2grib/stages/GribToMars.h"
#include "multio/tools/grib2grib/stages/MarsBasedFilter.h"
#include "multio/tools/grib2grib/stages/MarsToGrib.h"
#include "multio/tools/grib2grib/stages/MarsToMars.h"
#include "multio/tools/grib2grib/stages/Overrides.h"
#include "multio/tools/grib2grib/stages/PostEncodeValidation.h"

namespace multio::distGrib1ToGrib2::grib2grib {

/// @brief Immutable configuration-derived context for the full `grib2grib` pipeline.
///
/// `GlobalContext` is the single runtime bundle produced from the strict YAML
/// schema rooted at `reader` and `stages`. It contains only parsed configuration
/// state. It intentionally does not own rank-local runtime resources such as
/// sinks, files, or MPI communicators.
struct GlobalContext {
    ReaderContext reader;
    GribBasedFilterContext gribBasedFilter;
    GribToMarsContext gribToMars;
    MarsToMarsContext marsToMars;
    OverridesContext overrides;
    MarsBasedFilterContext marsBasedFilter;
    MarsToGribContext marsToGrib;
    PostEncodeValidationContext postEncodeValidation;
    Grib2Fdb5Context grib2Fdb5;
};

/// @brief Validate the strict `grib2grib` YAML configuration.
/// @param config Parsed root configuration.
/// @throw eckit::BadValue If required sections are missing or malformed.
void validateGlobalContext(const eckit::LocalConfiguration& config);

/// @brief Parse the strict `grib2grib` YAML configuration into stage contexts.
/// @param config Parsed root configuration.
/// @return Fully materialized immutable stage contexts.
GlobalContext parseGlobalContext(const eckit::LocalConfiguration& config);

/// @brief Symmetric no-throw cleanup hook for stage contexts.
/// @param context Parsed global context.
void freeGlobalContext(GlobalContext& context) noexcept;

}  // namespace multio::distGrib1ToGrib2::grib2grib

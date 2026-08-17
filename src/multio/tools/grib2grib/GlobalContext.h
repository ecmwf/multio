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

#include "multio/tools/grib2grib/stages/Grib2Fdb5.h"
#include "multio/tools/grib2grib/stages/GribBasedFilter.h"
#include "multio/tools/grib2grib/stages/GribToMars.h"
#include "multio/tools/grib2grib/stages/MarsBasedFilter.h"
#include "multio/tools/grib2grib/stages/MarsToGrib.h"
#include "multio/tools/grib2grib/stages/MarsToMars.h"
#include "multio/tools/grib2grib/stages/Overrides.h"
#include "multio/tools/grib2grib/stages/PostEncodeValidation.h"

namespace multio::distGrib1ToGrib2::grib2grib {

struct GlobalContext {
    GribBasedFilterContext gribBasedFilter;
    GribToMarsContext gribToMars;
    MarsToMarsContext marsToMars;
    OverridesContext overrides;
    MarsBasedFilterContext marsBasedFilter;
    MarsToGribContext marsToGrib;
    PostEncodeValidationContext postEncodeValidation;
    Grib2Fdb5Context grib2Fdb5;
};

void validateGlobalContext(const eckit::LocalConfiguration& config);

GlobalContext parseGlobalContext(const eckit::LocalConfiguration& config);

void freeGlobalContext(GlobalContext& context) noexcept;

}  // namespace multio::distGrib1ToGrib2::grib2grib

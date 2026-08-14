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
/// @brief Aggregated stage context implementation for the isolated `grib2grib` pipeline.

#include "multio/tools/grib2grib/GlobalContext.h"

namespace multio::distGrib1ToGrib2::grib2grib {

void validateGlobalContext(const eckit::LocalConfiguration& config) {
    validateGribBasedFilterContext(config);
    validateGribToMarsContext(config);
    validateMarsToMarsContext(config);
    validateOverridesContext(config);
    validateMarsBasedFilterContext(config);
    validateMarsToGribContext(config);
    validatePostEncodeValidationContext(config);
    validateGrib2Fdb5Context(config);
}

GlobalContext parseGlobalContext(const eckit::LocalConfiguration& config) {
    GlobalContext context;

    context.gribBasedFilter = parseGribBasedFilterContext(config);
    context.gribToMars = parseGribToMarsContext(config);
    context.marsToMars = parseMarsToMarsContext(config);
    context.overrides = parseOverridesContext(config);
    context.marsBasedFilter = parseMarsBasedFilterContext(config);
    context.marsToGrib = parseMarsToGribContext(config);
    context.postEncodeValidation = parsePostEncodeValidationContext(config);
    context.grib2Fdb5 = parseGrib2Fdb5Context(config);

    return context;
}

void freeGlobalContext(GlobalContext& context) noexcept {
    freeGrib2Fdb5Context(context.grib2Fdb5);
    freePostEncodeValidationContext(context.postEncodeValidation);
    freeMarsToGribContext(context.marsToGrib);
    freeMarsBasedFilterContext(context.marsBasedFilter);
    freeOverridesContext(context.overrides);
    freeMarsToMarsContext(context.marsToMars);
    freeGribToMarsContext(context.gribToMars);
    freeGribBasedFilterContext(context.gribBasedFilter);
}

}  // namespace multio::distGrib1ToGrib2::grib2grib

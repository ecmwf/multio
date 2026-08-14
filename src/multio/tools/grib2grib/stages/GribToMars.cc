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
/// @brief Standalone `GribToMars` stage implementation for the isolated `grib2grib` pipeline.

#include "multio/tools/grib2grib/stages/GribToMars.h"

#include "eckit/exception/Exceptions.h"

#include "metkit/grib2mars/api/Grib2Mars.h"

#include "multio/tools/grib2grib/Utils.h"

namespace multio::distGrib1ToGrib2::grib2grib {

void validateGribToMarsContext(const eckit::LocalConfiguration& config) {
    if (config.has("verbosity")) {
        (void)config.getLong("verbosity");
    }
}

GribToMarsContext parseGribToMarsContext(const eckit::LocalConfiguration& config) {
    GribToMarsContext parsed;

    parsed.verbosity = config.has("verbosity") ? config.getLong("verbosity") : 0;
    if (parsed.verbosity < 0) {
        parsed.verbosity = 0;
    }
    if (parsed.verbosity > 3) {
        parsed.verbosity = 3;
    }

    return parsed;
}

void freeGribToMarsContext(GribToMarsContext& context) noexcept {
    (void)context;
}

GribToMarsResult runGribToMarsStage(const metkit::codes::CodesHandle& inputHandle,
                                    const GribToMarsContext& context) noexcept {
    (void)context;

    GribToMarsResult result;

    try {
        metkit::grib2mars::Grib2Mars grib2mars;
        const auto marsMisc = grib2mars.convert<eckit::LocalConfiguration>(inputHandle);
        result.mars = marsMisc.mars;
        result.misc = marsMisc.misc;
    }
    catch (...) {
        printTrappedErrorDisclaimer();
        result.outcome = GribToMarsCode::MapGribToMarsFailed;
        return result;
    }

    try {
        result.values = inputHandle.getDoubleArray("values");
    }
    catch (...) {
        printTrappedErrorDisclaimer();
        result.outcome = GribToMarsCode::ValuesExtractionFailed;
        return result;
    }

    result.outcome = GribToMarsCode::Valid;
    return result;
}

}  // namespace multio::distGrib1ToGrib2::grib2grib

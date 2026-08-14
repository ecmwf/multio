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
/// @brief Standalone `MarsToMars` stage implementation for the isolated `grib2grib` pipeline.

#include "multio/tools/grib2grib/stages/MarsToMars.h"

#include "eckit/exception/Exceptions.h"

#include "metkit/mars2mars/api/Mars2Mars.h"

#include "multio/tools/grib2grib/Utils.h"

namespace multio::distGrib1ToGrib2::grib2grib {

namespace implementation {

eckit::LocalConfiguration mergeLocalConfigurations(const eckit::LocalConfiguration& base,
                                                   const eckit::LocalConfiguration& overwrite) {
    eckit::LocalConfiguration result{base};
    for (const auto& key : overwrite.keys()) {
        if (overwrite.isString(key)) {
            result.set(key, overwrite.getString(key));
        }
        else if (overwrite.isIntegral(key)) {
            result.set(key, overwrite.getLong(key));
        }
        else if (overwrite.isFloatingPoint(key)) {
            result.set(key, overwrite.getDouble(key));
        }
        else if (overwrite.isBoolean(key)) {
            result.set(key, overwrite.getBool(key));
        }
        else if (overwrite.isFloatingPointList(key)) {
            result.set(key, overwrite.getDoubleVector(key));
        }
        else {
            throw eckit::NotImplemented("Unexpected type for '" + key + "'", Here());
        }
    }
    return result;
}

}  // namespace implementation

void validateMarsToMarsContext(const eckit::LocalConfiguration& config) {
    if (config.has("verbosity")) {
        (void)config.getLong("verbosity");
    }
}

MarsToMarsContext parseMarsToMarsContext(const eckit::LocalConfiguration& config) {
    MarsToMarsContext parsed;

    parsed.verbosity = config.has("verbosity") ? config.getLong("verbosity") : 0;
    if (parsed.verbosity < 0) {
        parsed.verbosity = 0;
    }
    if (parsed.verbosity > 3) {
        parsed.verbosity = 3;
    }

    return parsed;
}

void freeMarsToMarsContext(MarsToMarsContext& context) noexcept {
    (void)context;
}

MarsToMarsResult runMarsToMarsStage(const eckit::LocalConfiguration& mars, const eckit::LocalConfiguration& misc,
                                    const MarsToMarsContext& context) noexcept {
    (void)context;

    MarsToMarsResult result;
    result.misc = misc;
    eckit::LocalConfiguration mappedMars;
    eckit::LocalConfiguration mappedMisc;

    try {
        metkit::mars2mars::Mars2Mars mars2mars;
        const auto mappedMarsMisc = mars2mars.convert<eckit::LocalConfiguration>(mars);
        mappedMars = mappedMarsMisc.mars;
        mappedMisc = mappedMarsMisc.misc;
        result.mars = mappedMars;
    }
    catch (...) {
        printTrappedErrorDisclaimer();
        result.outcome = MarsToMarsCode::MappingsFailed;
        return result;
    }

    try {
        result.misc = implementation::mergeLocalConfigurations(mappedMisc, misc);
    }
    catch (...) {
        printTrappedErrorDisclaimer();
        result.outcome = MarsToMarsCode::MergeMiscFailed;
        return result;
    }

    result.outcome = MarsToMarsCode::Valid;
    return result;
}

}  // namespace multio::distGrib1ToGrib2::grib2grib

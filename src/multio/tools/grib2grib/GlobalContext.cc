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
///
/// The strict root schema currently consists of:
/// - required top-level `reader`
/// - required top-level `stages`
/// - optional top-level `sink`
/// - optional top-level `debug-sinks`
///
/// `GlobalContext` is intentionally populated only from `reader` and `stages`.
/// Sink runtime state is created separately and does not live in the parsed
/// stage-context model.

#include "multio/tools/grib2grib/GlobalContext.h"

#include <string>

#include "eckit/exception/Exceptions.h"

namespace multio::distGrib1ToGrib2::grib2grib {

namespace {

eckit::LocalConfiguration getRequiredSubConfiguration(const eckit::LocalConfiguration& config, const std::string& key) {
    if (!config.has(key)) {
        throw eckit::BadValue("Missing required configuration section '" + key + "'", Here());
    }

    if (!config.isSubConfiguration(key)) {
        throw eckit::BadValue("Configuration entry '" + key + "' must be a section", Here());
    }

    return config.getSubConfiguration(key);
}

eckit::LocalConfiguration getOptionalStageConfiguration(const eckit::LocalConfiguration& stages,
                                                        const std::string& key) {
    if (!stages.has(key)) {
        return eckit::LocalConfiguration{};
    }

    if (!stages.isSubConfiguration(key)) {
        throw eckit::BadValue("Stage configuration 'stages." + key + "' must be a section", Here());
    }

    return stages.getSubConfiguration(key);
}

void validateReaderContext(const eckit::LocalConfiguration& config) {
    if (!config.has("mode")) {
        return;
    }

    if (!config.isString("mode")) {
        throw eckit::BadValue("reader.mode must be a string", Here());
    }

    (void)parseWorkUnitReaderMode(config.getString("mode"));
}

ReaderContext parseReaderContext(const eckit::LocalConfiguration& config) {
    ReaderContext context;

    if (!config.has("mode")) {
        return context;
    }

    context.mode = parseWorkUnitReaderMode(config.getString("mode"));
    return context;
}

}  // namespace

void validateGlobalContext(const eckit::LocalConfiguration& config) {
    const eckit::LocalConfiguration reader = getRequiredSubConfiguration(config, "reader");
    const eckit::LocalConfiguration stages = getRequiredSubConfiguration(config, "stages");

    validateReaderContext(reader);
    validateGribBasedFilterContext(getOptionalStageConfiguration(stages, "grib-based-filter"));
    validateGribToMarsContext(getOptionalStageConfiguration(stages, "grib-to-mars"));
    validateMarsToMarsContext(getOptionalStageConfiguration(stages, "mars-to-mars"));
    validateOverridesContext(getOptionalStageConfiguration(stages, "overrides"));
    validateMarsBasedFilterContext(getOptionalStageConfiguration(stages, "mars-based-filter"));
    validateMarsToGribContext(getOptionalStageConfiguration(stages, "mars-to-grib"));
    validatePostEncodeValidationContext(getOptionalStageConfiguration(stages, "post-encode-validation"));
    validateGrib2Fdb5Context(getOptionalStageConfiguration(stages, "grib2fdb5"));
}

GlobalContext parseGlobalContext(const eckit::LocalConfiguration& config) {
    const eckit::LocalConfiguration reader = getRequiredSubConfiguration(config, "reader");
    const eckit::LocalConfiguration stages = getRequiredSubConfiguration(config, "stages");

    GlobalContext context;

    context.reader = parseReaderContext(reader);
    context.gribBasedFilter = parseGribBasedFilterContext(getOptionalStageConfiguration(stages, "grib-based-filter"));
    context.gribToMars = parseGribToMarsContext(getOptionalStageConfiguration(stages, "grib-to-mars"));
    context.marsToMars = parseMarsToMarsContext(getOptionalStageConfiguration(stages, "mars-to-mars"));
    context.overrides = parseOverridesContext(getOptionalStageConfiguration(stages, "overrides"));
    context.marsBasedFilter = parseMarsBasedFilterContext(getOptionalStageConfiguration(stages, "mars-based-filter"));
    context.marsToGrib = parseMarsToGribContext(getOptionalStageConfiguration(stages, "mars-to-grib"));
    context.postEncodeValidation
        = parsePostEncodeValidationContext(getOptionalStageConfiguration(stages, "post-encode-validation"));
    context.grib2Fdb5 = parseGrib2Fdb5Context(getOptionalStageConfiguration(stages, "grib2fdb5"));

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

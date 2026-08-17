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
/// @brief Draft implementation of the isolated `Overrides` stage.
///
/// This file intentionally provides only the flat stage skeleton. The concrete
/// override logic will be filled in step by step.

#include "multio/tools/grib2grib/stages/Overrides.h"

#include "eckit/exception/Exceptions.h"

#include <unordered_map>

#include "multio/tools/grib2grib/Utils.h"

namespace multio::distGrib1ToGrib2::grib2grib {

namespace implementation {

PackingPolicy parsePackingPolicy(const std::string& value) {
    if (value == "ccsds") {
        return PackingPolicy::Ccsds;
    }
    if (value == "simple") {
        return PackingPolicy::Simple;
    }
    throw eckit::BadValue("Unsupported packing policy: " + value, Here());
}

/// @brief Apply the packing override policy to the MARS dictionary.
/// @param mars MARS dictionary being mutated.
/// @param options Parsed override options.
void applyPackingPolicyOverride(eckit::LocalConfiguration& mars, const OverridesContext& context) {
    const static std::unordered_map<std::string, std::string> ccsdsPackingMap{
        {"grid_simple", "ccsds"}, {"grid_complex", "complex"}, {"spectral_complex", "complex"},
        {"grid_ccsds", "ccsds"},  {"grid_ieee", "ccsds"},      {"grid_second_order", "ccsds"}};

    const static std::unordered_map<std::string, std::string> simplePackingMap{
        {"grid_simple", "simple"}, {"grid_complex", "complex"}, {"spectral_complex", "complex"},
        {"grid_ccsds", "ccsds"},   {"grid_ieee", "ccsds"},      {"grid_second_order", "ccsds"}};

    const auto& packingMap = context.packingPolicy == PackingPolicy::Simple ? simplePackingMap : ccsdsPackingMap;

    if (!mars.has("packing")) {
        throw eckit::BadValue("Overrides packing policy requires mars.packing", Here());
    }

    const auto packing = mars.getString("packing");
    const auto mapped = packingMap.find(packing);
    if (mapped == packingMap.cend()) {
        throw eckit::BadValue("Unhandled mars.packing '" + packing + "' for packing policy '"
                                  + std::string{toString(context.packingPolicy)} + "'",
                              Here());
    }

    mars.set("packing", mapped->second);
}

/// @brief Apply the optional `model` override to the MARS dictionary.
/// @param mars MARS dictionary being mutated.
/// @param options Parsed override options.
void applyModelOverride(eckit::LocalConfiguration& mars, const OverridesContext& context) {
    if (context.modelOverride) {
        mars.set("model", *context.modelOverride);
    }
}

/// @brief Apply the optional generating-process-identifier override.
/// @param misc Misc dictionary being mutated.
/// @param options Parsed override options.
void applyGeneratingProcessIdentifierOverride(eckit::LocalConfiguration& misc, const OverridesContext& context) {
    if (context.generatingProcessIdentifierOverride) {
        misc.set("generatingProcessIdentifier", *context.generatingProcessIdentifierOverride);
    }
}

/// @brief Apply the optional ensemble-size override to the misc dictionary.
/// @param misc Misc dictionary being mutated.
/// @param options Parsed override options.
void applyEnsembleSizeOverride(eckit::LocalConfiguration& misc, const OverridesContext& context) {
    if (context.ensembleSizeOverride) {
        misc.set("numberOfForecastsInEnsemble", *context.ensembleSizeOverride);
    }
}

/// @brief Apply the optional analysis-window-length-in-hours override.
/// @param misc Misc dictionary being mutated.
/// @param options Parsed override options.
void applyAnalysisWindowLengthInHoursOverride(eckit::LocalConfiguration& misc, const OverridesContext& context) {
    if (context.analysisWindowLengthInHoursOverride) {
        misc.set("lengthOfTimeWindow", *context.analysisWindowLengthInHoursOverride);
    }
}

/// @brief Apply the control-forecast override semantics.
/// @param mars MARS dictionary being mutated.
/// @param misc Misc dictionary being mutated.
/// @param options Parsed override options.
void applyControlForecastOverride(eckit::LocalConfiguration& mars, eckit::LocalConfiguration& misc,
                                  const OverridesContext& context) {
    if (!context.control) {
        return;
    }

    if (!mars.has("stream") || !mars.has("type") || mars.getString("stream") != "oper"
        || mars.getString("type") != "fc") {
        throw eckit::UserError("control override is only supported for stream=oper and type=fc in the Overrides stage",
                               Here());
    }

    mars.set("number", 0L);
    misc.set("typeOfEnsembleForecast", 1L);
    misc.set("numberOfForecastsInEnsemble", 51L);
}

/// @brief Apply the optional `expver` override to the MARS dictionary.
/// @param mars MARS dictionary being mutated.
/// @param options Parsed override options.
void applyExpverOverride(eckit::LocalConfiguration& mars, const OverridesContext& context) {
    if (context.expverOverride) {
        mars.set("expver", *context.expverOverride);
    }
}

}  // namespace implementation

/// @brief Convert a packing policy to a stable string representation.
/// @param policy Packing policy value.
/// @return Lowercase string spelling.
const char* toString(PackingPolicy policy) {
    switch (policy) {
        case PackingPolicy::Ccsds:
            return "ccsds";
        case PackingPolicy::Simple:
            return "simple";
    }
    return "unknown-packing-policy";
}

/// @brief Validate the raw `Overrides` options.
/// @param options Stage-local options subconfiguration.
void validateOverridesContext(const eckit::LocalConfiguration& config) {
    if (config.has("packing")) {
        (void)implementation::parsePackingPolicy(config.getString("packing"));
    }
    if (config.has("model")) {
        (void)config.getString("model");
    }
    if (config.has("ncycle")) {
        (void)config.getLong("ncycle");
    }
    if (config.has("ensemble-size")) {
        const auto ensembleSize = config.getLong("ensemble-size");
        if (ensembleSize <= 0) {
            throw eckit::BadValue("ensemble-size must be > 0", Here());
        }
    }
    if (config.has("analysis-window-length-in-hours")) {
        const auto analysisWindowLengthInHours = config.getLong("analysis-window-length-in-hours");
        if (analysisWindowLengthInHours <= 0) {
            throw eckit::BadValue("analysis-window-length-in-hours must be > 0", Here());
        }
    }
    if (config.has("control")) {
        (void)config.getBool("control");
    }
    if (config.has("expver")) {
        (void)config.getString("expver");
    }
    if (config.has("verbosity")) {
        (void)config.getLong("verbosity");
    }
}

/// @brief Parse the stage-local `Overrides` options once for reuse.
/// @param options Stage-local options subconfiguration.
/// @param verbosity Fallback verbosity inherited from the caller.
/// @return Parsed override options.
OverridesContext parseOverridesContext(const eckit::LocalConfiguration& config) {
    OverridesContext parsed;

    if (config.has("packing")) {
        parsed.packingPolicy = implementation::parsePackingPolicy(config.getString("packing"));
    }
    if (config.has("model")) {
        const auto model = config.getString("model");
        if (!model.empty()) {
            parsed.modelOverride = model;
        }
    }
    if (config.has("ncycle")) {
        const auto ncycle = config.getLong("ncycle");
        if (ncycle > 0) {
            parsed.generatingProcessIdentifierOverride = ncycle;
        }
    }
    if (config.has("ensemble-size")) {
        parsed.ensembleSizeOverride = config.getLong("ensemble-size");
    }
    if (config.has("analysis-window-length-in-hours")) {
        parsed.analysisWindowLengthInHoursOverride = config.getLong("analysis-window-length-in-hours");
    }
    if (config.has("control")) {
        parsed.control = config.getBool("control");
    }
    if (config.has("expver")) {
        const auto expver = config.getString("expver");
        if (!expver.empty()) {
            parsed.expverOverride = expver;
        }
    }

    parsed.verbosity = config.has("verbosity") ? config.getLong("verbosity") : 0;
    if (parsed.verbosity < 0) {
        parsed.verbosity = 0;
    }
    if (parsed.verbosity > 3) {
        parsed.verbosity = 3;
    }

    return parsed;
}

void freeOverridesContext(OverridesContext& context) noexcept {
    (void)context;
}

/// @brief Apply the override stage to the post-`MarsToMars` dictionaries.
/// @param mars Input MARS dictionary.
/// @param misc Input misc dictionary.
/// @param options Parsed override options.
/// @return Overridden `mars` and `misc` dictionaries.
OverrideResult runOverridesStage(const eckit::LocalConfiguration& mars, const eckit::LocalConfiguration& misc,
                                 const OverridesContext& context) noexcept {
    OverrideResult result;

    try {
        result.mars = mars;
        result.misc = misc;

        implementation::applyPackingPolicyOverride(result.mars, context);
        implementation::applyModelOverride(result.mars, context);
        implementation::applyGeneratingProcessIdentifierOverride(result.misc, context);
        implementation::applyEnsembleSizeOverride(result.misc, context);
        implementation::applyAnalysisWindowLengthInHoursOverride(result.misc, context);
        implementation::applyControlForecastOverride(result.mars, result.misc, context);
        implementation::applyExpverOverride(result.mars, context);

        result.outcome = MarsOverridesCode::Valid;
        return result;
    }
    catch (const eckit::Exception&) {
        printTrappedErrorDisclaimer();
        result.outcome = MarsOverridesCode::OptionOverridesFailed;
        return result;
    }
    catch (...) {
        printTrappedErrorDisclaimer();
        result.outcome = MarsOverridesCode::UnknownFailure;
        return result;
    }
}

}  // namespace multio::distGrib1ToGrib2::grib2grib

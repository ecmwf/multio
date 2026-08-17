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
/// @brief Draft override stage for the isolated `grib2grib` pipeline.
///
/// This stage operates only on the post-`MarsToMars` `mars` and `misc`
/// dictionaries. It does not access the input GRIB message.

#pragma once

#include <cstdint>
#include <optional>
#include <string>

#include "eckit/config/LocalConfiguration.h"

#include "multio/tools/grib2grib/StageOutcomes.h"

namespace multio::distGrib1ToGrib2::grib2grib {

/// @brief Packing override policy.
///
/// `Ccsds` applies the frozen mapping used in `grib2MarsMisc`.
/// `Simple` applies the separate mapping used in `grib1-to-grib2`.
enum class PackingPolicy : std::uint8_t
{
    Ccsds = 0,
    Simple,
};

/// @brief Parsed context consumed by the `Overrides` stage.
struct OverridesContext {
    /// Packing policy applied to the output MARS dictionary.
    PackingPolicy packingPolicy = PackingPolicy::Ccsds;

    /// Optional override for `mars.model`.
    std::optional<std::string> modelOverride;

    /// Optional override for `misc.generatingProcessIdentifier`.
    std::optional<long> generatingProcessIdentifierOverride;

    /// Optional override for `misc.numberOfForecastsInEnsemble`.
    std::optional<long> ensembleSizeOverride;

    /// Optional override for `misc.lengthOfTimeWindow` in hours.
    std::optional<long> analysisWindowLengthInHoursOverride;

    /// Enable control-forecast override semantics.
    bool control = false;

    /// Optional override for `mars.expver`.
    std::optional<std::string> expverOverride;

    /// Local verbosity override for this stage.
    std::int64_t verbosity = 0;
};

/// @brief Result of applying overrides to the `mars` and `misc` dictionaries.
struct OverrideResult {
    MarsOverridesCode outcome = MarsOverridesCode::UnknownFailure;
    eckit::LocalConfiguration mars;
    eckit::LocalConfiguration misc;
};

/// @brief Convert a packing policy to a stable string representation.
/// @param policy Packing policy value.
/// @return Lowercase string spelling.
const char* toString(PackingPolicy policy);

/// @brief Validate the raw context consumed by the `Overrides` stage.
/// @param config Stage-local context subconfiguration.
/// @throw eckit exception If a known option value is invalid.
void validateOverridesContext(const eckit::LocalConfiguration& config);

/// @brief Parse the stage-local `Overrides` context once for reuse.
/// @param config Stage-local context subconfiguration.
/// @return Parsed override context.
OverridesContext parseOverridesContext(const eckit::LocalConfiguration& config);

void freeOverridesContext(OverridesContext& context) noexcept;

/// @brief Apply the override stage to the post-`MarsToMars` dictionaries.
/// @param mars Input MARS dictionary.
/// @param misc Input misc dictionary.
/// @param context Parsed override context.
/// @return Stage outcome together with overridden `mars` and `misc` when valid.
OverrideResult runOverridesStage(const eckit::LocalConfiguration& mars, const eckit::LocalConfiguration& misc,
                                 const OverridesContext& context) noexcept;

}  // namespace multio::distGrib1ToGrib2::grib2grib

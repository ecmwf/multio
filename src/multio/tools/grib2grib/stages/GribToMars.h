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
/// @brief Standalone `GribToMars` stage for the isolated `grib2grib` pipeline.

#pragma once

#include <cstdint>
#include <optional>
#include <vector>

#include "eckit/config/LocalConfiguration.h"

#include "multio/tools/grib2grib/StageOutcomes.h"

namespace metkit::codes {
class CodesHandle;
}

namespace multio::distGrib1ToGrib2::grib2grib {

/// @brief Parsed context consumed by the standalone `GribToMars` stage.
struct GribToMarsContext {
    std::int64_t verbosity = 0;
    std::optional<eckit::LocalConfiguration> apiOptions;
};

/// @brief Result of the standalone `GribToMars` stage.
struct GribToMarsResult {
    GribToMarsCode outcome = GribToMarsCode::UnknownFailure;
    eckit::LocalConfiguration mars;
    eckit::LocalConfiguration misc;
    std::vector<double> values;
};

/// @brief Validate the raw context consumed by the `GribToMars` stage.
/// @param config Stage-local context subconfiguration.
/// @throw eckit exception If a known option value is invalid.
void validateGribToMarsContext(const eckit::LocalConfiguration& config);

/// @brief Parse the stage-local `GribToMars` context once for reuse.
/// @param config Stage-local context subconfiguration.
/// @return Parsed stage-local context.
GribToMarsContext parseGribToMarsContext(const eckit::LocalConfiguration& config);

void freeGribToMarsContext(GribToMarsContext& context) noexcept;

/// @brief Convert one input GRIB handle into `mars`, `misc`, and `values`.
/// @param inputHandle Read-only GRIB handle for the current message.
/// @param context Parsed stage-local context.
/// @return Stage-local result containing the stage outcome and, on success, the
///         converted dictionaries and unpacked values.
GribToMarsResult runGribToMarsStage(const metkit::codes::CodesHandle& inputHandle,
                                    const GribToMarsContext& context) noexcept;

}  // namespace multio::distGrib1ToGrib2::grib2grib

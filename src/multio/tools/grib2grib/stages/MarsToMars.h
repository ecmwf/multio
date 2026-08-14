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
/// @brief Standalone `MarsToMars` stage for the isolated `grib2grib` pipeline.

#pragma once

#include <cstdint>

#include "eckit/config/LocalConfiguration.h"

#include "multio/tools/grib2grib/StageOutcomes.h"

namespace multio::distGrib1ToGrib2::grib2grib {

/// @brief Parsed context consumed by the standalone `MarsToMars` stage.
struct MarsToMarsContext {
    std::int64_t verbosity = 0;
};

/// @brief Result of the standalone `MarsToMars` stage.
struct MarsToMarsResult {
    MarsToMarsCode outcome = MarsToMarsCode::UnknownFailure;
    eckit::LocalConfiguration mars;
    eckit::LocalConfiguration misc;
};

/// @brief Validate the raw context consumed by the `MarsToMars` stage.
/// @param config Stage-local context subconfiguration.
/// @throw eckit exception If a known option value is invalid.
void validateMarsToMarsContext(const eckit::LocalConfiguration& config);

/// @brief Parse the stage-local `MarsToMars` context once for reuse.
/// @param config Stage-local context subconfiguration.
/// @return Parsed stage-local context.
MarsToMarsContext parseMarsToMarsContext(const eckit::LocalConfiguration& config);

void freeMarsToMarsContext(MarsToMarsContext& context) noexcept;

/// @brief Apply the standalone `MarsToMars` stage to `mars` and `misc`.
/// @param mars Input MARS dictionary.
/// @param misc Input misc dictionary.
/// @param context Parsed stage-local context.
/// @return Mapped `mars`, merged `misc`, and the stage outcome.
MarsToMarsResult runMarsToMarsStage(const eckit::LocalConfiguration& mars, const eckit::LocalConfiguration& misc,
                                    const MarsToMarsContext& context) noexcept;

}  // namespace multio::distGrib1ToGrib2::grib2grib

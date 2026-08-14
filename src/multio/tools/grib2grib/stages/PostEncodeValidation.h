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
/// @brief Standalone `PostEncodeValidation` stage for the isolated `grib2grib` pipeline.

#pragma once

#include <cstdint>

#include "metkit/codes/api/CodesAPI.h"
#include "eckit/config/LocalConfiguration.h"

#include "multio/tools/grib2grib/StageOutcomes.h"

namespace metkit::codes {
class CodesHandle;
}

namespace multio::distGrib1ToGrib2::grib2grib {

/// @brief Parsed context consumed by the standalone `PostEncodeValidation` stage.
struct PostEncodeValidationContext {
    std::int64_t verbosity = 0;
};

/// @brief Validate the raw context consumed by the `PostEncodeValidation` stage.
/// @param config Stage-local context subconfiguration.
/// @throw eckit exception If a known option value is invalid.
void validatePostEncodeValidationContext(const eckit::LocalConfiguration& config);

/// @brief Parse the stage-local `PostEncodeValidation` context once for reuse.
/// @param config Stage-local context subconfiguration.
/// @return Parsed stage-local context.
PostEncodeValidationContext parsePostEncodeValidationContext(const eckit::LocalConfiguration& config);

void freePostEncodeValidationContext(PostEncodeValidationContext& context) noexcept;

/// @brief Validate one encoded GRIB2 message.
/// @param encodedHandle Encoded GRIB2 handle.
/// @param context Parsed stage-local context.
/// @return Post-encode validation outcome.
PostEncodeValidationCode runPostEncodeValidationStage(const metkit::codes::CodesHandle& encodedHandle,
                                                      const PostEncodeValidationContext& context) noexcept;

}  // namespace multio::distGrib1ToGrib2::grib2grib

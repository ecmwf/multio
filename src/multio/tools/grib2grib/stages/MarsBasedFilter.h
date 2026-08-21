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
/// @brief Standalone `MarsBasedFilter` stage for the isolated `grib2grib` pipeline.

#pragma once

#include <cstdint>
#include <optional>

#include "eckit/config/LocalConfiguration.h"

#include "multio/message/MetadataMatcher.h"
#include "multio/tools/grib2grib/StageOutcomes.h"

namespace multio::distGrib1ToGrib2::grib2grib {

/// @brief Parsed context consumed by the standalone `MarsBasedFilter` stage.
///
/// The stage applies the configured statistical-product checks first. When
/// `selectors` is present, it is then evaluated against `mars + misc` converted
/// into `multio::message::Metadata`; a selector match means rejection.
struct MarsBasedFilterContext {
    std::int64_t verbosity = 0;
    bool allowExtendedSetOfOperationsForZeroLengthFsWindow = false;
    bool allowFromStartStatisticsForAnalysis = false;
    bool allowPartialStatisticsWindow = true;
    std::optional<multio::message::match::MatchReduce> selectors;
};

/// @brief Validate the raw context consumed by the `MarsBasedFilter` stage.
/// @param config Stage-local context subconfiguration.
/// @throw eckit exception If a known option value is invalid.
void validateMarsBasedFilterContext(const eckit::LocalConfiguration& config);

/// @brief Parse the stage-local `MarsBasedFilter` context once for reuse.
/// @param config Stage-local context subconfiguration.
/// @return Parsed stage-local context.
MarsBasedFilterContext parseMarsBasedFilterContext(const eckit::LocalConfiguration& config);

void freeMarsBasedFilterContext(MarsBasedFilterContext& context) noexcept;

/// @brief Run the standalone `MarsBasedFilter` stage.
/// @param mars Input MARS dictionary.
/// @param misc Input misc dictionary.
/// @param context Parsed stage-local context.
/// @return `Rejected` when a statistical-product rule or configured selector
///         rejects the message, otherwise `Accepted`.
MarsBasedFilterCode runMarsBasedFilterStage(const eckit::LocalConfiguration& mars,
                                            const eckit::LocalConfiguration& misc,
                                            const MarsBasedFilterContext& context) noexcept;

}  // namespace multio::distGrib1ToGrib2::grib2grib

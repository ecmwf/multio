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
/// @brief GRIB-based filtering stage for the new isolated `grib2grib` pipeline.
///
/// This stage performs the earliest intentional message rejections. It operates
/// on a read-only `metkit::codes::CodesHandle` and is designed to stay small and
/// readable: a fixed sequence of explicit checks, each able to reject a message
/// before later stages do any expensive work.
///
/// Example stage-local options section:
/// @code{.yaml}
/// coarse-grain-options:
///   grib1-messages-policy: try-to-handle
///   grib2-messages-policy: ignore
///   invalid-messages-policy: try-to-handle
///   discipline192-messages-policy:
///     try-to-handle: [me, 4v]
///   verbosity: 0
/// @endcode

#pragma once

#include <string>
#include <vector>

#include "eckit/config/LocalConfiguration.h"

#include "multio/tools/grib2grib/StageOutcomes.h"
#include "multio/tools/grib2grib/Utils.h"

namespace metkit::codes {
class CodesHandle;
}

namespace multio::distGrib1ToGrib2::grib2grib {

/// @brief Parsed options consumed by the coarse-grain classifier.
///
/// Options are parsed once and then reused for many messages. This keeps the
/// message-classification path free from repetitive configuration parsing.
struct GribBasedFilterContext {
    /// Policy controlling whether discipline-192 messages are rejected here.
    OptionPolicy discipline192Policy = OptionPolicy::Ignore;

    /// Named callbacks that may allow selected discipline-192 messages.
    std::vector<std::string> discipline192AllowRules;

    /// Policy controlling whether GRIB1 messages are allowed to proceed.
    OptionPolicy grib1Policy = OptionPolicy::TryToHandle;

    /// Policy controlling whether GRIB2 messages are allowed to proceed.
    OptionPolicy grib2Policy = OptionPolicy::Ignore;

    /// Policy controlling whether invalid messages are rejected here.
    OptionPolicy invalidMessagesPolicy = OptionPolicy::TryToHandle;

    /// Local coarse-classifier verbosity override.
    std::int64_t verbosity = 0;
};

/// @brief Validate the dedicated coarse-grain options subconfiguration.
/// @param options Stage-local coarse-grain options.
/// @throw eckit exception If one of the known option values is invalid.
/// @note The validation is intentionally lightweight for now. It validates the
///       known keys used by this stage and can be tightened later.
void validateGribBasedFilterContext(const eckit::LocalConfiguration& config);

/// @brief Parse the dedicated coarse-grain options subconfiguration.
/// @param options Stage-local coarse-grain options.
/// @param verbosity Fallback verbosity inherited from the caller.
/// @return Parsed reusable options structure.
/// @note Explicit `verbosity` inside the stage-local configuration overrides the
///       fallback value.
GribBasedFilterContext parseGribBasedFilterContext(const eckit::LocalConfiguration& config);

void freeGribBasedFilterContext(GribBasedFilterContext& context) noexcept;

/// @brief Run the coarse-grain classifier on one input GRIB message.
/// @param inputHandle Read-only GRIB handle built once upstream for the current
///        message.
/// @param options Parsed coarse-grain options reused across messages.
/// @return A coarse-grain classification outcome.
/// @retval GribBasedFilterCode::Accepted Message accepted for the next stage.
/// @retval GribBasedFilterCode::RejectedDiscipline192 Message intentionally
///         rejected by the discipline-192 classifier.
/// @retval GribBasedFilterCode::RejectedGrib1ByEditionPolicy Message
///         intentionally rejected by the GRIB1 edition policy.
/// @retval GribBasedFilterCode::RejectedGrib2ByEditionPolicy Message
///         intentionally rejected by the GRIB2 edition policy.
/// @retval GribBasedFilterCode::RejectedInvalidInputMessage Message
///         intentionally rejected by the invalid-message classifier.
/// @retval GribBasedFilterCode::FailedGribBasedFilter Unexpected
///         technical failure while classifying the message.
/// @note The input handle is never modified by this stage.
GribBasedFilterCode runGribBasedFilterStage(const metkit::codes::CodesHandle& inputHandle,
                                            const GribBasedFilterContext& context) noexcept;

}  // namespace multio::distGrib1ToGrib2::grib2grib

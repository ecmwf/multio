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
/// @brief Flat `ProcessOneMessage` orchestration for the isolated `grib2grib` pipeline.
///
/// This file contains the stage-by-stage message pipeline for one already
/// decoded input GRIB message. The control flow is intentionally explicit:
/// each stage is called in order, its outcome is accounted immediately, and any
/// non-accepted / non-valid outcome returns early.
///
/// Every early return also triggers a best-effort write of the original input
/// GRIB to the corresponding stage-specific debug sink, if that sink is
/// configured. Debug sink failures are intentionally ignored apart from the
/// shared trapped-error disclaimer.

#include "multio/tools/grib2grib/ProcessOneMessage.h"

#include "multio/tools/grib2grib/Sink.h"
#include "multio/tools/grib2grib/Utils.h"

namespace multio::distGrib1ToGrib2::grib2grib {

void processOneMessage(const metkit::codes::CodesHandle& inputHandle, const GlobalContext& context,
                       Grib2GribSinks& writer, FileStageOutcomes& outcomes) noexcept {
    try {
        ++outcomes.nMessages;

        const auto gribBasedFilterOutcome = runGribBasedFilterStage(inputHandle, context.gribBasedFilter);
        outcomes.gribBasedFilter.bump(gribBasedFilterOutcome);
        if (gribBasedFilterOutcome != GribBasedFilterCode::Accepted) {
            writer.debugStageInput(ProcessingStage::GribBasedFilter, inputHandle);
            return;
        }

        const auto gribToMarsResult = runGribToMarsStage(inputHandle, context.gribToMars);
        outcomes.gribToMars.bump(gribToMarsResult.outcome);
        if (gribToMarsResult.outcome != GribToMarsCode::Valid) {
            writer.debugStageInput(ProcessingStage::GribToMars, inputHandle);
            return;
        }

        const auto marsToMarsResult
            = runMarsToMarsStage(gribToMarsResult.mars, gribToMarsResult.misc, context.marsToMars);
        outcomes.marsToMars.bump(marsToMarsResult.outcome);
        if (marsToMarsResult.outcome != MarsToMarsCode::Valid) {
            writer.debugStageInput(ProcessingStage::MarsToMars, inputHandle);
            return;
        }

        const auto overridesResult = runOverridesStage(marsToMarsResult.mars, marsToMarsResult.misc, context.overrides);
        outcomes.marsOverrides.bump(overridesResult.outcome);
        if (overridesResult.outcome != MarsOverridesCode::Valid) {
            writer.debugStageInput(ProcessingStage::MarsOverrides, inputHandle);
            return;
        }

        const auto marsBasedFilterOutcome
            = runMarsBasedFilterStage(overridesResult.mars, overridesResult.misc, context.marsBasedFilter);
        outcomes.marsBasedFilter.bump(marsBasedFilterOutcome);
        if (marsBasedFilterOutcome != MarsBasedFilterCode::Accepted) {
            writer.debugStageInput(ProcessingStage::MarsBasedFilter, inputHandle);
            return;
        }

        const auto marsToGribResult
            = runMarsToGribStage(gribToMarsResult.values, overridesResult.mars, overridesResult.misc,
                                 context.marsToGrib, writer.testCaseSink());
        outcomes.marsToGrib.bump(marsToGribResult.outcome);
        if (marsToGribResult.testCaseGenerationFailed) {
            ++outcomes.nFailedMarsToGribTestCaseGenerations;
        }
        if (marsToGribResult.testCaseWriteFailed) {
            ++outcomes.nFailedMarsToGribTestCaseWrites;
        }
        if (marsToGribResult.outcome != MarsToGribCode::Valid) {
            writer.debugStageInput(ProcessingStage::MarsToGrib, inputHandle);
            return;
        }

        const auto postEncodeValidationOutcome
            = runPostEncodeValidationStage(*marsToGribResult.encoded, context.postEncodeValidation);
        outcomes.postEncodeValidation.bump(postEncodeValidationOutcome);
        if (postEncodeValidationOutcome != PostEncodeValidationCode::Valid) {
            writer.debugStageInput(ProcessingStage::PostEncodeValidation, inputHandle);
            return;
        }

        const auto grib2Fdb5Result
            = runGrib2Fdb5Stage(*marsToGribResult.encoded, context.grib2Fdb5, writer.mainDataSink());
        outcomes.grib2Fdb5.bump(grib2Fdb5Result.outcome);
        if (grib2Fdb5Result.outcome != Grib2Fdb5Code::Valid) {
            writer.debugStageInput(ProcessingStage::Grib2Fdb5, inputHandle);
        }
    }
    catch (...) {
        printTrappedErrorDisclaimer();
        ++outcomes.nGenericProcessOneMessageFailures;
    }
}

}  // namespace multio::distGrib1ToGrib2::grib2grib

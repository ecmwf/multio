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
/// Every early return can trigger a best-effort write of the original input
/// GRIB to the debug sink, relabelled with the failing stage's diagnostic
/// experiment version. Debug sink failures are intentionally ignored.

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
            if (context.gribBasedFilter.enableDebugSink) {
                writer.debugStageInput(ProcessingStage::GribBasedFilter, gribBasedFilterOutcome, inputHandle);
            }
            return;
        }

        const auto gribToMarsResult = runGribToMarsStage(inputHandle, context.gribToMars);
        outcomes.gribToMars.bump(gribToMarsResult.outcome);
        if (gribToMarsResult.outcome != GribToMarsCode::Valid) {
            if (context.gribToMars.enableDebugSink) {
                writer.debugStageInput(ProcessingStage::GribToMars, gribToMarsResult.outcome, inputHandle);
            }
            return;
        }

        const auto marsToMarsResult
            = runMarsToMarsStage(gribToMarsResult.mars, gribToMarsResult.misc, context.marsToMars);
        outcomes.marsToMars.bump(marsToMarsResult.outcome);
        if (marsToMarsResult.outcome != MarsToMarsCode::Valid) {
            if (context.marsToMars.enableDebugSink) {
                writer.debugStageInput(ProcessingStage::MarsToMars, marsToMarsResult.outcome, inputHandle);
            }
            return;
        }

        const auto overridesResult = runOverridesStage(marsToMarsResult.mars, marsToMarsResult.misc, context.overrides);
        outcomes.marsOverrides.bump(overridesResult.outcome);
        if (overridesResult.outcome != MarsOverridesCode::Valid) {
            if (context.overrides.enableDebugSink) {
                writer.debugStageInput(ProcessingStage::MarsOverrides, overridesResult.outcome, inputHandle);
            }
            return;
        }

        const auto marsBasedFilterOutcome
            = runMarsBasedFilterStage(overridesResult.mars, overridesResult.misc, context.marsBasedFilter);
        outcomes.marsBasedFilter.bump(marsBasedFilterOutcome);
        if (marsBasedFilterOutcome != MarsBasedFilterCode::Accepted) {
            if (context.marsBasedFilter.enableDebugSink) {
                writer.debugStageInput(ProcessingStage::MarsBasedFilter, marsBasedFilterOutcome, inputHandle);
            }
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
            if (context.marsToGrib.enableDebugSink) {
                writer.debugStageInput(ProcessingStage::MarsToGrib, marsToGribResult.outcome, inputHandle);
            }
            return;
        }

        const auto postEncodeValidationOutcome
            = runPostEncodeValidationStage(*marsToGribResult.encoded, context.postEncodeValidation);
        outcomes.postEncodeValidation.bump(postEncodeValidationOutcome);
        if (postEncodeValidationOutcome != PostEncodeValidationCode::Valid) {
            if (context.postEncodeValidation.enableDebugSink) {
                writer.debugStageInput(ProcessingStage::PostEncodeValidation, postEncodeValidationOutcome,
                                       inputHandle);
            }
            return;
        }

        const auto grib2Fdb5Result
            = runGrib2Fdb5Stage(*marsToGribResult.encoded, context.grib2Fdb5, writer.mainDataSink());
        outcomes.grib2Fdb5.bump(grib2Fdb5Result.outcome);
        if (context.grib2Fdb5.enableDebugSink) {
            if (grib2Fdb5Result.outcome == Grib2Fdb5Code::Valid) {
                writer.debugSuccessfulInput(inputHandle);
            }
            else {
                writer.debugStageInput(ProcessingStage::Grib2Fdb5, grib2Fdb5Result.outcome, inputHandle);
            }
        }
    }
    catch (...) {
        printTrappedErrorDisclaimer();
        ++outcomes.nGenericProcessOneMessageFailures;
    }
}

}  // namespace multio::distGrib1ToGrib2::grib2grib

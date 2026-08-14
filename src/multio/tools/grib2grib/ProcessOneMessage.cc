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
            return;
        }

        const auto gribToMarsResult = runGribToMarsStage(inputHandle, context.gribToMars);
        outcomes.gribToMars.bump(gribToMarsResult.outcome);
        if (gribToMarsResult.outcome != GribToMarsCode::Valid) {
            return;
        }

        const auto marsToMarsResult = runMarsToMarsStage(gribToMarsResult.mars, gribToMarsResult.misc, context.marsToMars);
        outcomes.marsToMars.bump(marsToMarsResult.outcome);
        if (marsToMarsResult.outcome != MarsToMarsCode::Valid) {
            return;
        }

        const auto overridesResult = runOverridesStage(marsToMarsResult.mars, marsToMarsResult.misc, context.overrides);
        outcomes.marsOverrides.bump(overridesResult.outcome);
        if (overridesResult.outcome != MarsOverridesCode::Valid) {
            return;
        }

        const auto marsBasedFilterOutcome
            = runMarsBasedFilterStage(overridesResult.mars, overridesResult.misc, context.marsBasedFilter);
        outcomes.marsBasedFilter.bump(marsBasedFilterOutcome);
        if (marsBasedFilterOutcome != MarsBasedFilterCode::Accepted) {
            return;
        }

        const auto marsToGribResult = runMarsToGribStage(gribToMarsResult.values, overridesResult.mars,
                                                         overridesResult.misc, context.marsToGrib, writer.testCaseSink());
        outcomes.marsToGrib.bump(marsToGribResult.outcome);
        if (marsToGribResult.testCaseGenerationFailed) {
            ++outcomes.nFailedMarsToGribTestCaseGenerations;
        }
        if (marsToGribResult.testCaseWriteFailed) {
            ++outcomes.nFailedMarsToGribTestCaseWrites;
        }
        if (marsToGribResult.outcome != MarsToGribCode::Valid) {
            return;
        }

        const auto postEncodeValidationOutcome
            = runPostEncodeValidationStage(*marsToGribResult.encoded, context.postEncodeValidation);
        outcomes.postEncodeValidation.bump(postEncodeValidationOutcome);
        if (postEncodeValidationOutcome != PostEncodeValidationCode::Valid) {
            return;
        }

        const auto grib2Fdb5Result = runGrib2Fdb5Stage(*marsToGribResult.encoded, context.grib2Fdb5, writer.mainDataSink());
        outcomes.grib2Fdb5.bump(grib2Fdb5Result.outcome);
    }
    catch (...) {
        printTrappedErrorDisclaimer();
        ++outcomes.nGenericProcessOneMessageFailures;
    }
}

}  // namespace multio::distGrib1ToGrib2::grib2grib

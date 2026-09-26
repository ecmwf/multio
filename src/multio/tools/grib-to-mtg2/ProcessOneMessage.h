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

#pragma once

#include "multio/tools/grib-to-mtg2/GlobalContext.h"
#include "multio/tools/grib-to-mtg2/StageOutcomes.h"

namespace metkit::codes {
class CodesHandle;
}

namespace multio::grib_to_mtg2 {

class GribToMtg2Sinks;

/// @brief Process one already-decoded input GRIB message through all standalone stages.
/// @param inputHandle Read-only GRIB handle for the current message.
/// @param context Aggregated stage contexts.
/// @param writer Rank-local sinks (main sink, debug sinks, testcase sink).
/// @param outcomes Per-file stage counters updated in place.
void processOneMessage(const metkit::codes::CodesHandle& inputHandle, const GlobalContext& context,
                       GribToMtg2Sinks& writer, FileStageOutcomes& outcomes) noexcept;

}  // namespace multio::grib_to_mtg2

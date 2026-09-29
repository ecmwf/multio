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
/// @brief Process all `WorkUnit`s owned by the current MPI rank.

#pragma once

#include <string>
#include <vector>

#include "multio/tools/grib-to-mtg2/GlobalContext.h"
#include "multio/tools/grib-to-mtg2/StageOutcomes.h"
#include "multio/tools/grib-to-mtg2/UnitOfWork.h"

namespace multio::grib_to_mtg2 {

class GribToMtg2Sinks;

std::vector<FileStageOutcomes> processRankOwnedUnitsOfWork(const std::vector<WorkUnit>& workUnits,
                                                           const GlobalContext& context, GribToMtg2Sinks& writer);

}  // namespace multio::grib_to_mtg2

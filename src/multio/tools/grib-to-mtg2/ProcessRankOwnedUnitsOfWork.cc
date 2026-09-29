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

#include "multio/tools/grib-to-mtg2/ProcessRankOwnedUnitsOfWork.h"

#include "multio/tools/grib-to-mtg2/ProcessOneUnitOfWork.h"
#include "multio/tools/grib-to-mtg2/Sink.h"

namespace multio::grib_to_mtg2 {

std::vector<FileStageOutcomes> processRankOwnedUnitsOfWork(const std::vector<WorkUnit>& workUnits,
                                                           const GlobalContext& context, GribToMtg2Sinks& writer) {
    std::vector<FileStageOutcomes> outcomes;
    outcomes.reserve(workUnits.size());

    for (const auto& workUnitState : workUnits) {
        UnitOfWork unitOfWork{workUnitState, context.reader.mode};
        outcomes.push_back(processOneUnitOfWork(unitOfWork, context, writer));
    }

    return outcomes;
}

}  // namespace multio::grib_to_mtg2

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

#include "multio/tools/grib2grib/ProcessRankOwnedUnitsOfWork.h"

#include "multio/tools/grib2grib/ProcessOneUnitOfWork.h"
#include "multio/tools/grib2grib/Sink.h"

namespace multio::distGrib1ToGrib2::grib2grib {

std::vector<FileStageOutcomes> processRankOwnedUnitsOfWork(const std::vector<WorkUnit>& workUnits,
                                                           const GlobalContext& context, Grib2GribSinks& writer) {
    std::vector<FileStageOutcomes> outcomes;
    outcomes.reserve(workUnits.size());

    for (const auto& workUnitState : workUnits) {
        UnitOfWork unitOfWork{workUnitState, context.reader.mode};
        outcomes.push_back(processOneUnitOfWork(unitOfWork, context, writer));
    }

    return outcomes;
}

}  // namespace multio::distGrib1ToGrib2::grib2grib

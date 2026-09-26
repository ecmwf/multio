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
/// @brief Process one `UnitOfWork` through the message-level pipeline.

#pragma once

#include "multio/tools/grib-to-mtg2/GlobalContext.h"
#include "multio/tools/grib-to-mtg2/StageOutcomes.h"
#include "multio/tools/grib-to-mtg2/UnitOfWork.h"

namespace multio::distGrib1ToGrib2::grib2grib {

class Grib2GribSinks;

FileStageOutcomes processOneUnitOfWork(UnitOfWork& unitOfWork, const GlobalContext& context,
                                       Grib2GribSinks& writer) noexcept;

}  // namespace multio::distGrib1ToGrib2::grib2grib

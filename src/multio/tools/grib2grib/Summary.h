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
/// @brief Root-side summary utilities for `grib2grib` outcomes.

#pragma once

#include <vector>

#include "multio/tools/grib2grib/StageOutcomes.h"

namespace multio::distGrib1ToGrib2::grib2grib {

std::vector<FileStageOutcomes> createPerFileOutcomes(const std::vector<FileStageOutcomes>& outcomesPerWorkUnit);

}  // namespace multio::distGrib1ToGrib2::grib2grib

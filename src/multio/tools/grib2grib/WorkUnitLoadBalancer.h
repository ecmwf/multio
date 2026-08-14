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
/// @brief MPI-free bucket creation utilities for `grib2grib` work units.

#pragma once

#include <cstddef>
#include <cstdint>
#include <string>
#include <vector>

#include "multio/tools/grib2grib/UnitOfWork.h"

namespace multio::distGrib1ToGrib2::grib2grib {

struct WorkBucket {
    std::vector<WorkUnit> workUnits;
    std::uint64_t totalWeightBytes = 0;
};

std::vector<WorkBucket> createBuckets(const std::vector<std::string>& filenames, std::size_t nWorkers,
                                      std::size_t averageWorkUnitsPerWorker);

std::vector<char> serializeWorkBucket(const WorkBucket& bucket);
WorkBucket deserializeWorkBucket(const std::vector<char>& payload);

}  // namespace multio::distGrib1ToGrib2::grib2grib

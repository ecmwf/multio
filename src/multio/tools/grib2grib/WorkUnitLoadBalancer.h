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

/// @brief Load-balanced assignment of work units to one synthetic or real worker.
///
/// `totalWeightBytes` stores the scheduling weight used during balancing and is
/// currently the sum of raw work-unit byte spans.
struct WorkBucket {
    std::vector<WorkUnit> workUnits;
    std::uint64_t totalWeightBytes = 0;
};

/// @brief Build balanced work buckets from a list of input files.
/// @param filenames Input files whose sizes drive work-unit generation.
/// @param nWorkers Number of target buckets to create.
/// @param averageWorkUnitsPerWorker Target average number of work units per worker.
/// @return `nWorkers` balanced buckets ordered by worker index.
/// @throw std::invalid_argument If `nWorkers` or `averageWorkUnitsPerWorker` is zero.
/// @throw std::runtime_error If any input file cannot be sized.
std::vector<WorkBucket> createBuckets(const std::vector<std::string>& filenames, std::size_t nWorkers,
                                      std::size_t averageWorkUnitsPerWorker);

/// @brief Serialize one bucket into a compact binary payload.
/// @param bucket Bucket metadata and contained work units to encode.
/// @return Binary payload suitable for MPI transfer.
std::vector<char> serializeWorkBucket(const WorkBucket& bucket);

/// @brief Deserialize one bucket from a compact binary payload.
/// @param payload Binary payload previously produced by `serializeWorkBucket(...)`.
/// @return Reconstructed bucket metadata and contained work units.
/// @throw std::runtime_error If the payload is truncated or inconsistent.
WorkBucket deserializeWorkBucket(const std::vector<char>& payload);

}  // namespace multio::distGrib1ToGrib2::grib2grib

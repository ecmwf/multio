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
/// @brief MPI wrapper helpers for `grib2grib`.

#pragma once

#include <string>
#include <vector>

#include "multio/tools/grib2grib/StageOutcomes.h"
#include "multio/tools/grib2grib/WorkUnitLoadBalancer.h"

namespace eckit::mpi {
class Comm;
}

namespace multio::distGrib1ToGrib2::grib2grib {

std::string broadcastOptionsStringFromRoot(const std::string& rootPayload, const eckit::mpi::Comm& comm);

WorkBucket distributeRankOwnedBucket(const std::vector<WorkBucket>* rootBuckets, const eckit::mpi::Comm& comm);

std::vector<FileStageOutcomes> gatherOutcomes(const std::vector<FileStageOutcomes>& localOutcomes,
                                              const eckit::mpi::Comm& comm);

}  // namespace multio::distGrib1ToGrib2::grib2grib

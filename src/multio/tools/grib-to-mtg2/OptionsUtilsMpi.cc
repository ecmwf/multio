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
/// @brief MPI-dependent options utilities for `grib-to-mtg2`.

#include "multio/tools/grib-to-mtg2/OptionsUtils.h"

#include "eckit/mpi/Comm.h"

#include "multio/tools/grib-to-mtg2/MpiUtils.h"

namespace multio::grib_to_mtg2 {

eckit::LocalConfiguration loadAndBroadcastOptionsAsConfiguration(const std::string& yamlFile,
                                                                 const eckit::mpi::Comm& comm) {
    const auto rootPayload = comm.rank() == 0 ? readOptionsFileAsString(yamlFile) : std::string{};
    const auto payload = broadcastOptionsStringFromRoot(rootPayload, comm);
    return parseOptionsYaml(payload);
}

}  // namespace multio::grib_to_mtg2

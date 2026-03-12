/*
 * (C) Copyright 2026- ECMWF and individual contributors.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#pragma once

#include <cstddef>
#include <vector>

#include "eckit/mpi/Comm.h"

namespace multio::server {

class StepMeter {
public:
    StepMeter(const eckit::mpi::Comm& serverComm, std::size_t nClients);

    void update(std::size_t clientRank, std::size_t step);

private:
    const eckit::mpi::Comm& serverComm_;
    const std::size_t nClients_;
    std::vector<std::size_t> stepCounters_;
};

}  // namespace multio::server

/*
 * (C) Copyright 2026- ECMWF and individual contributors.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "StepMeter.h"

#include <algorithm>

#include "eckit/exception/Exceptions.h"

namespace multio::server {

StepMeter::StepMeter(const eckit::mpi::Comm& serverComm, std::size_t nClients) :
    serverComm_{serverComm}, nClients_{nClients}, stepCounters_(nClients, 0) {}

void StepMeter::update(std::size_t clientRank, std::size_t step) {
    ASSERT(clientRank < nClients_);
    ASSERT(stepCounters_[clientRank] == 0 || stepCounters_[clientRank] < step);  // Step should increase!

    const auto oldMinStep = *std::min_element(stepCounters_.begin(), stepCounters_.end());
    stepCounters_[clientRank] = step;
    const auto newMinStep = *std::min_element(stepCounters_.begin(), stepCounters_.end());

    if (newMinStep > oldMinStep) {
        serverComm_.barrier();
        if (serverComm_.rank() == 0) {
            eckit::Log::info() << "UPDATE STEP METER " << oldMinStep << " -> " << newMinStep << std::endl;
            // TODO: call ecFlow Light API + write meter file
        }
    }
}

}  // namespace multio::server

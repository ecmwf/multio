/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Philipp Geier

/// @date August 20222

#pragma once

#include "eckit/config/LocalConfiguration.h"
#include "eckit/mpi/Comm.h"

#include "multio/config/ComponentConfiguration.h"

#include <optional>

namespace multio::transport::mpi {

using config::ComponentConfiguration;

// TODO: we may want to hash the payload (and the header?)
enum class CommSetupType : unsigned
{
    Unknown = 0,
    Passed = 1,
    Split = 2
};

struct CommSetupOptions {
    std::optional<CommSetupType> defaultType{CommSetupType::Passed};
    std::optional<std::string> parentCommName{};
};


eckit::mpi::Comm& getComm(const ComponentConfiguration& compConf, const std::string& name,
                          const std::optional<CommSetupOptions>& options = std::optional<CommSetupOptions>{});

inline CommSetupType parseType(const std::string& typeString);

}  // namespace multio::transport::mpi

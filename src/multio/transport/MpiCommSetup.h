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

#ifndef multio_transport_MpiCommSetup_H
#define multio_transport_MpiCommSetup_H

#include "eckit/config/LocalConfiguration.h"
#include "eckit/mpi/Parallel.h"
#include "eckit/utils/Optional.h"

#include "multio/util/ConfigurationContext.h"

namespace multio {

using util::ConfigurationContext;

namespace transport {
namespace mpi {

// TODO: we may want to hash the payload (and the header?)
enum class CommSetupType : unsigned
{
    Unknown = 0,
    Passed = 1,
    Split = 2
};

struct CommSetupOptions {
    eckit::Optional<CommSetupType> defaultType{CommSetupType::Passed};
    eckit::Optional<std::string> parentCommName{};
    eckit::Optional<std::string> alias{};
};


eckit::mpi::Comm& getComm(
    const ConfigurationContext& confCtx, const std::string& name,
    const eckit::Optional<CommSetupOptions>& options = eckit::Optional<CommSetupOptions>{});
// eckit::mpi::Comm& getOrAddComm(
//     const ConfigurationContext& confCtx, const std::string& name, int comm,
//     const eckit::Optional<CommSetupOptions>& options = eckit::Optional<CommSetupOptions>{});

inline CommSetupType parseType(const std::string& typeString);

}  // namespace mpi
}  // namespace transport
}  // namespace multio

#endif

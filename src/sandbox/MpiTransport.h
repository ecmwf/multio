/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Domokos Sarmany
/// @author Simon Smart
/// @author Tiago Quintino

/// @date Jan 2019

#ifndef multio_sandbox_MpiTransport_H
#define multio_sandbox_MpiTransport_H

#include "eckit/mpi/Comm.h"

#include "sandbox/Transport.h"

namespace multio {
namespace sandbox {

class MpiTransport final : public Transport {
public:
    MpiTransport(const eckit::Configuration& config);

private:
    Message receive() override;

    void send(const Message& message) override;

    void print(std::ostream& os) const override;

    Peer localPeer() const override;

    const eckit::mpi::Comm& comm_;
};

}  // namespace sandbox
}  // namespace multio

#endif

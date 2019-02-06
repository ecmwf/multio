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

#ifndef multio_sandbox_SimpleTransport_H
#define multio_sandbox_SimpleTransport_H

#include "eckit/container/Queue.h"

#include "sandbox/Message.h"

#include "Transport.h"

namespace multio {
namespace sandbox {

class SimpleTransport final : public Transport {
public:
    SimpleTransport(const eckit::Configuration& config);
    ~SimpleTransport() override;

private:
    eckit::Queue<Message> queue_;

private:
    Message receive() override;
    void send(const Message& message) override;

    Peer localPeer() const override;

    void print(std::ostream& os) const override;
};

}  // namespace sandbox
}  // namespace multio

#endif

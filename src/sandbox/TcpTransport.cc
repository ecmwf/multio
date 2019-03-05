/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "TcpTransport.h"

#include <iostream>

namespace multio {
namespace sandbox {

TcpTransport::TcpTransport(const eckit::Configuration& cfg) : Transport(cfg) {}

Message TcpTransport::receive() {
    Message msg{};
    return msg;
}

void TcpTransport::send(const Message& msg) {
}

Peer TcpTransport::localPeer() const {
}

void TcpTransport::print(std::ostream& os) const {
    os << "TcpTransport()";
}

static TransportBuilder<TcpTransport> TcpTransportBuilder("Tcp");

}  // namespace sandbox
}  // namespace multio

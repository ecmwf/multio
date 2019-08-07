/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "Peer.h"

#include <string>
#include <iostream>

namespace multio {
namespace server {

multio::server::Peer::operator std::string() {
    return domain_ + ":" + std::to_string(id_);
}

void Peer::print(std::ostream& out) const {
    out << "Peer(domain=" << domain_ << ",id=" << id_ << ")";
}

}  // namespace server
}  // namespace multio

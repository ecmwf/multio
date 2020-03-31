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

Peer::Peer(const std::string& group, size_t id) : group_{group}, id_{id} {}

bool Peer::operator==(const Peer& rhs) const {
    return id_ == rhs.id_ && group_ == rhs.group_;
}

bool Peer::operator!=(const Peer& rhs) const {
    return not operator==(rhs);
}

bool Peer::operator<(const Peer& rhs) const {
    return (id_ != rhs.id_) ? (id_ < rhs.id_) : (group_ < rhs.group_);
}

const std::string& Peer::group() const {
    return group_;
}
size_t Peer::id() const {
    return id_;
}

Peer::operator std::string() {
    return group_ + ":" + std::to_string(id_);
}

void Peer::print(std::ostream& out) const {
    out << "Peer(group=" << group_ << ",id=" << id_ << ")";
}


//--------------------------------------------------------------------------------------------------

}  // namespace multio

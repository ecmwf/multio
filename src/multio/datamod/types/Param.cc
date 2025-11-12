/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "Param.h"

#include "metkit/mars/Param.h"

namespace multio::datamod {

Param::Param(const Param& param) : id_{param.id_} {}

Param::Param(const std::int64_t id) : id_{id} {}

Param::Param(const std::string& str) : id_{metkit::Param(str).paramId()} {}

Param& Param::operator=(const Param& param) {
    this->id_ = param.id_;
    return *this;
}

Param& Param::operator=(std::int64_t id) {
    this->id_ = id;
    return *this;
}

Param& Param::operator=(const std::string& str) {
    this->id_ = metkit::Param(str).paramId();
    return *this;
}

bool operator==(const Param& lhs, const Param& rhs) noexcept {
    return lhs.id() == rhs.id();
}

bool operator<=(const Param& lhs, const Param& rhs) noexcept {
    return lhs.id() <= rhs.id();
}

bool operator>=(const Param& lhs, const Param& rhs) noexcept {
    return lhs.id() >= rhs.id();
}

std::ostream& operator<<(std::ostream& out, const Param& param) {
    out << param.id();
    return out;
}

}  // namespace multio::datamod


void multio::util::Print<multio::datamod::Param>::print(PrintStream& ps, const multio::datamod::Param& p) {
    ps << "Param(" << p.id() << ")";
}

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

/// @date Jan 2024

#include "multio/message/SharedPayload.h"

namespace multio::message {

//----------------------------------------------------------------------------------------------------------------------

const void* SharedPayload::data() const {
    return util::visit(
        eckit::Overloaded{
            [](const std::shared_ptr<eckit::Buffer>& sharedBuf) -> const void* { return sharedBuf->data(); },
            [](const PayloadReference& ref) -> const void* { return ref.data(); },
        },
        *this);
}

std::size_t SharedPayload::size() const {
    return util::visit(
        eckit::Overloaded{
            [](const std::shared_ptr<eckit::Buffer>& sharedBuf) -> std::size_t { return sharedBuf->size(); },
            [](const PayloadReference& ref) -> std::size_t { return ref.size(); },
        },
        *this);
}

SharedPayload::operator PayloadReference() const {
    return util::visit(
        eckit::Overloaded{
            [](const std::shared_ptr<eckit::Buffer>& sharedBuf) -> PayloadReference {
                return PayloadReference{static_cast<const char*>(sharedBuf->data()), sharedBuf->size() * sizeof(char)};
            },
            [](const PayloadReference& ref) -> PayloadReference { return ref; },
        },
        *this);
}

eckit::Stream& operator<<(eckit::Stream& strm, const SharedPayload& sp) {
    util::visit(eckit::Overloaded{[&strm](const std::shared_ptr<eckit::Buffer>& p) { strm << *p.get(); },
                                  [&strm](const PayloadReference& p) { strm.writeBlob(p.data(), p.size()); }},
                sp);
    return strm;
}

std::shared_ptr<eckit::Buffer> SharedPayload::moveOrCopy() {
    return util::visit(
        eckit::Overloaded{
            [&](const std::shared_ptr<eckit::Buffer>& p) -> std::shared_ptr<eckit::Buffer> {
                if (p.use_count() == 1) {
                    // Now move the buffer
                    return std::move(p);
                }
                else {
                    return std::make_shared<eckit::Buffer>(p->data(), p->size());
                }
            },
            [&](const PayloadReference& p) { return std::make_shared<eckit::Buffer>(p.data(), p.size()); }},
        *this);
}

void SharedPayload::acquire() {
    (*this) = this->moveOrCopy();
}

void* SharedPayload::modifyData() {
    return util::visit(eckit::Overloaded{
                           [](std::shared_ptr<eckit::Buffer>& sharedBuf) -> void* { return sharedBuf->data(); },
                           [](PayloadReference& ref) -> void* { throw; },
                       },
                       *this);
}

}  // namespace multio::message

//----------------------------------------------------------------------------------------------------------------------

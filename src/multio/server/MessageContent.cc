/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "Message.h"

namespace multio {
namespace server {

Message::Content::Content(const Header& header, const eckit::Buffer& payload) :
    header_{header},
    payload_{payload, payload.size()} {}

Message::Content::Content(const Header& header, eckit::Buffer&& payload) :
    header_{header},
    payload_{std::move(payload)} {}

Message::Header& Message::Content::header() {
    return header_;
};

eckit::Buffer& Message::Content::payload() {
    return payload_;
}

const eckit::Buffer& Message::Content::payload() const {
    return payload_;
}

size_t Message::Content::size() const {
    return payload().size();
}

}  // namespace server
}  // namespace multio

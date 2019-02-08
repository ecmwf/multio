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

#include <cstring>
#include <map>

#include "eckit/log/Bytes.h"
#include "eckit/log/Log.h"
#include "eckit/log/ResourceUsage.h"
#include "eckit/maths/Functions.h"

namespace multio {
namespace sandbox {

int Message::protocolVersion() {
    return 1;
}

std::string Message::tag2str(Message::Tag t) {
    static std::map<Tag, std::string> m = {{Tag::Empty, "Empty"},
                                           {Tag::Open, "Open"},
                                           {Tag::Close, "Close"},
                                           {Tag::Mapping, "Mapping"},
                                           {Tag::Field, "Field"}};

    ASSERT(t < Tag::ENDTAG);

    return m.find(t)->second;
}

Message::Message() :
    tag_(Message::Tag::Empty),
    version_(protocolVersion()),
    source_(),
    destination_(),
    payload_(std::make_shared<eckit::Buffer>("\0", 1)) {}

Message::Message(Message::Tag tag, Peer source, Peer destination, const eckit::Buffer& payload) :
    tag_(tag),
    version_(protocolVersion()),
    source_(source),
    destination_(destination),
    payload_(std::make_shared<eckit::Buffer>(payload, payload.size())) {}

const void* Message::payload() const {
    return payload_->data();
}

size_t Message::size() const {
    return payload_->size();
}

void Message::print(std::ostream& out) const {
    out << "Message("
        << "version=" << version_ << ",tag=" << tag2str(tag_) << ",source=" << source_
        << ",destination=" << destination_ << ")";
}


}  // namespace sandbox
}  // namespace multio

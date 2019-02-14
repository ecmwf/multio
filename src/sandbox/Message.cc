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
#include "eckit/serialisation/Stream.h"

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
    version_(protocolVersion()),
    tag_(Message::Tag::Empty),
    source_(),
    destination_(),
    payload_(std::make_shared<eckit::Buffer>("\0", 1)) {}

Message::Message(Message::Tag tag, Peer source, Peer destination, const eckit::Buffer& payload,
                 const std::string& cat, const std::string& repr) :
    version_(protocolVersion()),
    tag_(tag),
    source_(source),
    destination_(destination),
    payload_(std::make_shared<eckit::Buffer>(payload, payload.size())),
    category_(cat),
    representation_(repr) {}

const void* Message::payload() const {
    return payload_->data();
}

size_t Message::size() const {
    return payload_->size();
}

void Message::encode(eckit::Stream& strm) const {
    strm << version_;
    strm << static_cast<unsigned>(tag_);
    strm << source_.domain_;
    strm << source_.id_;
    strm << destination_.domain_;
    strm << destination_.id_;
    strm << payload_->size();
    strm << *payload_;
    strm << category_;
    strm << representation_;
}

void Message::decode(eckit::Stream& strm) {
    strm >> version_;

    unsigned t;
    strm >> t;
    tag_ = static_cast<Message::Tag>(t);

    strm >> source_.domain_;
    strm >> source_.id_;
    strm >> destination_.domain_;
    strm >> destination_.id_;

    unsigned long sz;
    strm >> sz;

    eckit::Buffer buffer(sz);
    strm >> buffer;
    payload_ = std::make_shared<eckit::Buffer>(buffer, buffer.size());

    strm >> category_;
    strm >> representation_;
}


void Message::print(std::ostream& out) const {
    out << "Field("
        << "version=" << version_ << ",tag=" << tag2str(tag_) << ",source=" << source_
        << ",destination=" << destination_ << ", category=" << category_ << ", representation="
        << representation_ << ")";
}

}  // namespace sandbox
}  // namespace multio

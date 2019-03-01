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

namespace {
eckit::Stream& operator<<(eckit::Stream& strm, const Message::Header& header) {
    strm << static_cast<unsigned>(header.tag_);

    strm << header.source_.domain_;
    strm << header.source_.id_;

    strm << header.destination_.domain_;
    strm << header.destination_.id_;

    strm << header.mapping_;
    strm << header.map_count_;

    strm << header.category_;
    strm << header.field_id_;
    strm << header.global_field_size_;

    return strm;
}

eckit::Stream& operator>>(eckit::Stream& strm, Message::Header& header) {
    unsigned t;
    strm >> t;
    header.tag_ = static_cast<Message::Tag>(t);

    strm >> header.source_.domain_;
    strm >> header.source_.id_;

    strm >> header.destination_.domain_;
    strm >> header.destination_.id_;

    strm >> header.mapping_;
    strm >> header.map_count_;

    strm >> header.category_;
    strm >> header.field_id_;
    strm >> header.global_field_size_;

    return strm;
}
}  // namespace

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

Message::Message() : Message{{Message::Tag::Empty}, 0} {}

Message::Message(Header&& header, const eckit::Buffer& payload) :
    version_{protocolVersion()},
    header_{header},
    payload_{std::make_shared<eckit::Buffer>(payload, payload.size())} {}

size_t Message::size() const {
    return payload_->size();
}

void Message::encode(eckit::Stream& strm) const {
    strm << version_;

    strm << header_;

    strm << payload_->size();
    strm << *payload_;
}

void Message::decode(eckit::Stream& strm) {
    strm >> version_;

    strm >> header_;

    unsigned long sz;
    strm >> sz;

    eckit::Buffer buffer(sz);
    strm >> buffer;
    payload_ = std::make_shared<eckit::Buffer>(buffer, buffer.size());
}

eckit::Buffer& Message::payload() {
    return *payload_;
}

void Message::print(std::ostream& out) const {
    out << "Field("
        << "version=" << version_ << ", tag=" << tag2str(header_.tag_)
        << ", source=" << header_.source_ << ", destination=" << header_.destination_
        << ", mapping=" << header_.mapping_ << ", map_count=" << header_.map_count_
        << ", category=" << header_.category_ << ", field_id=" << header_.field_id_
        << ", global_size=" << header_.global_field_size_ << ")";
}

}  // namespace sandbox
}  // namespace multio

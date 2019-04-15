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

std::string Message::tag2str(Tag t) {
    static std::map<Tag, std::string> m = {{Tag::Empty, "Empty"},
                                           {Tag::Open, "Open"},
                                           {Tag::Close, "Close"},
                                           {Tag::Mapping, "Mapping"},
                                           {Tag::Field, "Field"}};

    ASSERT(t < Tag::ENDTAG);

    return m.find(t)->second;
}

Message::Content::Content(const Header& header, const eckit::Buffer& payload) :
    header_{header},
    payload_{payload, payload.size()} {}

size_t Message::Content::size() const {
    return payload_.size();
}

Message::Message() : Message{{Message::Tag::Empty}, 0} {}

Message::Message(const Header& header, const eckit::Buffer& payload) :
    version_{protocolVersion()},
    content_{std::make_shared<Content>(header, payload)} {}

const Message::Header& Message::header() const {
    return content_->header_;
}

int Message::version() const {
    return version_;
}

Message::Tag Message::tag() const {
    return content_->header_.tag_;
}

Peer Message::destination() const {
    return content_->header_.destination_;
}

Peer Message::source() const {
    return content_->header_.source_;
}

size_t Message::size() const {
    return content_->size();
}

const std::string& Message::mapping() const {
    return content_->header_.mapping_;
}

size_t Message::map_count() const {
    return content_->header_.map_count_;
}

const std::string& Message::category() const {
    return content_->header_.category_;
}

const std::string& Message::field_id() const {
    return content_->header_.field_id_;
}

size_t Message::field_size() const {
    return content_->header_.global_field_size_;
}

eckit::Buffer& Message::payload() {
    return content_->payload_;
}

const eckit::Buffer& Message::payload() const {
    return content_->payload_;
}

void Message::encode(eckit::Stream& strm) const {
    strm << version_;

    strm << content_->header_;

    strm << content_->size();

    strm << content_->payload_;
}

void Message::decode(eckit::Stream& strm) {
    strm >> version_;

    strm >> content_->header_;

    unsigned long sz;
    strm >> sz;

    eckit::Buffer buffer(sz);
    strm >> buffer;
    content_->payload_ = std::move(buffer);
}

void Message::print(std::ostream& out) const {
    out << "Message("
        << "version=" << version() << ", tag=" << tag2str(tag()) << ", source=" << source()
        << ", destination=" << destination() << ", mapping=" << mapping()
        << ", map_count=" << map_count() << ", category=" << category()
        << ", field_id=" << field_id() << ", global_size=" << field_size() << ")";
}

}  // namespace sandbox
}  // namespace multio

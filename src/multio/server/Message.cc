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

#include "eckit/config/YAMLConfiguration.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/serialisation/Stream.h"

namespace multio {
namespace server {

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

Message::Message() : Message{Message::Header{Message::Tag::Empty, Peer{}, Peer{}}, 0} {}

Message::Message(const Header& header, const eckit::Buffer& payload) :
    version_{protocolVersion()},
    content_{std::make_shared<Content>(header, payload)} {}

const Message::Header& Message::header() const {
    return content_->header();
}

int Message::version() const {
    return version_;
}

Message::Tag Message::tag() const {
    return header().tag();
}

Peer Message::destination() const {
    return header().destination();
}

Peer Message::source() const {
    return header().source();
}

size_t Message::size() const {
    return content_->size();
}

const std::string& Message::mapping() const {
    return header().mapping();
}

size_t Message::map_count() const {
    return header().map_count();
}

const std::string& Message::category() const {
    return header().category();
}

const std::string& Message::field_id() const {
    return header().field_id();
}

const Metadata& Message::metadata() const {
    return header().metadata();
}

size_t Message::field_size() const {
    return header().global_field_size();
}

eckit::Buffer& Message::payload() {
    return content_->payload();
}

const eckit::Buffer& Message::payload() const {
    return content_->payload();
}

void Message::encode(eckit::Stream& strm) const {
    strm << version_;

    header().encode(strm);

    strm << content_->size();

    strm << content_->payload();
}

void Message::decode(eckit::Stream& strm) {
    strm >> version_;

    content_->header().decode(strm);

    unsigned long sz;
    strm >> sz;

    eckit::Buffer buffer(sz);
    strm >> buffer;
    content_->payload() = std::move(buffer);
}

void Message::print(std::ostream& out) const {
    out << "Message("
        << "version=" << version() << ", tag=" << tag2str(tag()) << ", source=" << source()
        << ", destination=" << destination() << ", mapping=" << mapping()
        << ", map_count=" << map_count() << ", category=" << category()
        << ", field_id=" << field_id() << ", global_size=" << field_size() << ")";
}

}  // namespace server
}  // namespace multio

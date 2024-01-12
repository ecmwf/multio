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

#include "eccodes.h"

#include "eckit/config/YAMLConfiguration.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/message/Message.h"
#include "eckit/serialisation/Stream.h"

#include "metkit/codes/CodesContent.h"
#include "metkit/codes/UserDataContent.h"

namespace multio {
namespace message {

int Message::protocolVersion() {
    return 1;
}

std::string Message::tag2str(Tag t) {
    static std::map<Tag, std::string> m
        = {{Tag::Empty, "Empty"}, {Tag::Open, "Open"},     {Tag::Close, "Close"},
           {Tag::Grib, "Grib"},   {Tag::Domain, "Domain"}, {Tag::Mask, "Mask"},
           {Tag::Field, "Field"}, {Tag::Flush, "Flush"},   {Tag::Notification, "Notification"}};

    ASSERT(t < Tag::ENDTAG);

    return m.find(t)->second;
}

Message::Message() : Message(Message::Header{Message::Tag::Empty, Peer{}, Peer{}}) {}

Message::Message(Header&& header, const eckit::Buffer& payload) :
    Message(std::move(header), std::make_shared<eckit::Buffer>(payload, payload.size())) {}

Message::Message(Header&& header, eckit::Buffer&& payload) :
    Message(std::move(header), std::make_shared<eckit::Buffer>(std::move(payload))) {}

// Message::Message(Header&& header, std::shared_ptr<eckit::Buffer> payload) :
//     Message(std::make_shared<Header>(std::move(header)), std::move(payload)) {}

Message::Message(Header&& header, std::shared_ptr<eckit::Buffer>&& payload) :
    version_{protocolVersion()}, header_{std::move(header)}, payload_{std::move(payload)} {}

Message::Message(Header&& header, const std::shared_ptr<eckit::Buffer>& payload) :
    version_{protocolVersion()}, header_{std::move(header)}, payload_{payload} {}

// Message::Message(std::shared_ptr<Header>&& header, std::shared_ptr<eckit::Buffer>&& payload) :
//     version_{protocolVersion()}, header_{std::move(header)}, payload_{std::move(payload)} {}

// Message::Message(std::shared_ptr<Header>&& header, const std::shared_ptr<eckit::Buffer>& payload) :
//     version_{protocolVersion()}, header_{std::move(header)}, payload_{payload} {}

const Message::Header& Message::header() const {
    return header_;
}

int Message::version() const {
    return version_;
}

util::PrecisionTag Message::precision() const {
    return header().precision();
}


Message::Tag Message::tag() const {
    return header().tag();
}

Peer Message::source() const {
    return header().source();
}

Peer Message::destination() const {
    return header().destination();
}

std::string Message::name() const {
    return header().name();
}

std::string Message::category() const {
    return header().category();
}

std::int64_t Message::globalSize() const {
    return header().globalSize();
}

std::string Message::domain() const {
    return header().domain();
}

const std::string& Message::fieldId() const {
    return header().fieldId();
}

const Metadata& Message::metadata() const& {
    return header_.metadata();
}

// Metadata&& Message::metadata() && {
//     return std::move(header_).metadata();
// }

Message Message::modifyMetadata(Metadata&& md) const {
    return Message(header_.modifyMetadata(std::move(md)), payload_);
};

const eckit::Buffer& Message::payload() const {
    return *payload_;
}

std::shared_ptr<eckit::Buffer> Message::sharedPayload() const {
    return payload_;
}

size_t Message::size() const {
    return payload_->size();
}

void Message::encode(eckit::Stream& strm) const {
    header().encode(strm);

    strm << size();

    strm << payload();
}

void Message::print(std::ostream& out) const {
    out << "Message("
        << "version=" << version() << ", tag=" << tag2str(tag()) << ", source=" << source()
        << ", destination=" << destination() << ", metadata=" << fieldId() << ", payload-size=" << payload().size()
        << ")";
}

eckit::message::Message to_eckit_message(const Message& msg) {
    if (msg.tag() == Message::Tag::Grib) {
        codes_handle* h = codes_handle_new_from_message(nullptr, msg.payload().data(), msg.size());
        return eckit::message::Message{new metkit::codes::CodesContent{h, true}};
    }

    ASSERT(msg.tag() == Message::Tag::Field);
    return eckit::message::Message{new metkit::codes::UserDataContent(msg.payload().data(), msg.size())};
}

}  // namespace message
}  // namespace multio

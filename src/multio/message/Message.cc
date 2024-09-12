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

const std::string& Message::tag2str(Tag t) {
    static std::map<Tag, std::string> m = {{Tag::Empty, "Empty"},
                                           {Tag::Open, "Open"},
                                           {Tag::Close, "Close"},
                                           {Tag::Domain, "Domain"},
                                           {Tag::Mask, "Mask"},
                                           {Tag::Field, "Field"},
                                           {Tag::Flush, "Flush"},
                                           {Tag::Notification, "Notification"},
                                           {Tag::Parametrization, "Parametrization"}};

    ASSERT(t < Tag::ENDTAG);

    auto tstr = m.find(t);
    ASSERT(tstr != m.end());

    return tstr->second;
}


Message::Tag Message::parseTag(const std::string& tagStr) {
    static std::map<std::string, Tag> m = {{"Empty", Tag::Empty},
                                           {"Open", Tag::Open},
                                           {"Close", Tag::Close},
                                           {"Domain", Tag::Domain},
                                           {"Mask", Tag::Mask},
                                           {"Field", Tag::Field},
                                           {"Flush", Tag::Flush},
                                           {"Notification", Tag::Notification},
                                           {"Parametrization", Tag::Parametrization}};
    auto tag = m.find(tagStr);
    ASSERT(tag != m.end());

    return tag->second;
}


Message::Message() : Message(Message::Header{Message::Tag::Empty, Peer{}, Peer{}}) {}

Message::Message(Header&& header) :
    version_{protocolVersion()}, header_{std::move(header)}, payload_{PayloadReference{nullptr, 0}} {}

Message::Message(Header&& header, eckit::Buffer&& payload) :
    version_{protocolVersion()},
    header_{std::move(header)},
    payload_{std::make_shared<eckit::Buffer>(std::move(payload))} {}
Message::Message(Header&& header, const eckit::Buffer& payload) :
    version_{protocolVersion()},
    header_{std::move(header)},
    payload_{std::make_shared<eckit::Buffer>(payload.data(), payload.size())} {}

Message::Message(Header&& header, SharedPayload&& payload) :
    version_{protocolVersion()}, header_{std::move(header)}, payload_{std::move(payload)} {}
Message::Message(Header&& header, const SharedPayload& payload) :
    version_{protocolVersion()}, header_{std::move(header)}, payload_{payload} {}


const Message::Header& Message::header() const {
    return header_;
}

Message::Header& Message::header() {
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

const Metadata& Message::metadata() const {
    return header_.metadata();
}

Metadata& Message::modifyMetadata() {
    return header_.modifyMetadata();
}

SharedPayload& Message::payload() {
    return payload_;
}

const SharedPayload& Message::payload() const {
    return payload_;
}

void Message::acquire() {
    acquireMetadata();
    acquirePayload();
}

void Message::acquirePayload() {
    payload_.acquire();
}

void Message::acquireMetadata() {
    header_.acquireMetadata();
}

size_t Message::size() const {
    return payload_.size();
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


LogMessage Message::logMessage() const {
    return LogMessage{version_, header_.logHeader(), payload_.size()};
}


const std::string& LogMessage::fieldId() const {
    if (!header_.fieldId_) {
        if (std::shared_ptr<Metadata> md = header_.metadata_.lock()) {
            header_.fieldId_ = md->toString();
        }
        else {
            header_.fieldId_ = "<context lost>";
        }
    }
    return *header_.fieldId_;
}

void LogMessage::print(std::ostream& out) const {
    out << "Message("
        << "version=" << version_ << ", tag=" << Message::tag2str(header_.tag_) << ", source=" << header_.source_
        << ", destination=" << header_.destination_ << ", metadata=" << fieldId() << ", payload-size=" << payload_size_
        << ")";
}

eckit::message::Message to_eckit_message(const Message& msg) {
    ASSERT(msg.tag() == Message::Tag::Field);
    return eckit::message::Message{new metkit::codes::UserDataContent(msg.payload().data(), msg.size())};
}

}  // namespace message
}  // namespace multio

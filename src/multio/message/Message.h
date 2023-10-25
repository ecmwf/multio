/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Domokos Sarmany
/// @author Simon Smart
/// @author Tiago Quintino

/// @date Jan 2019

#pragma once

#include "eckit/io/Buffer.h"
#include "multio/util/PrecisionTag.h"

#include "multio/message/Metadata.h"
#include "multio/message/Peer.h"

#include <memory>
#include <optional>
#include <string>


namespace eckit {
class Stream;

namespace message {
class Message;
}
}  // namespace eckit

namespace multio::message {

// TODO: we may want to hash the payload (and the header?)

class Message {
public:  // types
    enum class Tag : unsigned
    {
        Empty = 0,
        Open,
        Close,
        Grib,
        Domain,
        Mask,
        Field,
        Flush,
        Notification,
        ENDTAG
    };

    class Header {
    public:
        Header(Tag tag, Peer src, Peer dst, std::string&& fieldId);
        Header(Tag tag, Peer src, Peer dst, Metadata&& md = message::Metadata{});

        Tag tag() const;

        Peer source() const;
        Peer destination() const;

        std::string name() const;

        std::string category() const;

        std::int64_t globalSize() const;

        std::string domain() const;

        util::PrecisionTag precision() const;

        const std::string& fieldId() const;

        void encode(eckit::Stream& strm) const;

        // Metadata&& metadata() &&;
        const Metadata& metadata() const&;

        Header modifyMetadata(Metadata&& md) const;

    private:
        Tag tag_;

        Peer source_;
        Peer destination_;

        Metadata metadata_;
        // encode fieldId_ lazily
        mutable std::optional<std::string> fieldId_;  // Make that a hash?
    };

    // class Content {
    // public:
    //     Content(Header&& header, const eckit::Buffer& payload = eckit::Buffer(0));
    //     Content(Header&& header, eckit::Buffer&& payload);

    //     size_t size() const;

    //     const Header& header();

    //     eckit::Buffer& payload();
    //     const eckit::Buffer& payload() const;

    // private:
    //     const Header header_;
    //     eckit::Buffer payload_;
    // };

public:  // methods
    static int protocolVersion();
    static std::string tag2str(Tag t);

    Message();
    Message(Header&& header, const eckit::Buffer& payload = eckit::Buffer{0});
    Message(Header&& header, eckit::Buffer&& payload);
    Message(Header&& header, std::shared_ptr<eckit::Buffer> payload);
    Message(std::shared_ptr<Header>&& header, std::shared_ptr<eckit::Buffer>&& payload);
    Message(std::shared_ptr<Header>&& header, const std::shared_ptr<eckit::Buffer>& payload);
    // Message(std::shared_ptr<Header> header, std::shared_ptr<eckit::Buffer> payload);

public:
    const Header& header() const;

    int version() const;
    Tag tag() const;

    Peer source() const;
    Peer destination() const;

    std::string name() const;

    std::string category() const;

    util::PrecisionTag precision() const;

    std::int64_t globalSize() const;

    std::string domain() const;

    const std::string& fieldId() const;

    // Metadata&& metadata() &&;

    const Metadata& metadata() const&;

    Message modifyMetadata(Metadata&& md) const;

    const eckit::Buffer& payload() const;

    std::shared_ptr<eckit::Buffer> sharedPayload() const;

    size_t size() const;

    void encode(eckit::Stream& strm) const;

private:  // methods
    void print(std::ostream& out) const;

    friend std::ostream& operator<<(std::ostream& s, const Message& x) {
        x.print(s);
        return s;
    }

private:  // members
    int version_;

    std::shared_ptr<Header> header_;
    std::shared_ptr<eckit::Buffer> payload_;
};

eckit::message::Message to_eckit_message(const Message& msg);


// Utility to convert message
template <typename From, typename To>
message::Message convert_precision(message::Message&& msg) {
    const size_t N = msg.payload().size() / sizeof(From);
    eckit::Buffer buffer(N * sizeof(To));

    auto md = msg.metadata();
    md.set<std::int64_t>("globalSize", buffer.size());
    md.set("precision", std::is_same_v<To, double> ? "double" : std::is_same_v<To, float> ? "single" : NOTIMP);

    const auto* a = reinterpret_cast<const From*>(msg.payload().data());
    auto* b = reinterpret_cast<To*>(buffer.data());
    for (size_t i = 0; i < N; ++i) {
        *(b++) = static_cast<To>(*(a++));
    }

    return {message::Message::Header{msg.tag(), msg.source(), msg.destination(), std::move(md)}, std::move(buffer)};
}


}  // namespace multio::message

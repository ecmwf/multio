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
/// @author Philipp Geier

/// @date Jan 2019

#pragma once

#include "eckit/io/Buffer.h"
#include "multio/util/PrecisionTag.h"

#include "multio/message/Metadata.h"
#include "multio/message/Peer.h"
#include "multio/message/SharedMetadata.h"
#include "multio/message/SharedPayload.h"
#include "multio/message/Glossary.h"

#include <memory>
#include <optional>
#include <string>

using multio::message::glossary;

namespace eckit {
class Stream;

namespace message {
class Message;
}
}  // namespace eckit

namespace multio::message {

// TODO: we may want to hash the payload (and the header?)
struct LogMessage;

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
        Parametrization,
        ENDTAG
    };


    struct LogHeader {
        Tag tag_;

        Peer source_;
        Peer destination_;

        std::weak_ptr<Metadata> metadata_;
        mutable std::optional<std::string> fieldId_;
    };

    class Header {
    public:
        Header(const Header&) = default;
        Header(Header&&) = default;

        Header& operator=(const Header&) = default;
        Header& operator=(Header&&) = default;

        Header(Tag tag, Peer src, Peer dst, std::string&& fieldId);
        Header(Tag tag, Peer src, Peer dst, Metadata&& md);
        // TODO optimize default construction - avoid make shared to optimize cases of default initialization +
        // assignment
        Header(Tag tag, Peer src, Peer dst, SharedMetadata md = SharedMetadata{});

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

        const Metadata& metadata() const;

        Metadata& modifyMetadata();


        // Copy or acquire metadata object if only owned by this object
        SharedMetadata moveOrCopyMetadata() const;

        // Copy or acquire metadata object if only owned by this object
        void acquireMetadata();

        LogHeader logHeader() const;

    private:
        Tag tag_;

        Peer source_;
        Peer destination_;

        SharedMetadata metadata_;
        // encode fieldId_ lazily
        mutable std::optional<std::string> fieldId_;  // Make that a hash?
    };


public:  // methods
    static int protocolVersion();
    static std::string tag2str(Tag t);

    Message(const Message&) = default;
    Message(Message&&) = default;

    Message& operator=(const Message&) = default;
    Message& operator=(Message&&) = default;

    Message();
    Message(Header&& header);
    Message(Header&& header, const eckit::Buffer& payload);
    Message(Header&& header, eckit::Buffer&& payload);
    Message(Header&& header, SharedPayload&& payload);
    Message(Header&& header, const SharedPayload& payload);

    LogMessage logMessage() const;

public:
    const Header& header() const;
    Header& header();

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

    const Metadata& metadata() const;
    Metadata& modifyMetadata();

    SharedPayload& payload();
    const SharedPayload& payload() const;

    // Try to acquire metadata & payload or copy otherwise
    void acquire();

    void acquireMetadata();
    void acquirePayload();

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

    Header header_;
    SharedPayload payload_;
};


struct LogMessage {
    int version_;

    Message::LogHeader header_;
    std::size_t payload_size_;

    const std::string& fieldId() const;
    void print(std::ostream& out) const;

    friend std::ostream& operator<<(std::ostream& s, const LogMessage& x) {
        x.print(s);
        return s;
    }
};


eckit::message::Message to_eckit_message(const Message& msg);


// Utility to convert message
template <typename From, typename To>
message::Message convert_precision(message::Message&& msg) {
    const size_t N = msg.payload().size() / sizeof(From);
    eckit::Buffer buffer(N * sizeof(To));

    auto md = msg.metadata();
    md.set<std::int64_t>(glossary().globalSize, buffer.size());
    md.set(glossary().precision, std::is_same_v<To, double> ? "double" : std::is_same_v<To, float> ? "single" : NOTIMP);

    const auto* a = reinterpret_cast<const From*>(msg.payload().data());
    auto* b = reinterpret_cast<To*>(buffer.data());
    for (size_t i = 0; i < N; ++i) {
        *(b++) = static_cast<To>(*(a++));
    }

    return {message::Message::Header{msg.tag(), msg.source(), msg.destination(), std::move(md)}, std::move(buffer)};
}


}  // namespace multio::message

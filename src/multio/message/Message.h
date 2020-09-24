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

#ifndef multio_server_Message_H
#define multio_server_Message_H

#include <memory>
#include <string>

#include "eckit/io/Buffer.h"

#include "multio/message/Metadata.h"
#include "multio/message/Peer.h"

namespace eckit {
class Stream;

namespace message {
class Message;
}
}  // namespace eckit

namespace multio {
namespace message {

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
        Field,
        Statistics,
        StepComplete,
        StepNotification,
        ENDTAG
    };

    class Header {
    public:
        Header(Tag tag, Peer src, Peer dst, const std::string& nm = "", const std::string& cat = "",
               size_t cnt = 0, size_t fsz = 0, const std::string& dom = "", const std::string& fid = "");

        Tag tag() const;

        Peer source() const;
        Peer destination() const;

        const std::string& name() const;

        const std::string& category() const;

        size_t domainCount() const;

        size_t globalSize() const ;

        const std::string& domain() const;

        const std::string& fieldId() const;

        void encode(eckit::Stream& strm) const;
        void decode(eckit::Stream& strm);

        const Metadata& metadata() const;

    private:
        Tag tag_;

        Peer source_;
        Peer destination_;

        std::string name_;

        std::string category_;

        size_t domainCount_;

        size_t globalSize_;

        // For fields only
        std::string domain_;

        std::string fieldId_; // Make that a hash?
        Metadata metadata_;
        void setMetadata();
    };

    class Content {
    public:
        Content(Header&& header, const eckit::Buffer& payload = eckit::Buffer(0));
        Content(Header&& header, eckit::Buffer&& payload);

        size_t size() const;

        Header& header();

        eckit::Buffer& payload();
        const eckit::Buffer& payload() const;

    private:
        Header header_;
        eckit::Buffer payload_;
    };

public:  // methods
    static int protocolVersion();
    static std::string tag2str(Tag t);

    Message();
    Message(Header&& header, const eckit::Buffer& payload = eckit::Buffer(0));
    Message(Header&& header, eckit::Buffer&& payload);

    const Header& header() const;

    int version() const;
    Tag tag() const;

    Peer source() const;
    Peer destination() const;

    const std::string& name() const;

    const std::string& category() const;

    size_t domainCount() const;

    size_t globalSize() const ;

    const std::string& domain() const;

    const std::string& fieldId() const;
    const Metadata& metadata() const;

    eckit::Buffer& payload();
    const eckit::Buffer& payload() const;

    size_t size() const;

    void encode(eckit::Stream& strm) const;
    void decode(eckit::Stream& strm);

private:  // methods
    void print(std::ostream& out) const;

    friend std::ostream& operator<<(std::ostream& s, const Message& x) {
        x.print(s);
        return s;
    }

private:  // members
    int version_;

    std::shared_ptr<Content> content_;

};

eckit::message::Message to_eckit_message(const Message& msg);

}  // namespace message
}  // namespace multio

#endif

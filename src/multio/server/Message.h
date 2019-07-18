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

#include "eckit/config/LocalConfiguration.h"
#include "eckit/io/Buffer.h"

#include "multio/server/Peer.h"

namespace eckit {
class Stream;
}  // namespace eckit

namespace multio {
namespace server {

using Metadata = eckit::LocalConfiguration;

// TODO: we may want to hash the payload (and the header?)

class Message {
public:  // types
    enum class Tag : unsigned
    {
        Empty = 0,
        Open,
        Close,
        GribTemplate,
        Mapping,
        Field,
        StepComplete,
        StepNotification,
        ENDTAG
    };

    class Header {
    public:
        Header(Tag tag, Peer src, Peer dst, const std::string& map = "", size_t cnt = 0,
               const std::string& cat = "", const std::string& fid = "", size_t fsz = 0);

        Tag tag() const;

        Peer source() const;
        Peer destination() const;

        const std::string& mapping() const; // For fields and mappings
        size_t map_count() const;           // For mappings only

        // For fields only
        const std::string& category() const;
        const std::string& field_id() const;
        size_t global_field_size() const ;

        void encode(eckit::Stream& strm) const;
        void decode(eckit::Stream& strm);

        const Metadata& metadata() const;

    private:
        Tag tag_;

        Peer source_;
        Peer destination_;

        std::string mapping_;  // For fields and mappings
        size_t map_count_;     // For mappings only

        // For fields only
        std::string category_;
        std::string field_id_;
        size_t global_field_size_;

        Metadata metadata_;
        void setMetadata();
    };

    class Content {
    public:
        Content(const Header& header, const eckit::Buffer& payload = 0);
        Content(const Header& header, eckit::Buffer&& payload);

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
    Message(const Header& header, const eckit::Buffer& payload = 0);
    Message(const Header& header, eckit::Buffer&& payload);

    const Header& header() const;

    int version() const;
    Tag tag() const;

    Peer destination() const;
    Peer source() const;

    size_t size() const;

    const std::string& mapping() const;
    size_t map_count() const;

    const std::string& category() const;
    const std::string& field_id() const;
    const Metadata& metadata() const;
    size_t field_size() const;

    eckit::Buffer& payload();
    const eckit::Buffer& payload() const;

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

}  // namespace server
}  // namespace multio

#endif

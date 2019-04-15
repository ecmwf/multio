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

#ifndef multio_sandbox_Message_H
#define multio_sandbox_Message_H

#include <memory>
#include <string>

#include "eckit/io/Buffer.h"

#include "sandbox/Peer.h"

namespace eckit {
class Stream;
}

namespace multio {
namespace sandbox {

// TODO: we may want to hash the payload (and the header?)

class Message {
public:  // types
    enum class Tag : unsigned
    {
        Empty = 0,
        Open,
        Close,
        Mapping,
        Field,
        StepComplete,
        ENDTAG
    };

    struct Header {
        Tag tag_;

        Peer source_;
        Peer destination_;

        std::string mapping_;  // For fields and mappings
        size_t map_count_;     // For mappings only

        // For fields only
        std::string category_;
        std::string field_id_;  // Could also be the hash of MARS metadata
        size_t global_field_size_;
    };

    struct Content {
        Header header_;
        eckit::Buffer payload_;

        Content(const Header& header, const eckit::Buffer& payload = 0);
        size_t size() const;
    };

public:  // methods
    static int protocolVersion();
    static std::string tag2str(Tag t);

    Message();
    Message(const Header& header, const eckit::Buffer& payload = 0);

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

}  // namespace sandbox
}  // namespace multio

#endif

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
#include <thread>
#include <vector>

#include "eckit/io/Buffer.h"

#include "sandbox/Peer.h"

namespace eckit {
class Stream;
}

namespace multio {
namespace sandbox {

class Message {
public:  // types
    enum class Tag : unsigned
    {
        Empty = 0,
        Open,
        Close,
        Mapping,
        Field,
        ENDTAG
    };

public:  // methods
    static int protocolVersion();
    static std::string tag2str(Tag);

    Message();
    Message(Tag tag, Peer source, Peer destination, const eckit::Buffer& payload,
            const std::string& map = "", size_t cnt = -1, const std::string& cat = "");

    int version() const { return version_; }

    Tag tag() const { return tag_; }

    Peer destination() const { return destination_; }
    Peer source() const { return source_; }

    size_t size() const;
    const void* payload() const;

    const std::string& mapping() const { return mapping_;};
    int map_count() const { return map_count_; }

    const std::string& category() const { return category_; };

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

    Tag tag_;

    Peer source_;
    Peer destination_;

    std::shared_ptr<eckit::Buffer> payload_;

    // For fields and mappings
    std::string mapping_;

    // For mappings only
    long map_count_;

    // For fields only
    std::string category_;
};

}  // namespace sandbox
}  // namespace multio

#endif

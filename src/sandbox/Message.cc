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

#include "sandbox/print_buffer.h"

namespace multio {
namespace sandbox {

int Message::protocolVersion() {
    return 1;
}

std::string Message::tag2str(Message::Tag t)
{
    static std::map<unsigned, const char*> m = {
        { unsigned(Tag::Open), "OPEN"}
    };

    return m.find(unsigned(t))->second;
}

Message::Message(Message::Tag tag, Peer from, Peer to, const eckit::Buffer& payload):
    tag_(tag),
    version_(protocolVersion()),
    from_(from),
    to_(to),
    payload_(new eckit::Buffer(payload, payload.size()))
{
}

const void* Message::payload() const {
    return payload_->data();
}

size_t Message::size() const {
    return payload_->size();
}

void Message::print(std::ostream& out) const {
    out << "Message("
        << "version=" << version_
        << ",tag=" << tag2str(tag_)
        << ",from=" << from_
        << ",to=" << to_
        << ")";
}



}  // namespace sandbox
}  // namespace multio

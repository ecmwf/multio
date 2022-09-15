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

#include "eckit/config/YAMLConfiguration.h"
#include "eckit/serialisation/Stream.h"

namespace multio {
namespace message {

Message::Header::Header(Tag tag, Peer src, Peer dst, std::string&& fieldId) :
    tag_{tag},
        source_{std::move(src)},
        destination_{std::move(dst)},
        metadata_{message::to_metadata(fieldId)},
        fieldId_{std::move(fieldId)} {}

// TODO: Discussion on ownership of Metadata & LocalConfiguration. 
// LocalConfiguration does not support moving, instead shallow copies are created (reference counting).
// Yet the ownership is not always clear.
// Possibly also add a constructor for copying Metadata
Message::Header::Header(Tag tag, Peer src, Peer dst, Metadata&& md) :
    tag_{tag},
    source_{std::move(src)},
    destination_{std::move(dst)},
    metadata_{std::move(md)},
    fieldId_{message::to_string(metadata_)} {}

Message::Tag Message::Header::tag() const {
    return tag_;
}

Peer Message::Header::source() const {
    return source_;
}

Peer Message::Header::destination() const {
    return destination_;
}

const Metadata& Message::Header::metadata() const {
    return metadata_;
}

std::string Message::Header::name() const {
    return metadata_.getString("name");
}

std::string Message::Header::category() const {
    return metadata_.getString("category");
}

size_t Message::Header::domainCount() const {
    return metadata_.getUnsigned("domainCount");
}

long Message::Header::globalSize() const {
    return metadata_.getLong("globalSize");
}

std::string Message::Header::domain() const {
    return metadata_.getString("domain");
}

const std::string& Message::Header::fieldId() const {
    return fieldId_;
}

void Message::Header::encode(eckit::Stream& strm) const {
    strm << static_cast<unsigned>(tag_);

    strm << source_.group();
    strm << source_.id();

    strm << destination_.group();
    strm << destination_.id();

    strm << fieldId_;
}

}  // namespace message
}  // namespace multio

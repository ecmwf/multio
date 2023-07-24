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

namespace multio::message {

Message::Header::Header(Tag tag, Peer src, Peer dst, std::string&& fieldId) :
    tag_{tag},
    source_{std::move(src)},
    destination_{std::move(dst)},
    metadata_{message::toMetadata(fieldId)},
    fieldId_{std::move(fieldId)} {

    // TODO: Maybe it is useful to check here if in the metadata we have the fields:
    //
    // name, category, globalSize, domain, precision?
    //
    // They are all required
}

Message::Header::Header(Tag tag, Peer src, Peer dst, Metadata&& md) :
    tag_{tag}, source_{std::move(src)}, destination_{std::move(dst)}, metadata_{std::move(md)}, fieldId_{} {}

Message::Tag Message::Header::tag() const {
    return tag_;
}

Peer Message::Header::source() const {
    return source_;
}

Peer Message::Header::destination() const {
    return destination_;
}

const Metadata& Message::Header::metadata() const& {
    return metadata_;
}

// Metadata&& Message::Header::metadata() && {
//     return std::move(metadata_);
// }

std::string Message::Header::name() const {
    if (!metadata_.has("name")) {
        throw MetadataMissingKeyException("name", Here());
    }
    return metadata_.get<std::string>("name");
}

std::string Message::Header::category() const {
    if (!metadata_.has("category")) {
        throw MetadataMissingKeyException("category", Here());
    }
    return metadata_.get<std::string>("category");
}

std::int64_t Message::Header::globalSize() const {
    if (!metadata_.has("globalSize")) {
        throw MetadataMissingKeyException("globalSize", Here());
    }
    return metadata_.get<std::int64_t>("globalSize");
}

std::string Message::Header::domain() const {
    if (!metadata_.has("domain")) {
        throw MetadataMissingKeyException("domain", Here());
    }
    return metadata_.get<std::string>("domain");
}

util::PrecisionTag Message::Header::precision() const {
    if (!metadata_.has("precision")) {
        throw MetadataMissingKeyException("precision", Here());
    }
    return util::decodePrecisionTag(metadata_.get<std::string>("precision"));
}

const std::string& Message::Header::fieldId() const {
    if (!fieldId_) {
        fieldId_ = message::toString(metadata_);
    }
    return *fieldId_;
}

void Message::Header::encode(eckit::Stream& strm) const {
    strm << static_cast<unsigned>(tag_);

    strm << source_.group();
    strm << source_.id();

    strm << destination_.group();
    strm << destination_.id();

    strm << fieldId();
}

Message::Header Message::Header::modifyMetadata(Metadata&& md) const {
    return Header{tag_, std::move(source_), std::move(destination_), std::move(md)};
};


}  // namespace multio::message

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

#include "Glossary.h"
#include "eckit/config/YAMLConfiguration.h"
#include "eckit/serialisation/Stream.h"

namespace multio::message {

Message::Header::Header(Tag tag, Peer src, Peer dst, std::string&& fieldId) :
    tag_{tag},
    source_{std::move(src)},
    destination_{std::move(dst)},
    metadata_{std::make_shared<Metadata>(message::metadataFromYAML(fieldId))},
    fieldId_{std::move(fieldId)} {

    // TODO: Maybe it is useful to check here if in the metadata we have the fields:
    //
    // name, category, globalSize, domain, precision?
    //
    // They are all required
}

Message::Header::Header(Tag tag, Peer src, Peer dst, Metadata&& md) :
    tag_{tag},
    source_{std::move(src)},
    destination_{std::move(dst)},
    metadata_{std::make_shared<Metadata>(std::move(md))},
    fieldId_{} {}

Message::Header::Header(Tag tag, Peer src, Peer dst, SharedMetadata md) :
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

const Metadata& Message::Header::metadata() const {
    return metadata_.read();
}
Metadata& Message::Header::modifyMetadata() {
    return metadata_.modify();;
}

// Metadata&& Message::Header::metadata() && {
//     return std::move(metadata_);
// }

// Copy or acquire metadata object if only owned by this object
SharedMetadata Message::Header::moveOrCopyMetadata() const {
    return metadata_.moveOrCopy();
}

// Copy or acquire metadata object if only owned by this object
void Message::Header::acquireMetadata() {
    metadata_.acquire();
}


std::string Message::Header::name() const {
    if (auto optVal = metadata_.read().getOpt<std::string>(glossary().name); optVal) {
        return *optVal;
    }
    throw MetadataMissingKeyException("name", Here());
}

std::string Message::Header::category() const {
    if (auto optVal = metadata_.read().getOpt<std::string>(glossary().category); optVal) {
        return *optVal;
    }
    throw MetadataMissingKeyException("category", Here());
}

std::int64_t Message::Header::globalSize() const {
    if (auto optVal = metadata_.read().getOpt<std::int64_t>(glossary().globalSize); optVal) {
        return *optVal;
    }
    throw MetadataMissingKeyException("globalSize", Here());
}

std::string Message::Header::domain() const {
    if (auto optVal = metadata_.read().getOpt<std::string>(glossary().domain); optVal) {
        return *optVal;
    }
    throw MetadataMissingKeyException("domain", Here());
}

util::PrecisionTag Message::Header::precision() const {
    if (auto optVal = metadata_.read().getOpt<std::string>(glossary().precision); optVal) {
        return util::decodePrecisionTag(*optVal);
    }
    throw MetadataMissingKeyException("precision", Here());
}

const std::string& Message::Header::fieldId() const {
    if (!fieldId_) {
        fieldId_ = metadata_.read().toString();
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
}

Message::LogHeader Message::Header::logHeader() const {
    return Message::LogHeader{tag_, source_, destination_, metadata_.weakRef(), fieldId_};
}

}  // namespace multio::message

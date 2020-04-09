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
namespace server {

Message::Header::Header(Tag tag, Peer src, Peer dst, const std::string& nm, const std::string& cat,
                        size_t cnt, size_t fsz, const std::string& dom, const std::string& fid) :
    tag_{tag},
    source_{std::move(src)},
    destination_{std::move(dst)},
    name_{nm},
    category_{cat},
    domainCount_{cnt},
    globalSize_{fsz},
    domain_{dom},
    fieldId_{fid} {
    setMetadata();
}

Message::Tag Message::Header::tag() const {
    return tag_;
}

Peer Message::Header::source() const {
    return source_;
}

Peer Message::Header::destination() const {
    return destination_;
}

const std::string& Message::Header::name() const {
    return name_;
}

const std::string& Message::Header::category() const {
    return category_;
}

size_t Message::Header::domainCount() const {
    return domainCount_;
}

size_t Message::Header::globalSize() const {
    return globalSize_;
}

const std::string& Message::Header::domain() const {
    return domain_;
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

    strm << name_;
    strm << category_;

    strm << domainCount_;
    strm << globalSize_;

    strm << domain_;
    strm << fieldId_;
}

void Message::Header::decode(eckit::Stream& strm) {
    strm >> name_;
    strm >> category_;

    strm >> domainCount_;
    strm >> globalSize_;

    strm >> domain_;
    strm >> fieldId_;

    setMetadata();
}

const Metadata& Message::Header::metadata() const {
    return metadata_;
}

void Message::Header::setMetadata() {
    const eckit::Configuration& config = eckit::YAMLConfiguration{fieldId_};
    metadata_ = Metadata{config};
}

}  // namespace server
}  // namespace multio

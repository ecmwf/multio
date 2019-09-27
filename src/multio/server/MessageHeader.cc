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

Message::Header::Header(Tag tag, Peer src, Peer dst, const std::string& map, size_t cnt,
                        const std::string& cat, const std::string& fid, size_t fsz) :
    tag_{tag},
    source_{std::move(src)},
    destination_{std::move(dst)},
    mapping_{map},
    map_count_{cnt},
    category_{cat},
    field_id_{fid},
    global_field_size_{fsz} {
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

const std::string& Message::Header::mapping() const {
    return mapping_;
}

size_t Message::Header::map_count() const {
    return map_count_;
}

const std::string& Message::Header::category() const {
    return category_;
}

const std::string& Message::Header::field_id() const {
    return field_id_;
}

size_t Message::Header::global_field_size() const {
    return global_field_size_;
}

void Message::Header::encode(eckit::Stream& strm) const {
    strm << static_cast<unsigned>(tag_);

    strm << source_.domain();
    strm << source_.id();

    strm << destination_.domain();
    strm << destination_.id();

    strm << mapping_;
    strm << map_count_;

    strm << category_;
    strm << field_id_;
    strm << global_field_size_;
}

void Message::Header::decode(eckit::Stream& strm) {
    strm >> mapping_;
    strm >> map_count_;

    strm >> category_;
    strm >> field_id_;
    strm >> global_field_size_;

    setMetadata();
}

const Metadata& Message::Header::metadata() const {
    return metadata_;
}

void Message::Header::setMetadata() {
    const eckit::Configuration& config = eckit::YAMLConfiguration{field_id_};
    metadata_ = Metadata{config};
}

}  // namespace server
}  // namespace multio

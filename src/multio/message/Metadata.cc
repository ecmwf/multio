/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "Metadata.h"

namespace multio::message {

std::string toString(const Metadata& metadata) {
    return details::toString(metadata);
}

Metadata toMetadata(const eckit::Value& value) {
    return details::toMetadata<MetadataTraits>(value);
}

Metadata toMetadata(const std::string& fieldId) {
    return details::toMetadata<MetadataTraits>(fieldId);
}

std::optional<MetadataValue> toMetadataValue(const eckit::Value& v) {
    return details::toMetadataMaybe<MetadataTraits>(v);
}

}  // namespace multio::message

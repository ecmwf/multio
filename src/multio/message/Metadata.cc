/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Philipp Geier

/// @date Sept 2023

#include "multio/message/Metadata.h"

#include "eckit/parser/YAMLParser.h"

#include <sstream>


namespace multio::message {

//----------------------------------------------------------------------------------------------------------------------

// Constructor for unordered_map
// Metadata::Metadata() : values_{512} {}
// Metadata::Metadata(std::initializer_list<std::pair<const KeyType, MetadataValue>> li) : values_{std::move(li), 512}
// {}

// Construtore for map
Metadata::Metadata() : values_{} {}
Metadata::Metadata(std::initializer_list<std::pair<const KeyType, MetadataValue>> li) : values_{std::move(li)} {}

Metadata::Metadata(MapType&& values) : values_{std::move(values)} {}


MetadataValue&& Metadata::get(const KeyType& k) && {
    return std::move(referenceGetter(*this, k).get());
}

MetadataValue& Metadata::get(const KeyType& k) & {
    return referenceGetter(*this, k).get();
}

const MetadataValue& Metadata::get(const KeyType& k) const& {
    return referenceGetter(*this, k).get();
}


MetadataValue& Metadata::operator[](const KeyType& key) {
    return values_[key];
}
MetadataValue& Metadata::operator[](KeyType&& key) {
    return values_[std::move(key)];
}


bool Metadata::empty() const noexcept {
    return values_.empty();
}

std::size_t Metadata::size() const noexcept {
    return values_.size();
}

void Metadata::clear() noexcept {
    values_.clear();
}

/**
 * Extracts all values from other metadata whose keys are not contained yet in this metadata.
 * Explicitly always modifies both metadata.
 */
void Metadata::merge(This& other) {
    values_.merge(other.values_);
}
void Metadata::merge(This&& other) {
    values_.merge(std::move(other.values_));
}


void Metadata::updateOverwrite(const Metadata& other) {
    for (const auto& kv : other.values_) {
        values_.insert_or_assign(kv.first, kv.second);
    }
}


void Metadata::updateOverwrite(Metadata&& other) {
    auto tmp = std::move(values_);
    values_ = std::move(other.values_);
    values_.merge(tmp);
}

void Metadata::updateNoOverwrite(const Metadata& other) {
    values_.insert(other.values_.begin(), other.values_.end());
}


void Metadata::updateNoOverwrite(Metadata&& other) {
    values_.merge(other.values_);
}


std::string Metadata::toString() const {
    std::stringstream ss;
    eckit::JSON json(ss);
    json << *this;
    return ss.str();
}

void Metadata::json(eckit::JSON& j) const {
    j << *this;
}

//----------------------------------------------------------------------------------------------------------------------


eckit::JSON& operator<<(eckit::JSON& json, const Metadata& metadata) {
    json.startObject();
    for (const auto& kv : metadata) {
        json << kv.first;
        json << kv.second;
    }
    json.endObject();
    return json;
}


std::ostream& operator<<(std::ostream& os, const Metadata& metadata) {
    eckit::JSON json(os);
    json << metadata;
    return os;
}


//----------------------------------------------------------------------------------------------------------------------

Metadata metadataFromYAML(const std::string& yamlString) {
    std::istringstream in(yamlString);
    eckit::YAMLParser parser(in);
    return toMetadata(parser.parse());
}

//----------------------------------------------------------------------------------------------------------------------


Metadata toMetadata(const eckit::Value& v) {
    if (!v.isMap()) {
        std::ostringstream oss;
        oss << "toMetadata():: eckit::Value is not a map: " << v;
        throw MetadataException(oss.str(), Here());
    }
    Metadata m;

    eckit::Value keys = v.keys();
    for (unsigned int i = 0; i < keys.size(); ++i) {
        std::string key = keys[i];
        auto mv = tryToMetadataValue(v[key]);
        if (mv) {
            m.set(key, *mv);
        }
    }

    return m;
}


//----------------------------------------------------------------------------------------------------------------------

//-----------------------------------------------------------------------------

}  // namespace multio::message

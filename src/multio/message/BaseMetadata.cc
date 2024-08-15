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

#include "multio/message/BaseMetadata.h"

#include <sstream>


namespace multio::message {

//----------------------------------------------------------------------------------------------------------------------

// Constructor for unordered_map
BaseMetadata::BaseMetadata() : values_{512} {}
BaseMetadata::BaseMetadata(std::initializer_list<std::pair<const KeyType, MetadataValue>> li) :
    values_{std::move(li), 512} {}

// Construtore for map
// BaseMetadata::BaseMetadata() : values_{} {}
// BaseMetadata::BaseMetadata(std::initializer_list<std::pair<const KeyType, MetadataValue>> li) :
// values_{std::move(li)} {}

BaseMetadata::BaseMetadata(MapType&& values) : values_{std::move(values)} {}
BaseMetadata::BaseMetadata(const MapType& values) : values_{values} {}


MetadataValue&& BaseMetadata::get(const KeyType& k) && {
    return std::move(referenceGetter(*this, k).get());
}

MetadataValue& BaseMetadata::get(const KeyType& k) & {
    return referenceGetter(*this, k).get();
}

const MetadataValue& BaseMetadata::get(const KeyType& k) const& {
    return referenceGetter(*this, k).get();
}


MetadataValue& BaseMetadata::operator[](const KeyType& key) {
    return values_[key];
}
MetadataValue& BaseMetadata::operator[](KeyType&& key) {
    return values_[std::move(key)];
}


bool BaseMetadata::empty() const noexcept {
    return values_.empty();
}

std::size_t BaseMetadata::size() const noexcept {
    return values_.size();
}

void BaseMetadata::clear() noexcept {
    values_.clear();
}

/**
 * Extracts all values from other metadata whose keys are not contained yet in this metadata.
 * Explicitly always modifies both metadata.
 */
void BaseMetadata::merge(This& other) {
    values_.merge(other.values_);
}
void BaseMetadata::merge(This&& other) {
    values_.merge(std::move(other.values_));
}


void BaseMetadata::updateOverwrite(const BaseMetadata& other) {
    for (const auto& kv : other.values_) {
        values_.insert_or_assign(kv.first, kv.second);
    }
}


void BaseMetadata::updateOverwrite(BaseMetadata&& other) {
    auto tmp = std::move(values_);
    values_ = std::move(other.values_);
    values_.merge(tmp);
}

void BaseMetadata::updateNoOverwrite(const BaseMetadata& other) {
    values_.insert(other.values_.begin(), other.values_.end());
}


void BaseMetadata::updateNoOverwrite(BaseMetadata&& other) {
    values_.merge(other.values_);
}


std::string BaseMetadata::toString() const {
    std::stringstream ss;
    eckit::JSON json(ss);
    json << *this;
    return ss.str();
}

void BaseMetadata::json(eckit::JSON& j) const {
    j << *this;
}

//----------------------------------------------------------------------------------------------------------------------


eckit::JSON& operator<<(eckit::JSON& json, const BaseMetadata& metadata) {
    json.startObject();
    for (const auto& kv : metadata) {
        json << kv.first;
        json << kv.second;
    }
    json.endObject();
    return json;
}


std::ostream& operator<<(std::ostream& os, const BaseMetadata& metadata) {
    eckit::JSON json(os);
    json << metadata;
    return os;
}


//----------------------------------------------------------------------------------------------------------------------


}  // namespace multio::message

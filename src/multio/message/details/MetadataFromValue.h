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

#pragma once

#include "multio/message/details/Metadata.h"

#include "eckit/config/Configuration.h"
#include "eckit/value/Value.h"

#include <sstream>

namespace multio::message::details {

template <typename Traits>
Metadata<Traits> toMetadata(const eckit::Value& value);

//-----------------------------------------------------------------------------

enum class ValueType : unsigned int
{
    NotUsed = 0,
    Double,
    Int,
    Bool,
    String,
    Map,
    List
};

template <ValueType V>
struct ValueTag {};

// JSON/YAMLParser now refactored - that helps for (de)serializing directly without eckit::Value.
// However configurations still use eckit::Value. Therefore these helpers are still required.
template <typename F>
decltype(auto) visitValueType(const eckit::Value& v, F&& f) noexcept {
    if (v.isList()) {
        return std::forward<F>(f)(ValueTag<ValueType::List>{});
    }
    if (v.isMap()) {
        return std::forward<F>(f)(ValueTag<ValueType::Map>{});
    }
    if (v.isNumber()) {
        return std::forward<F>(f)(ValueTag<ValueType::Int>{});
    }
    if (v.isDouble()) {
        return std::forward<F>(f)(ValueTag<ValueType::Double>{});
    }
    if (v.isBool()) {
        return std::forward<F>(f)(ValueTag<ValueType::Bool>{});
    }
    if (v.isString()) {
        return std::forward<F>(f)(ValueTag<ValueType::String>{});
    }
    return std::forward<F>(f)(ValueTag<ValueType::NotUsed>{});
};

template <typename Traits>
std::optional<MetadataValue<Traits>> toMetadataValue(const eckit::Value& v) {
    return visitValueType(
        v, eckit::Overloaded{
               [&v](ValueTag<ValueType::List>) -> std::optional<MetadataValue<Traits>> {
                   if (!v.size()) {
                       return std::nullopt;
                   }
                   auto fillVec = [&v](auto vec) {
                       auto size = v.size();
                       vec.reserve(size);
                       for (unsigned int i = 0; i < size; ++i) {
                           vec.push_back(v[i]);
                       }
                       return MetadataValue<Traits>{std::move(vec)};
                   };
                   return visitValueType(
                       v[0], eckit::Overloaded{
                                 [&fillVec](ValueTag<ValueType::Int>) -> std::optional<MetadataValue<Traits>> {
                                     return fillVec(std::vector<std::int64_t>{});
                                 },
                                 [&fillVec](ValueTag<ValueType::Double>) -> std::optional<MetadataValue<Traits>> {
                                     return fillVec(std::vector<double>{});
                                 },
                                 [&fillVec](ValueTag<ValueType::Bool>) -> std::optional<MetadataValue<Traits>> {
                                     return fillVec(std::vector<bool>{});
                                 },
                                 [&fillVec](ValueTag<ValueType::String>) -> std::optional<MetadataValue<Traits>> {
                                     return fillVec(std::vector<std::string>{});
                                 },
                                 [](auto) -> std::optional<MetadataValue<Traits>> { return std::nullopt; },
                             });
               },
               [&v](ValueTag<ValueType::Map>) -> std::optional<MetadataValue<Traits>> { return toMetadata<Traits>(v); },
               [&v](ValueTag<ValueType::Int>) -> std::optional<MetadataValue<Traits>> {
                   return MetadataValue<Traits>{(std::int64_t)v};
               },
               [&v](ValueTag<ValueType::Double>) -> std::optional<MetadataValue<Traits>> {
                   return MetadataValue<Traits>{(double)v};
               },
               [&v](ValueTag<ValueType::Bool>) -> std::optional<MetadataValue<Traits>> {
                   return MetadataValue<Traits>{(bool)v};
               },
               [&v](ValueTag<ValueType::String>) -> std::optional<MetadataValue<Traits>> {
                   return MetadataValue<Traits>{(std::string)v};
               },
               [](auto) -> std::optional<MetadataValue<Traits>> { return std::nullopt; }});
}


template <typename Traits>
std::optional<Metadata<Traits>> toMetadataMaybe(const eckit::Value& v) {
    if (!v.isMap()) {
        return std::nullopt;
    }
    Metadata m;

    eckit::Value keys = v.keys();
    for (unsigned int i = 0; i < keys.size(); ++i) {
        std::string key = keys[i];
        auto mv = toMetadataValue<Traits>(v[key]);
        if (mv) {
            m.set(key, *mv);
        }
    }

    return m;
}

template <typename Traits>
Metadata<Traits> toMetadata(const eckit::Value& value) {
    auto optMetadata = toMetadataMaybe<Traits>(value);
    if (!optMetadata) {
        std::ostringstream oss;
        oss << "eckit::Value is not a map: " << value;
        throw MetadataException(oss.str(), Here());
    }
    return std::move(*optMetadata);
}

template <typename Traits>
Metadata<Traits> toMetadata(const std::string& fieldId) {
    std::istringstream in(fieldId);
    eckit::YAMLParser parser(in);
    auto optMetadata = toMetadataMaybe<Traits>(parser.parse());
    if (!optMetadata) {
        throw MetadataException(std::string("JSON string must start with a map: ") + fieldId, Here());
    }
    return std::move(*optMetadata);
}


//-----------------------------------------------------------------------------

}  // namespace multio::message::details

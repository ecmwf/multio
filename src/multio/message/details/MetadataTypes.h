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

#include "multio/util/TypeTraits.h"

#include <cstdint>
#include <iostream>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>


namespace multio::message::details {

//-----------------------------------------------------------------------------

// Default traits for a Metadata. Traits allow customizing key and map type to
// easily benchmark different setups
struct DefaultMetadataTraits {
    using KeyType = std::string;

    template <typename ValueType>
    using MapType = std::unordered_map<KeyType, ValueType>;

    template <typename ValueType>
    static MapType<ValueType> initMap() {
        return MapType<ValueType>{256};
    }

    template <typename ValueType>
    static MapType<ValueType> initMap(std::initializer_list<std::pair<const KeyType, ValueType>> li) {
        return MapType<ValueType>{std::move(li), 256};
    }
};

// Forward declaration
template <typename MetadataTraits = DefaultMetadataTraits>
class Metadata;

struct Null {
    constexpr operator bool() { return false; }
};

constexpr bool operator<(Null, Null) {
    return false;
}

std::ostream& operator<<(std::ostream& os, const Null&);


// MetadataTypes may be specialized for different types to support different set of types
template <typename Traits = DefaultMetadataTraits>
struct MetadataTypes {
    using Nulls = util::TypeList<Null>;
    using Integers = util::TypeList<bool, std::int8_t, std::int16_t, std::int32_t, std::int64_t>;
    using Floats = util::TypeList<double, float>;
    using Strings = util::TypeList<std::string>;
    using NonNullScalars = util::MergeTypeList_t<Integers, Floats, Strings>;
    using Scalars = util::MergeTypeList_t<Nulls, NonNullScalars>;

    using IntegerLists = util::MapTypeList_t<std::vector, Integers>;
    using FloatLists = util::MapTypeList_t<std::vector, Floats>;
    using StringLists = util::MapTypeList_t<std::vector, Strings>;
    using Lists = util::MergeTypeList_t<IntegerLists, FloatLists, StringLists>;

    using Nested = util::TypeList<Metadata<Traits>>;
    using NestedWrapped = util::MapTypeList_t<std::unique_ptr, Nested>;

    // Example for general lists:
    // using NestedLists = util::MapTypeList_t<std::vector, Nested>;
    // For metadata we don't want it. But It may be easy to move that code and use it as value through specializing only
    // Traits and this Types object
    using NestedLists = util::TypeList<>;
    using NestedListsWrapped = util::MapTypeList_t<std::unique_ptr, NestedLists>;

    using AllNonLists = util::MergeTypeList_t<Scalars, Nested>;
    using AllLists = util::MergeTypeList_t<Lists, NestedLists>;
    using AllNested = util::MergeTypeList_t<Nested, NestedLists>;

    using All = util::MergeTypeList_t<Scalars, Lists, Nested, NestedLists>;
    using AllWrapped = util::MergeTypeList_t<Scalars, Lists, NestedWrapped, NestedListsWrapped>;
};

//-----------------------------------------------------------------------------

}  // namespace multio::message::details

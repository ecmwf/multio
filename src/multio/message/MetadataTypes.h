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

#include "multio/message/PrehashedKey.h"
#include "multio/util/TypeTraits.h"

#include "eckit/log/JSON.h"


#include <cstdint>
#include <iostream>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>


namespace multio::message {

//-----------------------------------------------------------------------------

// Forward declaration
class BaseMetadata;
class Metadata;

//-----------------------------------------------------------------------------

struct Null {
    constexpr operator bool() { return false; }
};

constexpr bool operator<(Null, Null) noexcept {
    return false;
}

constexpr bool operator==(Null, Null) noexcept {
    return true;
}
constexpr bool operator!=(Null, Null) noexcept {
    return false;
}

std::ostream& operator<<(std::ostream& os, const Null&);
eckit::JSON& operator<<(eckit::JSON& json, const Null&);


//-----------------------------------------------------------------------------


// MetadataTypes
//
// Struct containing all types definition for the Metadata.
// Theres a special handling for unique_ptr - To support nested types it is necessary to wrap a type into a smart
// pointer or to create a class with explicit memory handling. To avoid explicit memory handling, types wrapped in a
// unique pointer will be handled transperently through a `get` and `visit` calls on the metadata object. Thus, the fact
// that a unique_ptr is used is hidden from the user.
struct MetadataTypes {
    // using KeyType = std::string;
    using KeyType = PrehashedKey<std::string>;

    template <typename ValueType>
    using MapType = std::unordered_map<KeyType, ValueType>;


    using Nulls = util::TypeList<Null>;
    using Integers = util::TypeList<bool, std::int64_t>;
    using Floats = util::TypeList<double, float>;
    using Strings = util::TypeList<std::string>;
    using NonNullScalars = util::MergeTypeList_t<Integers, Floats, Strings>;
    using Scalars = util::MergeTypeList_t<Nulls, NonNullScalars>;

    using Bytes = util::TypeList<std::vector<unsigned char>>;
    using IntegerLists = util::MapTypeList_t<std::vector, Integers>;
    using FloatLists = util::MapTypeList_t<std::vector, Floats>;
    using StringLists = util::MapTypeList_t<std::vector, Strings>;
    using Lists = util::MergeTypeList_t<Bytes, IntegerLists, FloatLists, StringLists>;

    using Nested = util::TypeList<BaseMetadata>;
    using NestedWrapped = util::MapTypeList_t<std::unique_ptr, Nested>;  // Used for memory layout. Hidden from user.

    // Example for general lists:
    // using NestedLists = util::MapTypeList_t<std::vector, Nested>;
    // For metadata we don't want it. But It may be easy to move that code and use it as value through specializing only
    // Traits and this Types object
    using NestedLists = util::TypeList<>;
    using NestedListsWrapped
        = util::MapTypeList_t<std::unique_ptr, NestedLists>;  // Used for memory layout. Hidden from user.

    using AllNonLists = util::MergeTypeList_t<Scalars, Nested>;
    using AllLists = util::MergeTypeList_t<Lists, NestedLists>;
    using AllNested = util::MergeTypeList_t<Nested, NestedLists>;

    using All = util::MergeTypeList_t<Scalars, Lists, Nested, NestedLists>;
    using AllWrapped = util::MergeTypeList_t<Scalars, Lists, NestedWrapped,
                                             NestedListsWrapped>;  // Used for memory layout. Hidden from user.
};

// Helper for Intel ICPC - int is not deduced to int64_t automatically even not through the reimplemented sane
// conversion, however we don't want to miss that convenience
template <typename T>
struct MetadataValueConversionHelper;
template <typename T>
using MetadataValueConversionHelper_t = typename MetadataValueConversionHelper<T>::type;

template <typename, class = void>
struct HasMetadataValueConversionHelper : std::false_type {};

template <typename T>
struct HasMetadataValueConversionHelper<T, std::void_t<MetadataValueConversionHelper_t<T>>> : std::true_type {};

// Finally int specialization
template <>
struct MetadataValueConversionHelper<int> {
    using type = std::int64_t;
};
template <>
struct MetadataValueConversionHelper<long long> {
    using type = std::int64_t;
};


//-----------------------------------------------------------------------------

}  // namespace multio::message


template <>
struct std::hash<multio::message::Null> {
    std::size_t operator()(const multio::message::Null&) const noexcept(true) { return 0; }
};

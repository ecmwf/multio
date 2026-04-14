/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @file Entry.h
/// @brief Shared field descriptor and traits for declarative struct parsing/dumping.
///
/// This header provides the common infrastructure used by both the config parser
/// (util/config/Parser.h) and the metadata parser (datamod/Parser.h).
///
/// A "parseable struct" opts in by defining a static constexpr fields_ tuple of Entry descriptors:
/// @code
///   struct MyStruct {
///       std::string name;
///       std::int64_t count = 0;
///       static constexpr auto fields_ = std::make_tuple(
///           requiredEntry("name", &MyStruct::name),
///           optionalEntry("count", &MyStruct::count)
///       );
///   };
/// @endcode

#pragma once

#include <optional>
#include <string_view>
#include <tuple>
#include <type_traits>


namespace multio::util::record {


//----------------------------------------------------------------------------------------------------------------------
// Entry -- a single field descriptor binding a string key to a C++ member pointer
//----------------------------------------------------------------------------------------------------------------------

template <typename TStruct, typename TValue>
struct Entry {
    const std::string_view key;
    TValue TStruct::* value;
    const bool required;

    TValue& get(TStruct& obj) const { return obj.*value; }
    const TValue& get(const TStruct& obj) const { return obj.*value; }
};


//----------------------------------------------------------------------------------------------------------------------
// HasFieldsMember -- SFINAE trait detecting whether TStruct has a static fields_ member
//----------------------------------------------------------------------------------------------------------------------

template <typename TStruct, class = void>
struct HasFieldsMember : std::false_type {};

template <typename TStruct>
struct HasFieldsMember<TStruct, std::void_t<decltype(TStruct::fields_)>> : std::true_type {};

template <typename TStruct>
inline constexpr bool HasFieldsMember_v = HasFieldsMember<TStruct>::value;


//----------------------------------------------------------------------------------------------------------------------
// containsKey -- check if a key exists in a struct's fields_ tuple
//----------------------------------------------------------------------------------------------------------------------

template <typename TStruct>
bool containsKey(const std::string& key) {
    return std::apply([&](const auto&... field) { return ((field.key == key) || ... || false); }, TStruct::fields_);
}


//----------------------------------------------------------------------------------------------------------------------
// Factory functions
//----------------------------------------------------------------------------------------------------------------------

/// Register a required field in the struct
template <typename TStruct, typename TValue>
constexpr Entry<TStruct, TValue> requiredEntry(const std::string_view& key, TValue TStruct::* value) {
    return {key, value, true};
}

/// Register an optional (or defaulted) field in the struct
template <typename TStruct, typename TValue>
constexpr Entry<TStruct, TValue> optionalEntry(const std::string_view& key, TValue TStruct::* value) {
    return {key, value, false};
}


//----------------------------------------------------------------------------------------------------------------------
// OptionalValueType_t — extracts T from std::optional<T>
//
// Used by mars2mars matchers/setters to work with the inner value type of optional fields.
//----------------------------------------------------------------------------------------------------------------------

template <typename T>
struct OptionalValueType;

template <typename T>
struct OptionalValueType<std::optional<T>> {
    using type = T;
};

template <typename T>
using OptionalValueType_t = typename OptionalValueType<T>::type;


}  // namespace multio::util::record

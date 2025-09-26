/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#pragma once

#include <chrono>
#include <functional>
#include <memory>  
#include <optional>
#include <string>
#include <tuple>
#include <type_traits>
#include <variant>
#include <vector>

#include "eckit/config/LocalConfiguration.h"
#include "eckit/filesystem/PathName.h"

namespace multio::util {

//-----------------------------------------------------------------------------

// Helper to stringify types
// Instead of using a constexpr string_view, a call operator is used at runtime.
// This is because this information is usally needed at runtime for proper error or logging descriptions
// and on this way it is easier to concatenate typenames for templated types like variant or tuple
//
// With C++20 all could be written with constexpr strings

template <typename T>
struct TypeToString;

template <typename T>
std::string typeToString() {
    return TypeToString<T>{}();
}

template <typename T>
struct TypeToString<const T> {
    std::string operator()() const { return std::string("const ") + typeToString<std::remove_cv_t<T>>(); };
};

template <typename T>
struct TypeToString<T&> {
    std::string operator()() const { return typeToString<std::remove_reference_t<T>>() + std::string("&"); };
};
template <typename T>
struct TypeToString<T&&> {
    std::string operator()() const { return typeToString<std::remove_reference_t<T>>() + std::string("&&"); };
};


template <>
struct TypeToString<std::string> {
    std::string operator()() const { return "std::string"; };
};

template <>
struct TypeToString<char> {
    std::string operator()() const { return "char"; };
};
template <>
struct TypeToString<bool> {
    std::string operator()() const { return "bool"; };
};
template <>
struct TypeToString<double> {
    std::string operator()() const { return "double"; };
};
template <>
struct TypeToString<float> {
    std::string operator()() const { return "float"; };
};
template <>
struct TypeToString<std::int64_t> {
    std::string operator()() const { return "std::int64_t"; };
};
template <>
struct TypeToString<std::int32_t> {
    std::string operator()() const { return "std::int32_t"; };
};
template <>
struct TypeToString<std::int16_t> {
    std::string operator()() const { return "std::int16_t"; };
};
template <>
struct TypeToString<std::int8_t> {
    std::string operator()() const { return "std::int8_t"; };
};
template <>
struct TypeToString<std::uint64_t> {
    std::string operator()() const { return "std::uint64_t"; };
};
template <>
struct TypeToString<std::uint32_t> {
    std::string operator()() const { return "std::uint32_t"; };
};
template <>
struct TypeToString<std::uint16_t> {
    std::string operator()() const { return "std::uint16_t"; };
};
template <>
struct TypeToString<std::uint8_t> {
    std::string operator()() const { return "std::uint8_t"; };
};


template <typename T>
struct TypeToString<std::optional<T>> {
    std::string operator()() const { return std::string("std::optional<") + typeToString<T>() + std::string(">"); };
};

// TODO handle allocator to string?...
template <typename T, typename Alloc>
struct TypeToString<std::vector<T, Alloc>> {
    std::string operator()() const { return std::string("std::vector<") + typeToString<T>() + std::string(">"); };
};


template <typename... T>
struct TypeToString<std::tuple<T...>> {
    std::string operator()() const {
        return std::string("std::tuple<") + ((typeToString<T>() + std::string(", ")) + ... + std::string(">"));
    };
};

template <typename... T>
struct TypeToString<std::variant<T...>> {
    std::string operator()() const {
        return std::string("std::variant<") + ((typeToString<T>() + std::string(", ")) + ... + std::string(">"));
    };
};

template <typename T>
struct TypeToString<std::reference_wrapper<T>> {
    std::string operator()() const {
        return std::string("std::reference_wrapper<") + typeToString<T>() + std::string(">");
    };
};

template <typename T, typename Deleter>
struct TypeToString<std::unique_ptr<T, Deleter>> {
    std::string operator()() const {
        return std::string("std::unique_ptr<") + typeToString<T>() + std::string(", Deleter>");
    };
};

template <typename T>
struct TypeToString<std::shared_ptr<T>> {
    std::string operator()() const { return std::string("std::shared_ptr<") + typeToString<T>() + std::string(">"); };
};

template <>
struct TypeToString<std::chrono::hours> {
    std::string operator()() const { return std::string("std::chrono::hours"); };
};

template <>
struct TypeToString<std::chrono::seconds> {
    std::string operator()() const { return std::string("std::chrono::seconds"); };
};

template <>
struct TypeToString<eckit::LocalConfiguration> {
    std::string operator()() const { return std::string("eckit::LocalConfiguration"); };
};

template <>
struct TypeToString<eckit::PathName> {
    std::string operator()() const { return std::string("eckit::PathName"); };
};

//-----------------------------------------------------------------------------


}  // namespace multio::util


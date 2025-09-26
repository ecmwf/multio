/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @date Jan 2024

#pragma once

#include "Hash.h"

#include "eckit/log/JSON.h"

#include <functional>
#include <iostream>
#include <string_view>
#include <type_traits>


namespace multio::util {

//----------------------------------------------------------------------------------------------------------------------

template <typename T>
class PrehashedKey {
public:
    using This = PrehashedKey<T>;
    using ValueType = T;
    using HashType = std::decay_t<decltype(std::hash<T>{}(std::declval<T>()))>;

    PrehashedKey(const ValueType& v) : value_{v}, hash_{std::hash<T>{}(value_)} {}
    PrehashedKey(ValueType&& v) : value_{std::move(v)}, hash_{std::hash<T>{}(value_)} {}

    PrehashedKey(const This&) = default;
    PrehashedKey(This&&) = default;

    This& operator=(const This&) = default;
    This& operator=(This&&) = default;

    template <typename TC,
              std::enable_if_t<(!std::is_same_v<std::decay_t<TC>, ValueType> && !std::is_same_v<std::decay_t<TC>, This>
                                && std::is_constructible_v<ValueType, TC>),
                               bool>
              = true>
    PrehashedKey(TC&& v) : value_{std::forward<TC>(v)}, hash_{std::hash<T>{}(value_)} {}

    operator ValueType&&() && noexcept { return std::move(value_); }
    operator const ValueType&() const& noexcept { return value_; }

    ValueType&& value() && noexcept { return std::move(value_); }
    const ValueType& value() const& noexcept { return value_; }

    const HashType& hash() const noexcept { return hash_; }

private:
    ValueType value_;
    HashType hash_;
};
}  // namespace multio::util

namespace std {

template <typename T>
struct equal_to<multio::util::PrehashedKey<T>> {
    using HashType = typename multio::util::PrehashedKey<T>::HashType;
    constexpr bool operator()(const multio::util::PrehashedKey<T>& lhs, const multio::util::PrehashedKey<T>& rhs) const
        noexcept(noexcept(std::equal_to<HashType>{}(lhs.hash(), rhs.hash())
                          && std::equal_to<T>{}(lhs.value(), rhs.value()))) {
        return std::equal_to<HashType>{}(lhs.hash(), rhs.hash()) && std::equal_to<T>{}(lhs.value(), rhs.value());
    }
};


template <typename T>
struct less<multio::util::PrehashedKey<T>> {
    constexpr bool operator()(const multio::util::PrehashedKey<T>& lhs, const multio::util::PrehashedKey<T>& rhs) const
        noexcept(noexcept(std::less<T>{}(lhs.value(), rhs.value()))) {
        return std::less<T>{}(lhs.value(), rhs.value());
    }
};

}  // namespace std


namespace multio::util {


template <typename T>
constexpr bool operator<(const PrehashedKey<T>& lhs,
                         const PrehashedKey<T>& rhs) noexcept(noexcept(std::less<PrehashedKey<T>>{}(lhs, rhs))) {
    return std::less<PrehashedKey<T>>{}(lhs, rhs);
}

template <typename T>
constexpr bool operator==(const PrehashedKey<T>& lhs,
                          const PrehashedKey<T>& rhs) noexcept(noexcept(std::equal_to<PrehashedKey<T>>{}(lhs, rhs))) {
    return std::equal_to<PrehashedKey<T>>{}(lhs, rhs);
}

template <typename T>
std::ostream& operator<<(std::ostream& os, const PrehashedKey<T>& k) {
    os << k.value();
    return os;
};

template <typename T>
eckit::JSON& operator<<(eckit::JSON& json, const PrehashedKey<T>& k) {
    json << k.value();
    return json;
};

//----------------------------------------------------------------------------------------------------------------------

// Specializations for string to be constexpr evaluatable. Unfortunately std::hash is not

template <>
class PrehashedKey<std::string_view> {
public:
    using This = PrehashedKey<std::string_view>;
    using ValueType = std::string_view;
    using HashType = std::uint64_t;

    constexpr PrehashedKey(const ValueType& v) : value_{v}, hash_{hashFNV1A64(value_.data(), value_.size())} {}
    constexpr PrehashedKey(ValueType&& v) : value_{std::move(v)}, hash_{hashFNV1A64(value_.data(), value_.size())} {}

    constexpr PrehashedKey(const This&) = default;
    constexpr PrehashedKey(This&&) = default;

    template <typename T, std::enable_if_t<std::is_same_v<T, std::string>, bool> = true>
    constexpr PrehashedKey(const PrehashedKey<T>& phstr) : value_{phstr.value()}, hash_{phstr.hash()} {};

    constexpr This& operator=(const This&) = default;
    constexpr This& operator=(This&&) = default;

    // Used for inter operation
    operator std::string() const { return std::string(value_); }

    template <typename TC,
              std::enable_if_t<(!std::is_same_v<std::decay_t<TC>, ValueType>
                                && !std::is_same_v<std::decay_t<TC>, PrehashedKey<std::string>>
                                && !std::is_same_v<std::decay_t<TC>, This> && std::is_constructible_v<ValueType, TC>),
                               bool>
              = true>
    constexpr PrehashedKey(TC&& v) : value_{std::forward<TC>(v)}, hash_{hashFNV1A64(value_.data(), value_.size())} {}

    constexpr operator ValueType&&() && noexcept { return std::move(value_); }
    constexpr operator const ValueType&() const& noexcept { return value_; }

    constexpr ValueType&& value() && noexcept { return std::move(value_); }
    constexpr const ValueType& value() const& noexcept { return value_; }

    constexpr const HashType& hash() const noexcept { return hash_; }

private:
    ValueType value_;
    HashType hash_;
};


template <>
class PrehashedKey<std::string> {
public:
    using This = PrehashedKey<std::string>;
    using ValueType = std::string;
    using HashType = std::uint64_t;

    PrehashedKey(const ValueType& v) : value_{v}, hash_{hashFNV1A64(value_.data(), value_.size())} {}
    PrehashedKey(ValueType&& v) : value_{std::move(v)}, hash_{hashFNV1A64(value_.data(), value_.size())} {}

    PrehashedKey(const This&) = default;
    PrehashedKey(This&&) = default;

    PrehashedKey(const PrehashedKey<std::string_view>& phstr) : value_{phstr.value()}, hash_{phstr.hash()} {};

    This& operator=(const This&) = default;
    This& operator=(This&&) = default;

    template <typename TC,
              std::enable_if_t<(!std::is_same_v<std::decay_t<TC>, ValueType>
                                && !std::is_same_v<std::decay_t<TC>, PrehashedKey<std::string_view>>
                                && !std::is_same_v<std::decay_t<TC>, This> && std::is_constructible_v<ValueType, TC>),
                               bool>
              = true>
    PrehashedKey(TC&& v) : value_{std::forward<TC>(v)}, hash_{hashFNV1A64(value_.data(), value_.size())} {}

    operator ValueType&&() && noexcept { return std::move(value_); }
    operator const ValueType&() const& noexcept { return value_; }

    ValueType&& value() && noexcept { return std::move(value_); }
    const ValueType& value() const& noexcept { return value_; }

    const HashType& hash() const noexcept { return hash_; }

private:
    ValueType value_;
    HashType hash_;
};


//----------------------------------------------------------------------------------------------------------------------


}  // namespace multio::util


//----------------------------------------------------------------------------------------------------------------------

template <typename T>
struct std::hash<multio::util::PrehashedKey<T>> {
    using HashType = typename multio::util::PrehashedKey<T>::HashType;

    constexpr HashType operator()(const multio::util::PrehashedKey<T>& t) const noexcept { return t.hash(); };
};

//----------------------------------------------------------------------------------------------------------------------

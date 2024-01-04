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

/// @date Jan 2024

#pragma once

#include "eckit/log/JSON.h"

#include <functional>
#include <iostream>
#include <type_traits>


namespace multio::message {

//----------------------------------------------------------------------------------------------------------------------

template <typename T>
class PrehashedKey {
public:
    using ValueType = T;
    using HashType = std::decay_t<decltype(std::hash<T>{}(std::declval<T>()))>;

    PrehashedKey(const T& v) : value_{v}, hash_{std::hash<T>{}(value_)} {}
    PrehashedKey(T&& v) : value_{std::move(v)}, hash_{std::hash<T>{}(value_)} {}

    template <typename TC,
              std::enable_if_t<(!std::is_same_v<std::decay_t<TC>, T> && std::is_constructible_v<T, TC>), bool> = true>
    PrehashedKey(TC&& v) : value_{std::forward<TC>(v)}, hash_{std::hash<T>{}(value_)} {}

    operator T&&() && noexcept { return std::move(value_); }
    operator const T&() const& noexcept { return value_; }

    T&& value() && noexcept { return std::move(value_); }
    const T& value() const& noexcept { return value_; }

    const HashType& hash() const noexcept { return hash_; }

private:
    T value_;
    HashType hash_;
};

template <typename T>
bool operator<(const PrehashedKey<T>& lhs, const PrehashedKey<T>& rhs) noexcept(noexcept(lhs.value() < rhs.value())) {
    return lhs.value() < rhs.value();
}

template <typename T>
bool operator==(const PrehashedKey<T>& lhs,
                const PrehashedKey<T>& rhs) noexcept(noexcept((lhs.hash() == rhs.hash())
                                                              && (lhs.value() == rhs.value()))) {
    return (lhs.hash() == rhs.hash()) && (lhs.value() == rhs.value());
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


}  // namespace multio::message


//----------------------------------------------------------------------------------------------------------------------

template <typename T>
struct std::hash<multio::message::PrehashedKey<T>> {
    using HashType = typename multio::message::PrehashedKey<T>::HashType;

    HashType operator()(const multio::message::PrehashedKey<T>& t) const noexcept { return t.hash(); };
};

//----------------------------------------------------------------------------------------------------------------------

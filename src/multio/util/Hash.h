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

/// @date Aug 2022


#pragma once


#include <functional>

namespace multio::util {

//-----------------------------------------------------------------------------


template <typename T>
auto hash(const T& t) noexcept(noexcept(std::hash<T>{}(t))) {
    return std::hash<T>{}(t);
}

// std is lacking a hash_combine mechanism. Hence one is provided here following
// the common approach of Fibonacci-Hashing with golden ratio.
// Readup:
//  * https://asecuritysite.com/hash/smh_fib
//  * https://github.com/HowardHinnant/hash_append/issues/7
//  * https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2018/p0814r2.pdf
template <typename IntType,
          std::enable_if_t<
              std::is_integral<IntType>::value && std::is_unsigned<IntType>::value && (sizeof(IntType) == 8), bool>
          = true>
constexpr IntType hash_append(IntType lhs, IntType rhs) noexcept {
    return lhs ^ (rhs + 0x9e3779b97f4a7c15LLU + (lhs << 12) + (lhs >> 4));
}

template <typename IntType,
          std::enable_if_t<
              std::is_integral<IntType>::value && std::is_unsigned<IntType>::value && (sizeof(IntType) == 4), bool>
          = true>
constexpr IntType hash_append(IntType lhs, IntType rhs) noexcept {
    return lhs ^ (rhs + 0x9e3779b9U + (rhs << 6) + (rhs >> 2));
}


std::size_t hash_combine_args(std::size_t lhs) noexcept;

template <typename T, typename... More>
std::size_t hash_combine_args(std::size_t lhs, const T& t,
                              const More&... more) noexcept((noexcept(hash(more)) && ...&& noexcept(hash(t)))) {
    return hash_combine_args(hash_append(lhs, hash(t)), more...);
}


template <typename T, typename... More>
std::size_t hash_combine(const T& t, const More&... more) noexcept((noexcept(hash(more)) && ...&& noexcept(hash(t)))) {
    return hash_combine_args(hash(t), more...);
}


// Fibonacci hashing - good to mix hash or map to a small interval (alternative to modulo)
//  * https://asecuritysite.com/hash/smh_fib
//  * https://probablydance.com/2018/06/16/fibonacci-hashing-the-optimization-that-the-world-forgot-or-a-better-alternative-to-integer-modulo/
template <std::size_t bitWidthOfInterest = 64>
constexpr std::uint64_t fib_hash(std::uint64_t h) noexcept {
    return (h * 0x9e3779b97f4a7c15LLU) >> (64 - bitWidthOfInterest);
}

template <std::size_t bitWidthOfInterest = 32>
constexpr std::uint32_t fib_hash(std::uint32_t h) noexcept {
    return (h * 0x9e3779b9U) >> (64 - bitWidthOfInterest);
}


//-----------------------------------------------------------------------------

}  // namespace multio::util


#ifdef MULTIO_TUPLE_HASH_SPECIALIZATION

#include <tuple>

template <typename... T>
struct std::hash<std::tuple<T...>> {
    std::size_t operator()(const std::tuple<T...>& t) const
        noexcept(noexcept(multio::util::hash_combine(std::declval<const T&>()...))) {
        return std::apply([](const auto&... args) { return multio::util::hash_combine(args...); }, t);
    }
};

#endif

//-----------------------------------------------------------------------------


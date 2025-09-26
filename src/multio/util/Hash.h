/*
 * (C) Copyright 2025- ECMWF.
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


#include <cstdint>
#include <functional>
#include <tuple>

namespace multio::util {

//-----------------------------------------------------------------------------


template <typename T>
constexpr auto hash(const T& t) noexcept(noexcept(std::hash<T>{}(t))) {
    return std::hash<T>{}(t);
}

// std is lacking a hashCombine mechanism. Hence one is provided here following
// the common approach of Fibonacci-Hashing with golden ratio.
// Readup:
//  * https://asecuritysite.com/hash/smh_fib
//  * https://github.com/HowardHinnant/hash_append/issues/7
//  * https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2018/p0814r2.pdf
template <typename IntType,
          std::enable_if_t<
              std::is_integral<IntType>::value && std::is_unsigned<IntType>::value && (sizeof(IntType) == 8), bool>
          = true>
constexpr IntType hashAppend(IntType lhs, IntType rhs) noexcept {
    return lhs ^ (rhs + 0x9e3779b97f4a7c15LLU + (lhs << 12) + (lhs >> 4));
}

template <typename IntType,
          std::enable_if_t<
              std::is_integral<IntType>::value && std::is_unsigned<IntType>::value && (sizeof(IntType) == 4), bool>
          = true>
constexpr IntType hashAppend(IntType lhs, IntType rhs) noexcept {
    return lhs ^ (rhs + 0x9e3779b9U + (rhs << 6) + (rhs >> 2));
}


constexpr std::size_t hashCombineArgs(std::size_t lhs) noexcept {
    return lhs;
}

template <typename T, typename... More>
constexpr std::size_t hashCombineArgs(std::size_t lhs, const T& t,
                                      const More&... more) noexcept((noexcept(hash(more)) && ...
                                                                     && noexcept(hash(t)))) {
    return hashCombineArgs(hashAppend(lhs, hash(t)), more...);
}


template <typename T, typename... More>
constexpr std::size_t hashCombine(const T& t,
                                  const More&... more) noexcept((noexcept(hash(more)) && ... && noexcept(hash(t)))) {
    return hashCombineArgs(hash(t), more...);
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

// FNV-1a 32bit hash
constexpr std::uint32_t hashFNV1A32(char const* s, std::size_t size) {
    return (((size > 0) ? hashFNV1A32(s, size - 1) : 2166136261u) ^ s[size]) * 16777619u;
}

// FNV-1a 64bit hash
constexpr std::uint64_t hashFNV1A64(char const* s, std::size_t size) {
    return (((size > 0) ? hashFNV1A64(s, size - 1) : 14695981039346656037u) ^ s[size]) * 1099511628211u;
}

//-----------------------------------------------------------------------------


template <typename... T>
struct HashableTuple {
    std::tuple<T...> tuple;

    operator std::tuple<T...> const&() const { return tuple; }
    operator std::tuple<T...>&&() { return std::move(tuple); }
};

//-----------------------------------------------------------------------------

}  // namespace multio::util


// template <std::size_t I, typename ...T>
// decltype(auto) std::get(multio::util::HashableTuple<T...>&& v)
// {
//     return std::get<I>(static_cast<std::tuple<T...>&&>(v));
// }
// template <std::size_t I, typename ...T>
// decltype(auto) std::get(multio::util::HashableTuple<T...>& v)
// {
//     return std::get<I>(static_cast<std::tuple<T...>&>(v));
// }
// template <std::size_t I, typename ...T>
// decltype(auto) std::get(multio::util::HashableTuple<T...> const& v)
// {
//     return std::get<I>(static_cast<std::tuple<T...> const&>(v));
// }


template <typename... T>
struct std::hash<multio::util::HashableTuple<T...>> {
    std::size_t operator()(const std::tuple<T...>& t) const
        noexcept(noexcept(multio::util::hashCombine(std::declval<const T&>()...))) {
        return std::apply([](const auto&... args) { return multio::util::hashCombine(args...); }, t);
    }
};


//-----------------------------------------------------------------------------

/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#pragma once

#include <iostream>
#include <variant>

namespace multio::util {

//-----------------------------------------------------------------------------

// TLDR: Alternative mechanism to `operator<<(std::ostream&, ...)` to be used in templated contexts
//
// This is an alternative to rely on implementing `operator<<` to provide printing mechanisms for
// templated types where the nested type also needs to be passed to `operator<<`.
// While CLang is doing this all fine, gcc can complain that it does not find a proper overlead.
// With explicit template instantiation this problem is avoided.

template <typename T>
struct Print;


// until we can use C++20 ...

// // Concept: checks if Print<T> exists and has a proper print method
// template<typename T>
// concept Printable =
//     is_complete<Print<T>>::value &&  // 1️⃣ must be specialized
//         requires(std::ostream& os, const T& t) {
//                 { Print<T>::print(os, t) } -> std::same_as<void>; // 2️⃣ must have correct signature
//         };


template <typename T, class = void>
struct Printable : std::false_type {};

template <typename T>
struct Printable<T, std::void_t<decltype(Print<T>::print(std::declval<std::ostream&>(), std::declval<const T&>()))>>
    : std::true_type {};

template <typename T>
inline constexpr bool Printable_v = Printable<T>::value;


template <typename T, class = void>
struct OstreamPrintable : std::false_type {};

template <typename T>
struct OstreamPrintable<T, std::void_t<decltype(std::declval<std::ostream&>() << std::declval<const T&>())>>
    : std::true_type {};

template <typename T>
inline constexpr bool OstreamPrintable_v = OstreamPrintable<T>::value;


// Function to call print on Print<T>
template <typename T, std::enable_if_t<Printable_v<T>, bool> = true>
void print(std::ostream& os, const T& v) {
    Print<T>::print(os, v);
}


// Function to call ostream<< if Print<T>::print is not defined
template <typename T, std::enable_if_t<!Printable_v<T> && OstreamPrintable_v<T>, bool> = true>
void print(std::ostream& os, const T& v) {
    os << v;
}


// This enforces that every class that implements Print, must not implement
// operator<<
template <typename T, std::enable_if_t<multio::util::Printable_v<T>, bool> = true>
std::ostream& operator<<(std::ostream& os, const T& v) {
    multio::util::print(os, v);
    return os;
}


//-----------------------------------------------------------------------------

template <typename... T>
struct Print<std::variant<T...>> {
    static void print(std::ostream& os, const std::variant<T...>& var) {
        std::visit([&](const auto& val) { os << val; }, var);
    }
};

//-----------------------------------------------------------------------------


}  // namespace multio::util


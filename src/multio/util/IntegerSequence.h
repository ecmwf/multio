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

/**
 * Integer_sequence and utils - simiar to C++14...
 * std::integer_sequence forbids using enums inside
 */

#pragma once

#include <type_traits>

namespace multio::util {

template <typename T, T... Args>
struct integer_sequence {
    typedef T value_type;

    static inline std::size_t size() { return (sizeof...(Args)); }

    static inline std::array<T, sizeof...(Args)> to_array() { return std::array<T, sizeof...(Args)>{Args...}; };
};


}  // namespace multio::util

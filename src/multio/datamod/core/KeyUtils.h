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

#include <tuple>
#include <type_traits>
#include "multio/util/TypeTraits.h"


namespace multio::datamod {

//-----------------------------------------------------------------------------
// Helpers to perform access on tuple through enum
// Requires contained types to have a `::id` defined
//-----------------------------------------------------------------------------

namespace keyUtils {

template <auto val, typename Tup, std::size_t I1, std::size_t... I, std::enable_if_t<(sizeof...(I) == 0), bool> = true>
constexpr std::size_t getKeyIndexById(std::index_sequence<I1, I...>) {
    using T1 = std::tuple_element_t<I1, Tup>;
    static_assert(std::is_same_v<std::decay_t<decltype(T1::id)>, decltype(val)>);
    static_assert(T1::id == val, "Non of the types match the key");
    return 0;
}
template <auto val, typename Tup, std::size_t I1, std::size_t... I, std::enable_if_t<(sizeof...(I) > 0), bool> = true>
constexpr std::size_t getKeyIndexById(std::index_sequence<I1, I...>) {
    using T1 = std::tuple_element_t<I1, Tup>;
    if constexpr (std::is_same_v<std::decay_t<decltype(T1::id)>, decltype(val)>) {
        if constexpr (T1::id == val) {
            return 0;
        }
        else {
            return 1 + getKeyIndexById<val, Tup>(std::index_sequence<I...>{});
        }
    }
    else {
        return 1 + getKeyIndexById<val, Tup>(std::index_sequence<I...>{});
    }
    return 0;  // Unreachable - avoid compiler warning
}


template <auto keyId, typename Tup, std::enable_if_t<util::IsTuple_v<std::decay_t<Tup>>, bool> = true>
decltype(auto) getById(Tup&& tup) {
    return std::get<getKeyIndexById<keyId, std::decay_t<Tup>>(
        std::make_index_sequence<std::tuple_size_v<std::decay_t<Tup>>>{})>(std::forward<Tup>(tup));
}


}  // namespace keyUtils

}  // namespace multio::datamod



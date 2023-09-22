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

/// @date July 2023

#pragma once

#include <memory>
#include <variant>

#include "TypeTraits.h"
#include "eckit/utils/Overloaded.h"
#include "eckit/utils/Translator.h"

//-----------------------------------------------------------------------------

namespace multio::util {

template <typename T, typename V>
struct GetVariantIndex;

template <typename T, typename... Ts>
struct GetVariantIndex<T, std::variant<Ts...>>
    : std::integral_constant<size_t, std::variant<TypeTag<Ts>...>{TypeTag<T>{}}.index()> {};

//-----------------------------------------------------------------------------

template <typename Func, typename... ValuesToVisit>
decltype(auto) visitUnwrapUniquePtr(Func&& f, ValuesToVisit&&... values) noexcept(noexcept(
    std::visit(util::forwardUnwrappedUniquePtr(std::forward<Func>(f)), std::forward<ValuesToVisit>(values)...))) {
    return std::visit(util::forwardUnwrappedUniquePtr(std::forward<Func>(f)), std::forward<ValuesToVisit>(values)...);
}


//-----------------------------------------------------------------------------


template <typename To>
struct TranslateToMaybe {
    template <typename From, std::enable_if_t<!eckit::IsTranslatable<std::decay_t<From>, To>::value, bool> = true>
    std::optional<To> operator()(From&&) const noexcept {
        return std::nullopt;
    }

    template <typename From, std::enable_if_t<eckit::IsTranslatable<std::decay_t<From>, To>::value, bool> = true>
    std::optional<To> operator()(From&& from) const noexcept {
        return eckit::translate<To>(std::forward<From>(from));
    }
};


template <typename T, typename TypeToVisit>
decltype(auto) visitTranslate(TypeToVisit&& typeToVisit) noexcept {
    return std::visit(TranslateToMaybe<T>{}, std::forward<TypeToVisit>(typeToVisit));
}

//-----------------------------------------------------------------------------


}  // namespace multio::util


//-----------------------------------------------------------------------------

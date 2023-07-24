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


template <typename T, typename V>
inline constexpr size_t GetVariantIndex_v = GetVariantIndex<T, V>::value;


//-----------------------------------------------------------------------------

template <typename Arg,
          std::enable_if_t<(HasVariantBaseType_v<std::decay_t<Arg>> && !std::is_lvalue_reference_v<Arg>), bool> = true>
typename std::decay_t<Arg>::Base&& tryToVariantBase(Arg&& arg) noexcept {
    using Base = typename std::decay_t<Arg>::Base;
    return static_cast<Base&&>(std::forward<Arg>(arg));
}

template <typename Arg,
          std::enable_if_t<(HasVariantBaseType_v<std::decay_t<Arg>> && std::is_lvalue_reference_v<Arg>), bool> = true>
decltype(auto) tryToVariantBase(Arg&& arg) noexcept {
    using Base = typename std::decay_t<Arg>::Base;
    if constexpr (std::is_const_v<std::remove_reference_t<Arg>>) {
        return static_cast<Base const&>(arg);
    }
    else {
        return static_cast<Base&>(arg);
    }
}

template <typename Arg, std::enable_if_t<(!HasVariantBaseType_v<std::decay_t<Arg>>), bool> = true>
Arg&& tryToVariantBase(Arg&& arg) noexcept {
    return std::forward<Arg>(arg);
}


// Visit helper for derived classes - trying to avoid gcc9 - gcc11 bug:
// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=90943
template <typename Func, typename... ValuesToVisit>
decltype(auto) visit(Func&& f, ValuesToVisit&&... values) noexcept(
    noexcept(std::visit(std::forward<Func>(f), tryToVariantBase(std::forward<ValuesToVisit>(values))...))) {
    return std::visit(std::forward<Func>(f), tryToVariantBase(std::forward<ValuesToVisit>(values))...);
}


//-----------------------------------------------------------------------------

template <typename Func, typename... ValuesToVisit>
decltype(auto) visitUnwrapUniquePtr(Func&& f, ValuesToVisit&&... values) noexcept(noexcept(
    util::visit(util::forwardUnwrappedUniquePtr(std::forward<Func>(f)), std::forward<ValuesToVisit>(values)...))) {
    return util::visit(util::forwardUnwrappedUniquePtr(std::forward<Func>(f)), std::forward<ValuesToVisit>(values)...);
}


//-----------------------------------------------------------------------------


template <typename To>
struct TranslateToMaybe {
    template <typename From, std::enable_if_t<!eckit::IsTranslatable_v<std::decay_t<From>, To>, bool> = true>
    std::optional<To> operator()(From&&) const noexcept {
        return std::nullopt;
    }

    template <typename From, std::enable_if_t<eckit::IsTranslatable_v<std::decay_t<From>, To>, bool> = true>
    std::optional<To> operator()(From&& from) const noexcept {
        return eckit::translate<To>(std::forward<From>(from));
    }
};


template <typename T, typename TypeToVisit>
decltype(auto) visitTranslate(TypeToVisit&& typeToVisit) noexcept {
    return util::visit(TranslateToMaybe<T>{}, std::forward<TypeToVisit>(typeToVisit));
}

//-----------------------------------------------------------------------------


}  // namespace multio::util


//-----------------------------------------------------------------------------

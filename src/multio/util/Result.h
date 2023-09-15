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

/// @date August 2023


#pragma once

#include "multio/util/VariantHelpers.h"

#include <optional>

namespace multio::util {

struct ErrorMessage {
    std::string msg;
};

template <typename T, typename ErrType = ErrorMessage>
class Result : std::variant<T, ErrType> {
public:
    using This = Result<T, ErrType>;
    using Base = std::variant<T, ErrType>;
    using std::variant<T, ErrType>::variant;
    using std::variant<T, ErrType>::operator=;

    const T* operator->() const noexcept { return std::get_if<T>(static_cast<const Base*>(this)); }

    T* operator->() noexcept { return std::get_if<T>(static_cast<Base*>(this)); }


    const T& operator*() const& noexcept { return *std::get_if<T>(static_cast<const Base*>(this)); }

    T& operator*() & noexcept { return *std::get_if<T>(static_cast<Base*>(this)); }

    T&& operator*() && noexcept { return std::move(*std::get_if<T>(static_cast<Base*>(this))); }


    const T& value() const& { return std::get<T>(static_cast<const Base&>(*this)); }

    T& value() & { return std::get<T>(static_cast<Base&>(*this)); }

    T&& value() && { return std::move(std::get<T>(static_cast<Base&&>(*this))); }


    const ErrType& error() const& { return std::get<ErrType>(static_cast<const Base&>(*this)); }

    ErrType& error() & { return std::get<ErrType>(static_cast<Base&>(*this)); }

    ErrType&& error() && { return std::move(std::get<ErrType>(static_cast<Base&&>(*this))); }


    operator bool() const {
        return std::visit(
            Overloaded{[](const T&) -> bool { return true; }, [](const ErrType&) -> bool { return false; }},
            static_cast<const Base&>(*this));
    }

    std::optional<T> asOpt() const& {
        return std::visit(Overloaded{[](const T& v) -> std::optional<T> { return v; },
                                     [](const ErrType&) -> std::optional<T> { return std::nullopt; }},
                          *this);
    }

    std::optional<T> asOpt() & {
        return std::visit(Overloaded{[](T& v) -> std::optional<T> { return v; },
                                     [](ErrType&) -> std::optional<T> { return std::nullopt; }},
                          *this);
    }

    std::optional<T> asOpt() && {
        return std::visit(Overloaded{[](T&& v) -> std::optional<T> { return std::move(v); },
                                     [](ErrType&&) -> std::optional<T> { return std::nullopt; }},
                          *this);
    }

    template <typename HandleFunc>
    const T& valueOrHandleErr(HandleFunc&& func) const& noexcept(
        noexcept(std::invoke(std::forward<HandleFunc>(func), std::declval<const ErrType&>()))) {
        return std::visit(Overloaded{[](const T& v) -> const T& { return v; },
                                     [&func](const ErrType& err) -> const T& {
                                         return std::invoke(std::forward<HandleFunc>(func), err);
                                     }},
                          static_cast<const Base&>(*this));
    }

    template <typename HandleFunc>
    T& valueOrHandleErr(HandleFunc&& func) & noexcept(noexcept(std::invoke(std::forward<HandleFunc>(func),
                                                                           std::declval<ErrType&>()))) {
        return std::visit(
            Overloaded{[](T& v) -> T& { return v; },
                       [&func](ErrType& err) -> T& { return std::invoke(std::forward<HandleFunc>(func), err); }},
            static_cast<Base&>(*this));
    }

    template <typename HandleFunc>
    T valueOrHandleErr(HandleFunc&& func) && noexcept(noexcept(std::invoke(std::forward<HandleFunc>(func),
                                                                           std::declval<ErrType&&>()))) {
        return std::visit(
            Overloaded{[](T&& v) -> T { return v; },
                       [&func](ErrType&& err) -> T { return std::forward<HandleFunc>(func)(std::move(err)); }},
            static_cast<Base&&>(*this));
    }
};


template <typename TI, typename T, typename ErrType>
struct GetVariantIndex<TI, Result<T, ErrType>> : GetVariantIndex<TI, std::variant<T, ErrType>> {};

}  // namespace multio::util


namespace std {
template <typename T, typename ErrType>
struct variant_size<multio::util::Result<T, ErrType>> : variant_size<std::variant<T, ErrType>> {};

template <std::size_t I, typename T, typename ErrType>
struct variant_alternative<I, multio::util::Result<T, ErrType>> : variant_alternative<I, std::variant<T, ErrType>> {};
}  // namespace std

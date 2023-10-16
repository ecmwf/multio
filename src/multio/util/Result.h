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
            eckit::Overloaded{[](const T&) -> bool { return true; }, [](const ErrType&) -> bool { return false; }},
            static_cast<const Base&>(*this));
    }


    std::optional<T> asOpt() const& { return asOptImpl(*this); }
    std::optional<T> asOpt() & { return asOptImpl(*this); }
    std::optional<T> asOpt() && { return asOptImpl(std::move(*this)); }


    template <typename Func>
    auto transform(Func&& func) && {
        return transformImpl(std::move(*this), std::forward<Func>(func));
    }
    template <typename Func>
    auto transform(Func&& func) & {
        return transformImpl(*this, std::forward<Func>(func));
    }
    template <typename Func>
    auto transform(Func&& func) const& {
        return transformImpl(*this, std::forward<Func>(func));
    }


    template <typename Func>
    auto transformError(Func&& func) && {
        return transformErrorImpl(std::move(*this), std::forward<Func>(func));
    }
    template <typename Func>
    auto transformError(Func&& func) & {
        return transformErrorImpl(*this, std::forward<Func>(func));
    }
    template <typename Func>
    auto transformError(Func&& func) const& {
        return transformErrorImpl(*this, std::forward<Func>(func));
    }


    template <typename HandleFunc>
    const T& valueOrHandleErr(HandleFunc&& func) const& {
        return valueOrHandleErrImpl<const T&>(*this, std::forward<HandleFunc>(func));
    }

    template <typename HandleFunc>
    T& valueOrHandleErr(HandleFunc&& func) & noexcept(noexcept(std::invoke(std::forward<HandleFunc>(func),
                                                                           std::declval<ErrType&>()))) {
        return valueOrHandleErrImpl<T&>(*this, std::forward<HandleFunc>(func));
    }

    template <typename HandleFunc>
    T valueOrHandleErr(HandleFunc&& func) && noexcept(noexcept(std::invoke(std::forward<HandleFunc>(func),
                                                                           std::declval<ErrType&&>()))) {
        return valueOrHandleErrImpl<T>(std::move(*this), std::forward<HandleFunc>(func));
    }

private:
    // Implementation details
    template <typename This_>
    static std::optional<T> asOptImpl(This_&& t) {
        return std::visit(
            [](auto&& v) -> std::optional<T> {
                using VT = decltype(v);
                if constexpr (std::is_same_v<ErrType, std::decay_t<VT>>) {
                    return std::nullopt;
                }
                else {
                    return std::optional<T>{std::forward<VT>(v)};
                }
            },
            std::forward<This_>(t));
    }

    template <typename This_, typename Func>
    static auto transformImpl(This_&& t, Func&& f) {
        using Res = Result<std::decay_t<decltype(std::forward<Func>(f)(std::get<T>(std::forward<This_>(t))))>, ErrType>;
        return std::visit(
            [&](auto&& v) -> Res {
                using VT = decltype(v);
                if constexpr (std::is_same_v<ErrType, std::decay_t<VT>>) {
                    return Res{std::forward<VT>(v)};
                }
                else {
                    return Res{std::forward<Func>(f)(std::forward<decltype(v)>(v))};
                }
            },
            std::forward<This_>(t));
    }

    template <typename This_, typename Func>
    static auto transformErrorImpl(This_&& t, Func&& f) {
        using Res = Result<T, std::decay_t<decltype(std::forward<Func>(f)(std::get<ErrType>(std::forward<This_>(t))))>>;
        return std::visit(
            [&](auto&& v) -> Res {
                using VT = decltype(v);
                if constexpr (std::is_same_v<ErrType, std::decay_t<VT>>) {
                    return Res{std::forward<Func>(f)(std::forward<decltype(v)>(v))};
                }
                else {
                    return Res{std::forward<VT>(v)};
                }
            },
            std::forward<This_>(t));
    }

    template <typename Res, typename This_, typename HandleFunc>
    static Res valueOrHandleErrImpl(This_&& t,
                                    HandleFunc&& func) noexcept(noexcept(std::invoke(std::forward<HandleFunc>(func),
                                                                                     std::declval<ErrType&&>()))) {
        return std::visit(
            [&](auto&& v) -> Res {
                using VT = decltype(v);
                if constexpr (std::is_same_v<ErrType, std::decay_t<VT>>) {
                    return std::forward<HandleFunc>(func)(std::forward<VT>(v));
                }
                else {
                    return std::forward<VT>(v);
                }
            },
            std::forward<This_>(t));
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

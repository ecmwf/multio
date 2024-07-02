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

/// @date Nov 2022


/**
 * TODO: I've added these functionality to have more transparency in the GRIBEncoder action.
 *   Often metadata has to be extracted conditionally and different keys may be tested.
 *   Hence I try to construct `deferred look ups` and evaluate one after another.
 *   This reduces a lot of boilerplate code.
 */

#pragma once

#include "eckit/config/LocalConfiguration.h"

#include "multio/util/TypeTraits.h"
#include "multio/util/VariantHelpers.h"

#include <optional>
#include <string>


namespace eckit {
class Configuration;
class LocalConfiguration;
}  // namespace eckit

namespace multio::util {

//-----------------------------------------------------------------------------------------------------------------------------------------

// Idenitiy function for Optionals
template <typename T, std::enable_if_t<IsOptional_v<std::decay_t<T>>, bool> = true>
T&& evalToOptional(T&& opt) noexcept {
    return std::forward<T>(opt);
}

// Evaluate functions, assume to return optional
template <typename T, std::enable_if_t<(!(IsOptional_v<std::decay_t<T>>)), bool> = true>
auto evalToOptional(T&& call) noexcept(noexcept(std::forward<T>(call)())) {
    return std::forward<T>(call)();
}


// Idenitiy function for Optionals
template <typename T, std::enable_if_t<IsOptional_v<std::decay_t<T>>, bool> = true>
T&& makeOptional(T&& opt) noexcept {
    return std::forward<T>(opt);
}

// Evaluate functions, assume to return optional
template <typename T, std::enable_if_t<(!(IsOptional_v<std::decay_t<T>>)), bool> = true>
auto makeOptional(T&& noOpt) noexcept(noexcept(std::optional<std::decay_t<T>>{std::forward<T>(noOpt)})) {
    return std::optional<std::decay_t<T>>{std::forward<T>(noOpt)};
}


//-----------------------------------------------------------------------------------------------------------------------------------------

template <typename Res>
Res firstOfTyped() noexcept(noexcept(Res{})) {
    return Res{};
}

template <typename Res, typename T, typename... TS>
Res firstOfTyped(T&& m, TS&&... ts) noexcept(noexcept(evalToOptional(std::forward<T>(m)))
                                             && noexcept(firstOfTyped<Res>(std::forward<TS>(ts)...))) {
    auto em = evalToOptional(std::forward<T>(m));
    if (em) {
        // Return with RVO
        return em;
    }
    return firstOfTyped<Res>(std::forward<TS>(ts)...);
}

// Evaluates the arguments lazily and returns the first value containing a value
template <typename T, typename... TS>
auto firstOf(T&& m, TS&&... ts) noexcept(
    noexcept(firstOfTyped<decltype(evalToOptional(std::forward<T>(m)))>(std::forward<T>(m), std::forward<TS>(ts)...))) {
    return firstOfTyped<decltype(evalToOptional(std::forward<T>(m)))>(std::forward<T>(m), std::forward<TS>(ts)...);
}


//-----------------------------------------------------------------------------------------------------------------------------------------

template <typename ArgType, typename Func>
auto withFirstOfTyped(Func&& func) noexcept(noexcept(makeOptional(std::forward<Func>(func)(ArgType{})))) {
    return makeOptional(std::forward<Func>(func)(ArgType{}));
}
template <typename ArgType, typename Func, typename T, typename... TS>
auto withFirstOfTyped(Func&& func, T&& m, TS&&... ts) noexcept(
    noexcept(evalToOptional(std::forward<T>(m)))
    && noexcept(makeOptional(std::forward<Func>(func)(evalToOptional(std::forward<T>(m)))))
    && noexcept(withFirstOfTyped<ArgType>(std::forward<Func>(func), std::forward<TS>(ts)...))) {
    auto em = evalToOptional(std::forward<T>(m));
    if (em) {
        return makeOptional(std::forward<Func>(func)(std::move(em)));
    }
    else {
        return withFirstOfTyped<ArgType>(std::forward<Func>(func), std::forward<TS>(ts)...);
    }
}

// Evaluates the arguments lazily and calls the `func` with the result of the first argument containing a value
// I.e. Syntastic sugar to test a metadata object for a lot of keys (of different types) and pass the type to a setter
// function
template <typename OptFunc, typename T, typename... TS>
auto withFirstOf(OptFunc&& func, T&& m, TS&&... ts) noexcept(
    noexcept(bool(func))
    && noexcept(withFirstOfTyped<std::decay_t<decltype(evalToOptional(std::forward<T>(m)))>>(
        (*(std::forward<OptFunc>(func))), std::forward<T>(m), std::forward<TS>(ts)...))) {
    using ArgType = std::decay_t<decltype(evalToOptional(std::forward<T>(m)))>;
    using OptRetType = decltype(withFirstOfTyped<ArgType>((*(std::forward<OptFunc>(func))), std::forward<T>(m),
                                                          std::forward<TS>(ts)...));
    if (func) {
        return withFirstOfTyped<ArgType>((*(std::forward<OptFunc>(func))), std::forward<T>(m), std::forward<TS>(ts)...);
    }
    return OptRetType{};
}


//-----------------------------------------------------------------------------------------------------------------------------------------

// Perform a lazy lookup.
template <typename T, typename Container>
class LookUp {
private:
    const Container& c_;
    std::string key_;

public:
    LookUp(const Container& c, const std::string& key) : c_(c), key_(key) {};

    std::optional<T> operator()() const { return c_.template getOpt<T>(key_); }
};

template <typename T, typename Container>
LookUp<T, Container> lookUp(const Container& c, const std::string& k) noexcept {
    return LookUp<T, Container>{c, k};
};


template <typename T, typename Container>
class LookUpTranslate {
private:
    const Container& c_;
    std::string key_;

public:
    LookUpTranslate(const Container& c, const std::string& key) : c_(c), key_(key) {};

    std::optional<T> operator()() const { return util::visitTranslate<T>(c_.get(key_)); }
};


template <typename T, typename Container>
LookUpTranslate<T, Container> lookUpTranslate(const Container& c, const std::string& k) noexcept {
    return LookUpTranslate<T, Container>{c, k};
};


// Perform lookups
std::optional<std::int64_t> lookUpInt64(const eckit::Configuration& c, const std::string& key);
std::optional<std::string> lookUpString(const eckit::Configuration& c, const std::string& key);
std::optional<double> lookUpDouble(const eckit::Configuration& c, const std::string& key);
std::optional<float> lookUpFloat(const eckit::Configuration& c, const std::string& key);
std::optional<bool> lookUpBool(const eckit::Configuration& c, const std::string& key);

template <>
std::optional<std::int64_t> LookUp<std::int64_t, eckit::Configuration>::operator()() const {
    return lookUpInt64(c_, key_);
}
template <>
std::optional<std::string> LookUp<std::string, eckit::Configuration>::operator()() const {
    return lookUpString(c_, key_);
}
template <>
std::optional<double> LookUp<double, eckit::Configuration>::operator()() const {
    return lookUpDouble(c_, key_);
}
template <>
std::optional<float> LookUp<float, eckit::Configuration>::operator()() const {
    return lookUpFloat(c_, key_);
}
template <>
std::optional<bool> LookUp<bool, eckit::Configuration>::operator()() const {
    return lookUpBool(c_, key_);
}


template <>
std::optional<std::int64_t> LookUp<std::int64_t, eckit::LocalConfiguration>::operator()() const {
    return lookUpInt64(c_, key_);
}
template <>
std::optional<std::string> LookUp<std::string, eckit::LocalConfiguration>::operator()() const {
    return lookUpString(c_, key_);
}
template <>
std::optional<double> LookUp<double, eckit::LocalConfiguration>::operator()() const {
    return lookUpDouble(c_, key_);
}
template <>
std::optional<float> LookUp<float, eckit::LocalConfiguration>::operator()() const {
    return lookUpFloat(c_, key_);
}
template <>
std::optional<bool> LookUp<bool, eckit::LocalConfiguration>::operator()() const {
    return lookUpBool(c_, key_);
}


template <>
std::optional<std::int64_t> LookUpTranslate<std::int64_t, eckit::Configuration>::operator()() const {
    return lookUpInt64(c_, key_);
}
template <>
std::optional<std::string> LookUpTranslate<std::string, eckit::Configuration>::operator()() const {
    return lookUpString(c_, key_);
}
template <>
std::optional<double> LookUpTranslate<double, eckit::Configuration>::operator()() const {
    return lookUpDouble(c_, key_);
}
template <>
std::optional<float> LookUpTranslate<float, eckit::Configuration>::operator()() const {
    return lookUpFloat(c_, key_);
}
template <>
std::optional<bool> LookUpTranslate<bool, eckit::Configuration>::operator()() const {
    return lookUpBool(c_, key_);
}


template <>
std::optional<std::int64_t> LookUpTranslate<std::int64_t, eckit::LocalConfiguration>::operator()() const {
    return lookUpInt64(c_, key_);
}
template <>
std::optional<std::string> LookUpTranslate<std::string, eckit::LocalConfiguration>::operator()() const {
    return lookUpString(c_, key_);
}
template <>
std::optional<double> LookUpTranslate<double, eckit::LocalConfiguration>::operator()() const {
    return lookUpDouble(c_, key_);
}
template <>
std::optional<float> LookUpTranslate<float, eckit::LocalConfiguration>::operator()() const {
    return lookUpFloat(c_, key_);
}
template <>
std::optional<bool> LookUpTranslate<bool, eckit::LocalConfiguration>::operator()() const {
    return lookUpBool(c_, key_);
}

//-----------------------------------------------------------------------------------------------------------------------------------------

}  // namespace multio::util

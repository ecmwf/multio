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

#ifndef multio_util_Metadata_H
#define multio_util_Metadata_H

#include "eckit/utils/Optional.h"

namespace eckit {
class Configuration;
}

namespace multio {
namespace util {

//-----------------------------------------------------------------------------------------------------------------------------------------

template <typename T>
struct IsOptional {
    static constexpr bool value = false;
};
template <typename T>
struct IsOptional<eckit::Optional<T>> {
    static constexpr bool value = true;
};


//-----------------------------------------------------------------------------------------------------------------------------------------

// Idenitiy function for Optionals
template <typename T, typename enable = typename std::enable_if<IsOptional<typename std::decay<T>::type>::value>::type>
T evalOptional(T&& opt) {
    return std::forward<T>(opt);
}

// Evaluate functions, assume to return optional
template <typename T,
          typename enable = typename std::enable_if<(!(IsOptional<typename std::decay<T>::type>::value))>::type>
auto evalOptional(T&& call) -> decltype(std::forward<T>(call)()) {
    return std::forward<T>(call)();
}


//-----------------------------------------------------------------------------------------------------------------------------------------

template <typename Res>
Res firstOfTyped() {
    return Res{};
}

template <typename Res, typename T, typename... TS>
Res firstOfTyped(T&& m, TS&&... ts) {
    auto em = evalOptional(std::forward<T>(m));
    if (em) {
        return em;
    }
    return firstOfTyped<Res>(std::forward<TS>(ts)...);
}

// Evaluates the arguments lazily and returns the first value containing a value
template <typename T, typename... TS>
auto firstOf(T&& m, TS&&... ts) -> decltype(evalOptional(std::forward<T>(m))) {
    return firstOfTyped<decltype(evalOptional(std::forward<T>(m)))>(std::forward<T>(m), std::forward<TS>(ts)...);
}


//-----------------------------------------------------------------------------------------------------------------------------------------

template <typename Res, typename ArgType, typename Func>
Res withFirstOfTyped(Func&& func) {
    return std::forward<Func>(func)(ArgType{});
}
template <typename Res, typename ArgType, typename Func, typename T, typename... TS>
Res withFirstOfTyped(Func&& func, T&& m, TS&&... ts) {
    auto em = evalOptional(std::forward<T>(m));
    if (em) {
        return std::forward<Func>(func)(std::move(em));
    }
    else {
        return withFirstOfTyped<Res, ArgType>(std::forward<Func>(func), std::forward<TS>(ts)...);
    }
}

// Evaluates the arguments lazily and calls the `func` with the result of the first argument containing a value
// I.e. Syntastic sugar to test a metadata object for a lot of keys (of different types) and pass the type to a setter
// function
template <typename Func, typename T, typename... TS>
auto withFirstOf(Func&& func, T&& m, TS&&... ts)
    -> decltype(std::forward<Func>(func)(evalOptional(std::forward<T>(m)))) {
    using ArgType = typename std::decay<decltype(evalOptional(std::forward<T>(m)))>::type;
    using Ret = decltype(std::forward<Func>(func)(evalOptional(std::forward<T>(m))));
    return withFirstOfTyped<Ret, ArgType>(std::forward<Func>(func), std::forward<T>(m), std::forward<TS>(ts)...);
}


//-----------------------------------------------------------------------------------------------------------------------------------------

// Perform a lazy lookup.
template <typename T>
class LookUp {
private:
    const eckit::Configuration& c_;
    std::string key_;

public:
    LookUp(const eckit::Configuration& c, const std::string& key) : c_(c), key_(key){};

    eckit::Optional<T> operator()() const;
};

using LookUpLong = LookUp<long>;
using LookUpString = LookUp<std::string>;
using LookUpDouble = LookUp<double>;
using LookUpFloat = LookUp<float>;
using LookUpBool = LookUp<bool>;

// Perform lookups
eckit::Optional<long> lookUpLong(const eckit::Configuration& c, const std::string& key);
eckit::Optional<std::string> lookUpString(const eckit::Configuration& c, const std::string& key);
eckit::Optional<double> lookUpDouble(const eckit::Configuration& c, const std::string& key);
eckit::Optional<float> lookUpFloat(const eckit::Configuration& c, const std::string& key);
eckit::Optional<bool> lookUpBool(const eckit::Configuration& c, const std::string& key);


}  // namespace util
}  // namespace multio

#endif

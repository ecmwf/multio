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

#include "multio/datamod/MarsTypes.h"
#include "multio/datamod/core/TypeParserDumper.h"

#include "multio/util/Hash.h"
#include "multio/util/Print.h"
#include "multio/util/TypeToString.h"

#include <chrono>
#include <string>
#include <variant>


namespace multio::datamod {

//-----------------------------------------------------------------------------


using TimeDurationVariant = std::variant<std::chrono::hours, std::chrono::seconds>;

// Time duration will get a proper type because printing it has a special domain specific meaning
// that goes beyond chrono::hours and chrono::seconds
class TimeDuration : public TimeDurationVariant {
public:
    using TimeDurationVariant::TimeDurationVariant;
    using TimeDurationVariant::operator=;

    TimeDuration(const TimeDuration& other) = default;
    TimeDuration(TimeDuration&&) noexcept = default;

    TimeDuration& operator=(const TimeDuration& other) = default;
    TimeDuration& operator=(TimeDuration&& other) noexcept = default;

    template <typename Func>
    decltype(auto) visit(Func&& func) const {
        return std::visit(std::forward<Func>(func), static_cast<const TimeDurationVariant&>(*this));
    }
    template <typename Func>
    decltype(auto) visit(Func&& func) {
        return std::visit(std::forward<Func>(func), static_cast<TimeDurationVariant&>(*this));
    }
};

}  // namespace multio::datamod


namespace std {
template <>
struct variant_size<multio::datamod::TimeDuration> : variant_size<multio::datamod::TimeDurationVariant> {};

template <std::size_t I>
struct variant_alternative<I, multio::datamod::TimeDuration>
    : variant_alternative<I, multio::datamod::TimeDurationVariant> {};
}  // namespace std

//-----------------------------------------------------------------------------

namespace multio::datamod {

// TODO Represent origin as enum and add mapping from stringified origin to enum with int values
using IntOrString = std::variant<std::int64_t, std::string>;


// To be renamed and kept internal -
enum class Repres : std::size_t
{
    GG,
    LL,
    SH,
    HEALPix  // We added it here because we use repres as an intermediate type. Officially healpix is not mapped to any
             // of the others...
};


// To be renamed and kept internal -
enum class LevType : std::size_t
{
    ML,
    PL,
    PV,
    PT,
    SOL,
    SFC,
    O2D,
    O3D,
    HL,
    HHL,
    HPL,
    AL
};

const std::vector<LevType>& allLevTypes();
}  // namespace multio::datamod


namespace multio::util {

template <>
struct Print<datamod::TimeDuration> {
    static void print(PrintStream& ps, const datamod::TimeDuration& v);
};

template <>
struct Print<datamod::Repres> {
    static void print(PrintStream& ps, const datamod::Repres& v);
};

template <>
struct Print<datamod::LevType> {
    static void print(PrintStream& ps, const datamod::LevType& v);
};


template <>
struct TypeToString<datamod::TimeDuration> {
    std::string operator()() const { return "datamod::TimeDuration"; };
};
template <>
struct TypeToString<datamod::Repres> {
    std::string operator()() const { return "datamod::Repres"; };
};
template <>
struct TypeToString<datamod::LevType> {
    std::string operator()() const { return "datamod::LevType"; };
};
}  // namespace multio::util

namespace multio::datamod {

template <>
struct DumpType<TimeDuration> {
    static std::variant<std::int64_t, std::string> dump(const TimeDuration&);
};


template <>
struct ParseType<TimeDuration> {
    static TimeDuration parse(std::int64_t hours) noexcept;
    static TimeDuration parse(const std::string& s);
};


template <>
struct DumpType<Repres> {
    static std::string dump(Repres);
};

template <>
struct ParseType<Repres> {
    static inline Repres parse(Repres v) noexcept { return v; };
    static Repres parse(const std::string& s);
};


Repres represFromGrid(const std::string& grid);

template <>
struct DumpType<LevType> {
    static std::string dump(LevType);
};

template <>
struct ParseType<LevType> {
    static inline LevType parse(LevType v) noexcept { return v; };
    static LevType parse(const std::string& s);
};


namespace mapper {

struct StringToIntMapper {
    static inline std::int64_t dump(std::int64_t v) noexcept { return v; };
    static inline std::int64_t parse(std::int64_t v) noexcept { return v; };
    static std::int64_t parse(const std::string& v); 
};

// TODO Discuss ith Param should get it's own type ParamId (wrapping an int..)
// Currently `metkit::Param` is used to create a paramId from string
// There is also the existing type `metkit::ParamID` which (unfortunately) can not be constructed from an eisting int.
struct ParamMapper {
    static std::int64_t dump(std::int64_t) noexcept;
    static std::int64_t parse(std::int64_t) noexcept;
    static std::int64_t parse(const std::string&);
};

struct BoolMapper {
    static inline bool dump(bool v) noexcept { return v; };
    static inline bool parse(bool v) noexcept { return v; };
    static inline bool parse(std::int64_t v) { return v > 0; };
    static bool parse(const std::string& v);
};

}  // namespace mapper

}  // namespace multio::datamod


template <>
struct std::hash<multio::datamod::TimeDuration> {
    std::size_t operator()(const multio::datamod::TimeDuration& td) const noexcept {
        return td.visit(eckit::Overloaded{
            [&](const std::chrono::hours& h) { return multio::util::hashCombine(h.count(), 'h'); },
            [&](const std::chrono::seconds& s) { return multio::util::hashCombine(s.count(), 's'); }});
    }
};

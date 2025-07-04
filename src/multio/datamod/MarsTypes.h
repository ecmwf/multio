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

#include "multio/datamod/ReaderWriter.h"
#include "multio/util/Hash.h"
#include "multio/util/TypeTraits.h"
#include "multio/util/TypeToString.h"
#include "multio/util/VariantHelpers.h"

#include <chrono>
#include <sstream>
#include <string>


namespace multio::datamod {

//-----------------------------------------------------------------------------


using TimeDuration = std::variant<std::chrono::hours, std::chrono::seconds>;
std::ostream& operator<<(std::ostream&, const TimeDuration&);


// To be renamed and kept internal - 
enum class Repres : std::size_t
{
    GG,
    LL,
    SH,
    HEALPix  // We added it here because we use repres as an intermediate type. Officially healpix is not mapped to any
             // of the others...
};

std::ostream& operator<<(std::ostream&, const Repres&);


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

std::ostream& operator<<(std::ostream&, const LevType&);
}  // namespace multio::datamod


namespace multio::util {
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
struct WriteSpec<TimeDuration> {
    static std::string write(const TimeDuration&);
};

template <>
struct ReadSpec<TimeDuration> {
    static TimeDuration read(std::int64_t hours) noexcept;
    static TimeDuration read(const std::string& s);
};


template <>
struct WriteSpec<Repres> {
    static std::string write(Repres);
};

template <>
struct ReadSpec<Repres> {
    static inline Repres read(Repres v) noexcept { return v; };
    static Repres read(const std::string& s);
};


Repres represFromGrid(const std::string& grid);

template <>
struct WriteSpec<LevType> {
    static std::string write(LevType);
};

template <>
struct ReadSpec<LevType> {
    static inline LevType read(LevType v) noexcept { return v; };
    static LevType read(const std::string& s);
};


namespace mapper {

// TODO Discuss ith Param should get it's own type ParamId (wrapping an int..)
// Currently `metkit::Param` is used to create a paramId from string
// There is also the existing type `metkit::ParamID` which (unfortunately) can not be constructed from an eisting int.
struct ParamMapper {
    static std::int64_t write(std::int64_t) noexcept;
    static std::int64_t read(std::int64_t) noexcept;
    static std::int64_t read(const std::string&);
};

struct IntToBoolMapper {
    static inline bool write(bool v) noexcept { return v; };
    static inline bool read(bool v) noexcept { return v; };
    static inline bool read(std::int64_t v) { return v > 0; };
};

}  // namespace mapper

}  // namespace multio::datamod


template <>
struct std::hash<multio::datamod::TimeDuration> {
    std::size_t operator()(const multio::datamod::TimeDuration& td) const noexcept {
        return std::visit(
            eckit::Overloaded{[&](const std::chrono::hours& h) { return multio::util::hashCombine(h.count(), 'h'); },
                              [&](const std::chrono::seconds& s) { return multio::util::hashCombine(s.count(), 's'); }},
            td);
    }
};

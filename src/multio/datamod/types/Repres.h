/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#pragma once

#include "multio/datamod/core/TypeParserDumper.h"

#include "multio/util/Print.h"
#include "multio/util/TypeToString.h"

#include <string>


namespace multio::datamod {

//-----------------------------------------------------------------------------

// To be renamed and kept internal -
enum class Repres : std::size_t
{
    GG,
    LL,
    SH,
    HEALPix  // We added it here because we use repres as an intermediate type. Officially healpix is not mapped to any
             // of the others...
};

}  // namespace multio::datamod


namespace multio::util {

template <>
struct Print<datamod::Repres> {
    static void print(PrintStream& ps, const datamod::Repres& v);
};

template <>
struct TypeToString<datamod::Repres> {
    std::string operator()() const { return "datamod::Repres"; };
};

}  // namespace multio::util

namespace multio::datamod {

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

}  // namespace multio::datamod


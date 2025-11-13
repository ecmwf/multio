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


namespace multio::datamod {

//----------------------------------------------------------------------------------------------------------------------


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
    AL,
    WV,
    DP,
    CAT,
    LAYER
};

const std::vector<LevType>& allLevTypes();

}  // namespace multio::datamod


template <>
struct multio::util::Print<multio::datamod::LevType> {
    static void print(PrintStream& ps, const datamod::LevType& v);
};

template <>
struct multio::datamod::DumpType<multio::datamod::LevType> {
    static std::string dump(LevType);
};

template <>
struct multio::datamod::ParseType<multio::datamod::LevType> {
    static LevType parse(const std::string& s);
};


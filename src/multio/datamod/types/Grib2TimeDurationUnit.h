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

#include "metkit/codes/api/CodesAPI.h"

namespace multio::datamod {

//----------------------------------------------------------------------------------------------------------------------

// IndicatorOfUnit* defined with official integer representation in GRIB 4.4 code table
// https://codes.ecmwf.int/grib/format/grib2/ctables/4/4/
enum class Grib2TimeDurationUnit : std::int64_t
{
    Minute = 0,    // String: "m"
    Hour = 1,      // String: "h"
    Day = 2,       // String: "D"
    Month = 3,     // String: "M"
    Year = 4,      // String: "Y"
    Years10 = 5,   // String: "10Y"
    Years30 = 6,   // String: "30Y"
    Years100 = 7,  // String: "C"
    Hours3 = 10,   // String: "3h"
    Hours6 = 11,   // String: "6h"
    Hours12 = 12,  // String: "12h"
    Second = 13,   // String: "s"
};

}  // namespace multio::datamod


namespace multio::util {

template <>
struct Print<datamod::Grib2TimeDurationUnit> {
    static void print(PrintStream&, const datamod::Grib2TimeDurationUnit&);
};

}  // namespace multio::util


namespace multio::datamod {

template <>
struct DumpType<Grib2TimeDurationUnit> {
    static std::string dump(Grib2TimeDurationUnit);
};

template <>
struct DumpType<Grib2TimeDurationUnit, metkit::codes::CodesHandle> {
    static std::int64_t dump(Grib2TimeDurationUnit);
};

template <>
struct ParseType<Grib2TimeDurationUnit> {
    static Grib2TimeDurationUnit parse(const std::string& s);
    static Grib2TimeDurationUnit parse(std::int64_t i);
};


}  // namespace multio::datamod

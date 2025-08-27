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


namespace multio::datamod {

//-----------------------------------------------------------------------------

// TypeOfStatisticalProcessing defined with official integer representation in GRIB 4.10 code table
// https://codes.ecmwf.int/grib/format/grib2/ctables/4/10/
enum class TypeOfStatisticalProcessing : std::int64_t
{
    Average = 0,
    Accumulation = 1,
    Maximum = 2,
    Minimum = 3,
    Difference = 4,
    RootMeanSquare = 5,
    StandardDeviation = 6,
    Covariance = 7,
    InverseDifference = 8,
    Ratio = 9,
    StandardizedAnomaly = 10,
    Summation = 11,
    ReturnPeriod = 12,
    Median = 13,
    Severity = 100,
    Mode = 101,
    IndexProcessing = 102,
};

}  // namespace multio::datamod


namespace multio::util {

template <>
struct Print<datamod::TypeOfStatisticalProcessing> {
    static void print(PrintStream&, const datamod::TypeOfStatisticalProcessing&);
};

template <>
struct TypeToString<datamod::TypeOfStatisticalProcessing> {
    std::string operator()() const { return "datamod::TypeOfStatisticalProcessing"; };
};
}  // namespace multio::util


namespace multio::datamod {

template <>
struct DumpType<TypeOfStatisticalProcessing> {
    static std::string dump(TypeOfStatisticalProcessing);
};

// TBD - add dump spec for writing to grib (dump as int)
// template <>
// struct DumpType<TypeOfStatisticalProcessing, util::MioGribHandle> {
//     static std::int64_t dump(TypeOfStatisticalProcessing);
// };

template <>
struct ParseType<TypeOfStatisticalProcessing> {
    static inline TypeOfStatisticalProcessing parse(TypeOfStatisticalProcessing v) noexcept { return v; };
    static TypeOfStatisticalProcessing parse(const std::string& s);
    static TypeOfStatisticalProcessing parse(std::int64_t i);
};


}  // namespace multio::datamod


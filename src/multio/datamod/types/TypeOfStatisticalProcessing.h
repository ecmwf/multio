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


template <>
struct multio::util::Print<multio::datamod::TypeOfStatisticalProcessing> {
    static void print(PrintStream&, const datamod::TypeOfStatisticalProcessing&);
};


namespace multio::datamod {

template <>
struct DumpType<TypeOfStatisticalProcessing> {
    static std::string dump(TypeOfStatisticalProcessing);
};

// TBD - add dump spec for writing to grib (dump as int)
// template <>
// struct DumpType<TypeOfStatisticalProcessing, metkit::codes::CodesHandle> {
//     static std::int64_t dump(TypeOfStatisticalProcessing);
// };

template <>
struct ParseType<TypeOfStatisticalProcessing> {
    static TypeOfStatisticalProcessing parse(const std::string& s);
    static TypeOfStatisticalProcessing parse(std::int64_t i);
};


}  // namespace multio::datamod

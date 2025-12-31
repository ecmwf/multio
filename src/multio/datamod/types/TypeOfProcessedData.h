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

// TypeOfProcessedData defined with official integer representation in GRIB 1.4 code table
// https://codes.ecmwf.int/grib/format/grib2/ctables/1/4
enum class TypeOfProcessedData : int64_t
{
    AnalysisProducts = 0,                     // an
    ForecastProducts = 1,                     // fc
    AnalysisAndForecastProducts = 2,          // af
    ControlForecastProducts = 3,              // cf
    PerturbedForecastProducts = 4,            // pf
    ControlAndPerturbedForecastProducts = 5,  // cp
    ProcessedSatelliteObservations = 6,       // sa
    ProcessedRadarObservations = 7,           // ra
    EventProbability = 8,                     // ep
};

}  // namespace multio::datamod


template <>
struct multio::util::Print<multio::datamod::TypeOfProcessedData> {
    static void print(PrintStream&, const datamod::TypeOfProcessedData&);
};


namespace multio::datamod {

template <>
struct DumpType<TypeOfProcessedData> {
    static std::string dump(TypeOfProcessedData);
};

template <>
struct DumpType<TypeOfProcessedData, metkit::codes::CodesHandle> {
    static int64_t dump(TypeOfProcessedData);
};

template <>
struct ParseType<TypeOfProcessedData> {
    static TypeOfProcessedData parse(const std::string& s);
    static TypeOfProcessedData parse(int64_t i);
};


}  // namespace multio::datamod

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

#include "multio/datamod/core/TypeParserDumper.h"
#include "multio/util/Hash.h"
#include "multio/util/MioGribHandle.h"
#include "multio/util/TypeToString.h"
#include "multio/util/TypeTraits.h"
#include "multio/util/VariantHelpers.h"

#include <chrono>
#include <sstream>
#include <string>


namespace multio::mars2grib::sections {

//-----------------------------------------------------------------------------


// Internal enum only
enum class TimeRangeType : std::size_t
{
    FixedTimeRange,
    SinceLastPostProcessingStep,
    SinceBeginningOfForecast
};

std::ostream& operator<<(std::ostream&, const TimeRangeType&);

}  // namespace multio::mars2grib::sections


namespace multio::util {
template <>
struct TypeToString<mars2grib::sections::TimeRangeType> {
    std::string operator()() const { return "datamod::TimeRangeType"; };
};
}  // namespace multio::util

namespace multio::datamod {

template <>
struct DumpType<mars2grib::sections::TimeRangeType> {
    static std::string dump(mars2grib::sections::TimeRangeType);
};


template <>
struct ParseType<mars2grib::sections::TimeRangeType> {
    static inline mars2grib::sections::TimeRangeType parse(mars2grib::sections::TimeRangeType v) noexcept { return v; };
    static mars2grib::sections::TimeRangeType parse(const std::string& s);
};


}  // namespace multio::datamod


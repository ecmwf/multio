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
#include "multio/util/MioGribHandle.h"
#include "multio/util/TypeToString.h"
#include "multio/util/TypeTraits.h"
#include "multio/util/VariantHelpers.h"

#include <chrono>
#include <sstream>
#include <string>


namespace multio::action::sections {

//-----------------------------------------------------------------------------


// Internal enum only
enum class TimeRangeType : std::size_t
{
    FixedTimeRange,
    SinceLastPostProcessingStep,
    SinceBeginningOfForecast
};

std::ostream& operator<<(std::ostream&, const TimeRangeType&);

}  // namespace multio::action::sections


namespace multio::util {
template <>
struct TypeToString<action::sections::TimeRangeType> {
    std::string operator()() const { return "datamod::TimeRangeType"; };
};
}  // namespace multio::util

namespace multio::datamod {

template <>
struct WriteSpec<action::sections::TimeRangeType> {
    static std::string write(action::sections::TimeRangeType);
};


template <>
struct ReadSpec<action::sections::TimeRangeType> {
    static inline action::sections::TimeRangeType read(action::sections::TimeRangeType v) noexcept { return v; };
    static action::sections::TimeRangeType read(const std::string& s);
};


}  // namespace multio::datamod


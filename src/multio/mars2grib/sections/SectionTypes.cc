/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "multio/mars2grib/sections/SectionTypes.h"
#include "multio/mars2grib/Mars2GribException.h"

namespace multio::datamod {

using namespace multio::mars2grib::sections;

std::string DumpType<TimeRangeType>::dump(TimeRangeType v) {
    switch (v) {
        case TimeRangeType::FixedTimeRange:
            return "fixed-timerange";
        case TimeRangeType::SinceLastPostProcessingStep:
            return "since-last-post-processing-step";
        case TimeRangeType::SinceBeginningOfForecast:
            return "since-beginning-of-forecast";
        default:
            throw multio::mars2grib::Mars2GribException(
                "DumpType<TimeRangeType>::dump: Unexpected value for TimeRangeType", Here());
    }
}


TimeRangeType ParseType<TimeRangeType>::parse(const std::string& val) {
    // May use a vector
    static const std::vector<std::pair<std::string, TimeRangeType>> typesOfStat{
        {"fixed-timerange", TimeRangeType::FixedTimeRange},
        {"since-last-post-processing-step", TimeRangeType::SinceLastPostProcessingStep},
        {"since-beginning-of-forecast", TimeRangeType::SinceBeginningOfForecast}};

    if (auto tos
        = std::find_if(typesOfStat.begin(), typesOfStat.end(), [&](const auto& pair) { return val == pair.first; });
        tos != typesOfStat.end()) {
        return tos->second;
    }
    throw multio::mars2grib::Mars2GribException(
        std::string("ParseType<TimeRangeType>::parse Unknown value for TimeRangeType: ") + val, Here());
}


}  // namespace multio::datamod

namespace multio::mars2grib::sections {
std::ostream& operator<<(std::ostream& os, const TimeRangeType& t) {
    os << datamod::TypeDumper<TimeRangeType>::dump(t);
    return os;
}
}  // namespace multio::mars2grib::sections


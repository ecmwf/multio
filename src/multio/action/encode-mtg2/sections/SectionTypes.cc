/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "multio/action/encode-mtg2/sections/SectionTypes.h"
#include "multio/action/encode-mtg2/EncodeMtg2Exception.h"

namespace multio::datamod {

using namespace multio::action::sections;

std::string WriteSpec<TimeRangeType>::write(TimeRangeType v) {
    switch (v) {
        case TimeRangeType::FixedTimeRange:
            return "fixed-timerange";
        case TimeRangeType::SinceLastPostProcessingStep:
            return "since-last-post-processing-step";
        case TimeRangeType::SinceBeginningOfForecast:
            return "since-beginning-of-forecast";
        default:
            throw action::EncodeMtg2Exception("WriteSpec<TimeRangeType>::write: Unexpected value for TimeRangeType",
                                              Here());
    }
}


TimeRangeType ReadSpec<TimeRangeType>::read(const std::string& val) {
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
    throw action::EncodeMtg2Exception(
        std::string("ReadSpec<TimeRangeType>::read Unknown value for TimeRangeType: ") + val, Here());
}


}  // namespace multio::datamod

namespace multio::action::sections {
std::ostream& operator<<(std::ostream& os, const TimeRangeType& t) {
    os << datamod::Writer<TimeRangeType>::write(t);
    return os;
}
}  // namespace multio::action::sections


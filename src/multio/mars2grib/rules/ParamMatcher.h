/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#pragma once

#include "multio/datamod/MarsMiscGeo.h"
#include "multio/mars2grib/rules/Matcher.h"

#include <vector>

namespace multio::mars2grib::matcher {

//-----------------------------------------------------------------------------
// Param matchers
//-----------------------------------------------------------------------------

auto paramRange(dm::EntryValueType_t<decltype(dm::PARAM)> start,
                dm::EntryValueType_t<decltype(dm::PARAM)> end) {
    return Range{dm::PARAM, start, end};
}

using ParamMatcher = Any<OneOf<std::decay_t<decltype(dm::PARAM)>>, Ranges<std::decay_t<decltype(dm::PARAM)>>>;

ParamMatcher matchParams(std::vector<dm::EntryValueType_t<decltype(dm::PARAM)>> params) {
    return ParamMatcher{
        std::make_tuple(OneOf{dm::PARAM, std::move(params)}, Ranges<std::decay_t<decltype(dm::PARAM)>>{{}})};
}

ParamMatcher matchParams(dm::EntryValueType_t<decltype(dm::PARAM)> param) {
    return matchParams(std::vector{param});
}

ParamMatcher matchParams(Range<std::decay_t<decltype(dm::PARAM)>> range) {
    return ParamMatcher{std::make_tuple(OneOf{dm::PARAM, {}}, Ranges<std::decay_t<decltype(dm::PARAM)>>{{range}})};
}

ParamMatcher matchParams(ParamMatcher m) {
    return m;
}

ParamMatcher matchParams(ParamMatcher m1, ParamMatcher&& m2) {
    auto& m1Vec = std::get<0>(m1.matchers).values;
    auto& m2Vec = std::get<0>(m2.matchers).values;

    m1Vec.insert(m1Vec.end(), std::make_move_iterator(m2Vec.begin()), std::make_move_iterator(m2Vec.end()));


    auto& m1Ranges = std::get<1>(m1.matchers).ranges;
    auto& m2Ranges = std::get<1>(m2.matchers).ranges;

    m1Ranges.insert(m1Ranges.end(), std::make_move_iterator(m2Ranges.begin()), std::make_move_iterator(m2Ranges.end()));

    return m1;
}

template <typename Arg, typename Arg2, typename... More>
ParamMatcher matchParams(Arg&& arg, Arg2&& arg2, More&&... more) {
    return matchParams(matchParams(matchParams(std::forward<Arg>(arg)), matchParams(std::forward<Arg2>(arg2))),
                       std::forward<More>(more)...);
}


}  // namespace multio::mars2grib::matcher


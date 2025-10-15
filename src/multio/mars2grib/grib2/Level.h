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

#include <cstdint>
#include "multio/datamod/MarsMiscGeo.h"

#include "multio/datamod/core/EntryDef.h"
#include "multio/datamod/core/Print.h"
#include "multio/datamod/types/TypeOfLevel.h"

// Level config
namespace multio::mars2grib::grib2 {

namespace dm = multio::datamod;

struct LevelKeys {
    dm::Entry<dm::TypeOfLevel> typeOfLevel;
    dm::Entry<std::int64_t> level;

    static constexpr std::string_view record_name_ = "level-keys";
    static constexpr auto record_entries_ = std::make_tuple(             //
        entryDef("typeOfLevel", &LevelKeys::typeOfLevel).tagOptional(),  //
        entryDef("level", &LevelKeys::level).tagOptional());
};

struct VerticalKeys {
    dm::Entry<bool> pvPresent;
    dm::Entry<std::vector<double>> pv;

    static constexpr std::string_view record_name_ = "vertical-keys";
    static constexpr auto record_entries_ = std::make_tuple(            //
        entryDef("PVPresent", &VerticalKeys::pvPresent).tagOptional(),  //
        entryDef("pv", &VerticalKeys::pv).tagOptional());
};


// Determines the level and whether a level has to be set for a typeOfLevel
LevelKeys setLevel(dm::TypeOfLevel, std::optional<std::int64_t> fixedLevel, const dm::FullMarsRecord&);

std::optional<VerticalKeys> setVertical(dm::TypeOfLevel, const dm::FullMarsRecord&, const dm::MiscRecord&);


}  // namespace multio::mars2grib::grib2

namespace multio::util {
template <>
struct Print<multio::mars2grib::grib2::LevelKeys> : multio::datamod::PrintRecord {};
template <>
struct Print<multio::mars2grib::grib2::VerticalKeys> : multio::datamod::PrintRecord {};
};  // namespace multio::util

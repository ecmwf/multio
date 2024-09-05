/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Philipp Geier

/// @date Aug 2024


#pragma once

#include <algorithm>
#include <cstdint>
#include <optional>
#include <string>

namespace multio::util {

//-----------------------------------------------------------------------------

enum Grib2TimeUnit : std::int64_t {
     Minute = 0,
     Hour = 1,
     Day = 2,
     Month = 3,
     Year = 4,
     Decade = 5,  // Decade (10 years)
     Y30 = 6,  // Normal (30 years)
     Century = 7,  //  Century (100 years)
     H3 = 10, // 3 hours
     H6 = 11, // 6 hours
     H12 = 12, // 12 hours
     Second = 13,
     Missing = 255,
};

std::int64_t grib2TimeUnitToSeconds(Grib2TimeUnit);

// Optimize helper function because ifs steps are naturally in hours
std::int64_t grib2TimeUnitToHours(Grib2TimeUnit);


//-----------------------------------------------------------------------------

}  // namespace multio::util

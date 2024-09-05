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

/// @date Aug 2023

#include "multio/util/DateTime.h"

#include "eckit/types/Date.h"
#include "eckit/types/DateTime.h"
#include "eckit/types/Time.h"

#include <chrono>
#include <cmath>


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

std::int64_t grib2TimeUnitToSeconds(Grib2TimeUnit u) {
    switch(u) {
         case Grib2TimeUnit::Minute:
            return 60;
         case Grib2TimeUnit::Hour:
            return 3600;
         case Grib2TimeUnit::Day:
            return 3600*24;
         case Grib2TimeUnit::Month:
            NOTIMP;
            break;
         case Grib2TimeUnit::Year:
            NOTIMP;
            break;
         case Grib2TimeUnit::Decade:
            NOTIMP;
            break;
         case Grib2TimeUnit::Y30:
            NOTIMP;
            break;
         case Grib2TimeUnit::Century:
            NOTIMP;
            break;
         case Grib2TimeUnit::H3:
            return 3600*3;
         case Grib2TimeUnit::H6:
            return 3600*6;
         case Grib2TimeUnit::H12:
            return 3600*12;
         case Grib2TimeUnit::Second:
            return 1;
         case Grib2TimeUnit::Missing:
            NOTIMP;
            break;
    }
    return 0;
}

std::int64_t grib2TimeUnitToHours(Grib2TimeUnit u) {
    switch(u) {
         case Grib2TimeUnit::Minute:
            NOTIMP;
            break;
         case Grib2TimeUnit::Hour:
            return 1;
         case Grib2TimeUnit::Day:
            return 24;
         case Grib2TimeUnit::Month:
            NOTIMP;
            break;
         case Grib2TimeUnit::Year:
            NOTIMP;
            break;
         case Grib2TimeUnit::Decade:
            NOTIMP;
            break;
         case Grib2TimeUnit::Y30:
            NOTIMP;
            break;
         case Grib2TimeUnit::Century:
            NOTIMP;
            break;
         case Grib2TimeUnit::H3:
            return 3;
         case Grib2TimeUnit::H6:
            return 6;
         case Grib2TimeUnit::H12:
            return 12;
         case Grib2TimeUnit::Second:
            NOTIMP;
            break;
         case Grib2TimeUnit::Missing:
            NOTIMP;
            break;
    }
    return 0;
}

//-----------------------------------------------------------------------------

}  // namespace multio::util

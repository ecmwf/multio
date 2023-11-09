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

DateInts toDateInts(std::int64_t date) noexcept {
    std::int64_t y = date * 1e-4;
    std::int64_t rem = date - (y * 1e4);
    std::int64_t m = rem * 1e-2;
    std::int64_t d = rem - (m * 1e2);
    return {y, m, d};
}


TimeInts toTimeInts(std::int64_t time) noexcept {
    std::int64_t h = time * 1e-4;
    std::int64_t rem = time - (h * 1e4);
    std::int64_t m = rem * 1e-2;
    std::int64_t s = rem - (m * 1e2);
    return {h, m, s};
}


std::int64_t fromDateInts(const DateInts& ints) noexcept {
    return ints.year * 10000 + ints.month * 100 + ints.day;
};


std::int64_t fromTimeInts(const TimeInts& ints) noexcept {
    return ints.hour * 10000 + ints.minute * 100 + ints.second;
};

DateTimeInts normalizeDateTime(DateTimeInts in) {
    if (in.time.hour >= 24 || in.time.minute >= 60 || in.time.second >= 60) {
        eckit::DateTime normalized
            = eckit::DateTime{eckit::Date{in.date.year, in.date.month, in.date.day}, eckit::Time{0, 0, 0}}
            + ((double)(((in.time.hour * 60) + in.time.minute) * 60 + in.time.second));

        return {toDateInts(normalized.date().yyyymmdd()), toTimeInts(normalized.time().hhmmss())};
    }
    return in;
};

//-----------------------------------------------------------------------------

std::optional<TimeUnit> timeUnitFromChar(char c) noexcept {
    switch (c) {
        // case 'Y':
        case ((char)TimeUnit::Year):
            return std::optional<TimeUnit>{TimeUnit::Year};
        // case 'm':
        case ((char)TimeUnit::Month):
            return std::optional<TimeUnit>{TimeUnit::Month};
        // case 'd':
        case ((char)TimeUnit::Day):
            return std::optional<TimeUnit>{TimeUnit::Day};
        // case 'H':
        case ((char)TimeUnit::Hour):
            return std::optional<TimeUnit>{TimeUnit::Hour};
        // case 'M':
        case ((char)TimeUnit::Minute):
            return std::optional<TimeUnit>{TimeUnit::Minute};
        // case 'S':
        case ((char)TimeUnit::Second):
            return std::optional<TimeUnit>{TimeUnit::Second};
    }
    return {};
};

std::optional<TimeUnit> timeUnitFromString(std::string_view sv) noexcept {
    if (sv.size() == 1) {
        return timeUnitFromChar(sv[0]);
    }
    return {};
};

char timeUnitToChar(TimeUnit tu) noexcept {
    return static_cast<char>(tu);
};

//-----------------------------------------------------------------------------

std::int64_t lastDayOfTheMonth(std::int64_t y, std::int64_t m) {
    // month must be base 0
    std::int64_t i = m - 1;
    const std::int64_t d_min = 0;
    return 31 - std::max(d_min, i % 6 - i / 6) % 2
         - std::max(d_min, 2 - i * (i % 2)) % 2 * (y % 4 == 0 ? y % 100 == 0 ? y % 400 == 0 ? 1 : 2 : 1 : 2);
}

//-----------------------------------------------------------------------------


double dateTimeDiffInSeconds(const DateInts& lhsDate, const TimeInts& lhsTime, const DateInts& rhsDate,
                             const TimeInts& rhsTime) {
    // eckit::Time is not supposed to hold values larger than 23:59:59. However Fesom sends 24:00:00.
    // Hence we compute the time diff manually
    eckit::DateTime l{eckit::Date{lhsDate.year, lhsDate.month, lhsDate.day}, eckit::Time{0, 0, 0}};

    double lhsSec = (lhsTime.hour * 60 + lhsTime.minute) * 60 + lhsTime.second;
    eckit::DateTime r{eckit::Date{rhsDate.year, rhsDate.month, rhsDate.day}, eckit::Time{0, 0, 0}};
    double rhsSec = (rhsTime.hour * 60 + rhsTime.minute) * 60 + rhsTime.second;

    return (l - r) + (lhsSec - rhsSec);
}


DateTimeDiff dateTimeDiff(const DateInts& lhsDate, const TimeInts& lhsTime, const DateInts& rhsDate,
                          const TimeInts& rhsTime) {
    constexpr double MIN = 60.0;
    constexpr double HOUR = 3600.0;
    constexpr double DAY = 86400.0;

    // Determine unit by checking equality from smaller to higher fractions (second to year)
    if (lhsTime.second != rhsTime.second) {
        return DateTimeDiff{static_cast<std::int64_t>(dateTimeDiffInSeconds(lhsDate, lhsTime, rhsDate, rhsTime)),
                            TimeUnit::Second};
    }
    else if (lhsTime.minute != rhsTime.minute) {
        return DateTimeDiff{static_cast<std::int64_t>(dateTimeDiffInSeconds(lhsDate, lhsTime, rhsDate, rhsTime) / MIN),
                            TimeUnit::Minute};
    }
    else if (lhsTime.hour != rhsTime.hour) {
        return DateTimeDiff{static_cast<std::int64_t>(dateTimeDiffInSeconds(lhsDate, lhsTime, rhsDate, rhsTime) / HOUR),
                            TimeUnit::Hour};
    }
    else if (lhsDate.day != rhsDate.day) {
        return DateTimeDiff{static_cast<std::int64_t>(dateTimeDiffInSeconds(lhsDate, lhsTime, rhsDate, rhsTime) / DAY),
                            TimeUnit::Day};
    }
    else if (lhsDate.month != rhsDate.month) {
        return DateTimeDiff{(lhsDate.year - rhsDate.year) * 12 + (lhsDate.month - rhsDate.month), TimeUnit::Month};
    }
    else if (lhsDate.year != rhsDate.year) {
        return DateTimeDiff{lhsDate.year - rhsDate.year, TimeUnit::Year};
    }
    else {
        return DateTimeDiff{0, TimeUnit::Second};
    }
}

DateTimeDiff dateTimeDiff(const DateTimeInts& lhs, const DateTimeInts& rhs) {
    return dateTimeDiff(lhs.date, lhs.time, rhs.date, rhs.time);
}


//-----------------------------------------------------------------------------

}  // namespace multio::util

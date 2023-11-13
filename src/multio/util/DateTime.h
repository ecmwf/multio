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


#pragma once

#include <algorithm>
#include <cstdint>
#include <optional>
#include <string>

namespace multio::util {

//-----------------------------------------------------------------------------

struct DateInts {
    std::int64_t year;
    std::int64_t month;
    std::int64_t day;
};


struct TimeInts {
    std::int64_t hour;
    std::int64_t minute;
    std::int64_t second;
};


struct DateTimeInts {
    DateInts date;
    TimeInts time;
};


DateInts toDateInts(std::int64_t date) noexcept;


TimeInts toTimeInts(std::int64_t time) noexcept;


std::int64_t fromDateInts(const DateInts& ints) noexcept;


std::int64_t fromTimeInts(const TimeInts& ints) noexcept;


DateTimeInts wrapDateTime(DateTimeInts);


//-----------------------------------------------------------------------------

enum class TimeUnit : char
{
    Year = 'Y',
    Month = 'm',
    Day = 'd',
    Hour = 'H',
    Minute = 'M',
    Second = 'S',

};

std::optional<TimeUnit> timeUnitFromChar(char c) noexcept;

std::optional<TimeUnit> timeUnitFromString(std::string_view sv) noexcept;

char timeUnitToChar(TimeUnit tu) noexcept;

//-----------------------------------------------------------------------------

std::int64_t lastDayOfTheMonth(std::int64_t y, std::int64_t m);

//-----------------------------------------------------------------------------

struct DateTimeDiff {
    std::int64_t diff;
    TimeUnit unit;
};


DateTimeDiff dateTimeDiff(const DateInts& lhsDate, const TimeInts& lhsTime, const DateInts& rhs,
                          const TimeInts& rhsTime);

DateTimeDiff dateTimeDiff(const DateTimeInts& lhs, const DateTimeInts& rhs);


double dateTimeDiffInSeconds(const DateInts& lhsDate, const TimeInts& lhsTime, const DateInts& rhsDate,
                             const TimeInts& rhsTime);

//-----------------------------------------------------------------------------

}  // namespace multio::util

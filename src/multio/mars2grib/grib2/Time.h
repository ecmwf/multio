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
#include "multio/datamod/types/TimeDuration.h"

// Level config
namespace multio::mars2grib::grib2 {

namespace dm = multio::datamod;


// TODO Significance of reference time
// Model

struct DateTimeKeys {
    dm::Entry<std::int64_t> year;
    dm::Entry<std::int64_t> month;
    dm::Entry<std::int64_t> day;
    dm::Entry<std::int64_t> hour;
    dm::Entry<std::int64_t> minute;
    dm::Entry<std::int64_t> second;

    static constexpr std::string_view record_name_ = "date-time-keys";
    static constexpr auto record_entries_ = std::make_tuple(  //
        entryDef("year", &DateTimeKeys::year),                //
        entryDef("month", &DateTimeKeys::month),              //
        entryDef("day", &DateTimeKeys::day),                  //
        entryDef("hour", &DateTimeKeys::hour),                //
        entryDef("minute", &DateTimeKeys::minute),            //
        entryDef("second", &DateTimeKeys::second));
};

struct RefDateTimeKeys {
    dm::Entry<std::int64_t> YearOfModelVersion;
    dm::Entry<std::int64_t> MonthOfModelVersion;
    dm::Entry<std::int64_t> DayOfModelVersion;
    dm::Entry<std::int64_t> HourOfModelVersion;
    dm::Entry<std::int64_t> MinuteOfModelVersion;
    dm::Entry<std::int64_t> SecondOfModelVersion;

    static constexpr std::string_view record_name_ = "ref-date-time-keys";
    static constexpr auto record_entries_ = std::make_tuple(      //
        entryDef("YearOfModelVersion", &DateTimeKeys::year),      //
        entryDef("MonthOfModelVersion", &DateTimeKeys::month),    //
        entryDef("DayOfModelVersion", &DateTimeKeys::day),        //
        entryDef("HourOfModelVersion", &DateTimeKeys::hour),      //
        entryDef("MinuteOfModelVersion", &DateTimeKeys::minute),  //
        entryDef("SecondOfModelVersion", &DateTimeKeys::second));
};


/// Keys that are used for both point-in-time and time-range
struct InitForecastTimeKeys {
    dm::Entry<std::int64_t> hoursAfterDataCutoff;    // Set to missing
    dm::Entry<std::int64_t> minutesAfterDataCutoff;  // Set to missing
    dm::Entry<std::string>
        indicatorOfUnitForForecastTime;  // Initialized to "h" - needs to be refactored for subhourly steps...

    static constexpr std::string_view record_name_ = "init-forecast-time-keys";
    static constexpr auto record_entries_ = std::make_tuple(                                          //
        entryDef("hoursAfterDataCutoff", &InitForecastTimeKeys::hoursAfterDataCutoff).tagOptional(),  //
        entryDef("minutesAfterDataCutoff", &InitForecastTimeKeys::minutesAfterDataCutoff).tagOptional(),
        entryDef("indicatorOfUnitForForecastTime", &InitForecastTimeKeys::indicatorOfUnitForForecastTime)
            .withDefault("h"));
};


struct PointInTimeKeys {
    // PointInTime will set `forecastTime` and `indicatorOfUnitForForecastTime` throuh step
    dm::Entry<dm::TimeDuration> step;

    static constexpr std::string_view record_name_ = "point-in-time-keys";
    static constexpr auto record_entries_ = std::make_tuple(  //
        entryDef("step", &PointInTimeKeys::step));
};

struct TimeRangeKeys {
    // TODO: handle setting forecastTime

    dm::Entry<std::int64_t> yearOfOverallTimeInterval;
    dm::Entry<std::int64_t> monthOfOverallTimeInterval;
    dm::Entry<std::int64_t> dayOfOverallTimeInterval;
    dm::Entry<std::int64_t> hourOfOverallTimeInterval;
    dm::Entry<std::int64_t> minuteOfOverallTimeInterval;
    dm::Entry<std::int64_t> secondOfOverallTimeInterval;

    dm::Entry<std::int64_t> numberOfTimeRange;

    // Vector for now - can be optimized later
    dm::Entry<std::vector<dm::TypeOfStatisticalProcessing>> typeOfStatisticalProcessing;
    dm::Entry<std::vector<std::int64_t>> typeOfTimeIncrement;
    dm::Entry<std::vector<std::string>> indicatorOfUnitForTimeRange;
    dm::Entry<std::vector<std::int64_t>> lengthOfTimeRange;
    dm::Entry<std::vector<std::string>> indicatorOfUnitForTimeIncrement;
    dm::Entry<std::vector<std::int64_t>> timeIncrement;

    static constexpr std::string_view record_name_ = "time-range-end-keys";
    static constexpr auto record_entries_ = std::make_tuple(  //
        entryDef("yearOfOverallTimeInterval", &DateTimeKeys::year),
        entryDef("monthOfOverallTimeInterval", &DateTimeKeys::month),
        entryDef("dayOfOverallTimeInterval", &DateTimeKeys::day),
        entryDef("hourOfOverallTimeInterval", &DateTimeKeys::hour),
        entryDef("minuteOfOverallTimeInterval", &DateTimeKeys::minute),
        entryDef("secondOfOverallTimeInterval", &DateTimeKeys::second),
        entryDef("numberOfTimeRange", &TimeRangeKeys::numberOfTimeRange),
        entryDef("typeOfStatisticalProcessing", &TimeRangeKeys::typeOfStatisticalProcessing),
        entryDef("typeOfTimeIncrement", &TimeRangeKeys::typeOfTimeIncrement),
        entryDef("indicatorOfUnitForTimeRange", &TimeRangeKeys::indicatorOfUnitForTimeRange),
        entryDef("lengthOfTimeRange", &TimeRangeKeys::lengthOfTimeRange),
        entryDef("indicatorOfUnitForTimeIncrement", &TimeRangeKeys::indicatorOfUnitForTimeIncrement),
        entryDef("timeIncrement", &TimeRangeKeys::timeIncrement));
};


InitForecastTimeKeys setInitForecastTime();
DateTimeKeys setDateTime(const dm::FullMarsRecord&);
std::optional<RefDateTimeKeys> setRefDateTime(const dm::FullMarsRecord&);

std::optional<PointInTimeKeys> setPointInTime(const dm::FullMarsRecord&);
std::optional<TimeRangeKeys> setTimeRange(const dm::FullMarsRecord&, const dm::MiscRecord);


}  // namespace multio::mars2grib::grib2

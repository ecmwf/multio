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

/// @date Sep 2023

#pragma once

#include "multio/message/details/MetadataTypes.h"

#include <string>

namespace multio::message::details {


/**
 * TBD Create a typed decsription of metadata entries through which access on metadata is performed.
 */
// template <typename KeyType, typename ValidTypes>
// struct KeyValueDescription {
//     KeyType key;

//     operator KeyType&() { return key; }
//     operator const KeyType&() const { return key; }
// };


//-----------------------------------------------------------------------------

/**
 * This class is ment to keep track of different metadata keys used within the action provided through multio.
 * Reasons to have this:
 *  - Keep track of metadata keys that are used - with a proper IDE we can jump to all places a key is used
 *  - Just using strings at multiple places is error prone (typos can happen)
 *  - In the future also type information and specialized access operations should be added
 *  - We can do proper benchmark of metadata operations with typical keys. Moreover its easy to benchmark different key
 * (fixed strings, prehashed strings in case of hashmaps) and maptypes
 */
template <typename Traits = DefaultMetadataTraits>
struct Glossary {
    using KeyType = typename Traits::KeyType;

    // template <typename ValidTypes>
    // using Description = KeyValueDescription<KeyType, ValidTypes>;

    // General keys
    const KeyType paramId{"paramId"};
    const KeyType globalSize{"globalSize"};
    const KeyType domain{"domain"};
    const KeyType date{"date"};
    const KeyType time{"time"};
    const KeyType precision{"precision"};

    // Mars keys
    const KeyType type{"type"};
    const KeyType classKey{"class"};
    const KeyType stream{"stream"};
    const KeyType expver{"expver"};

    // Eccodes concepts
    const KeyType gridType{"gridType"};
    const KeyType typeOfLevel{"typeOfLevel"};
    const KeyType localDefinitionNumber{"localDefinitionNumber"};

    // Eccodes general
    const KeyType typeOfGeneratingProcess{"typeOfGeneratingProcess"};  // Analog to mars type

    // Additional eccodes keys
    const KeyType missingValue{"missingValue"};
    const KeyType bitsPerValue{"bitsPerValue"};
    const KeyType bitmapPresent{"bitmapPresent"};

    // Eccodes grib reference date/time - direct setting (alternative to date & time)
    const KeyType year{"year"};
    const KeyType month{"month"};
    const KeyType day{"day"};
    const KeyType hour{"hour"};
    const KeyType minute{"minute"};
    const KeyType second{"second"};

    // Eccodes analysis date/time - direct setting (alternative to dateOfAnalysis & timeOfAnalysis) -- ONLY VALID FOR A
    // SPECIFIC localDefinitionNumber
    const KeyType yearOfAnalysis{"yearOfAnalysis"};
    const KeyType monthOfAnalysis{"monthOfAnalysis"};
    const KeyType dayOfAnalysis{"dayOfAnalysis"};
    const KeyType hourOfAnalysis{"hourOfAnalysis"};
    const KeyType minuteOfAnalysis{"minuteOfAnalysis"};
    const KeyType secondOfAnalysis{"secondOfAnalysis"};

    // Eccodes grib2 stat
    const KeyType yearOfEndOfOverallTimeInterval{"yearOfEndOfOverallTimeInterval"};
    const KeyType monthOfEndOfOverallTimeInterval{"monthOfEndOfOverallTimeInterval"};
    const KeyType dayOfEndOfOverallTimeInterval{"dayOfEndOfOverallTimeInterval"};
    const KeyType hourOfEndOfOverallTimeInterval{"hourOfEndOfOverallTimeInterval"};
    const KeyType minuteOfEndOfOverallTimeInterval{"minuteOfEndOfOverallTimeInterval"};
    const KeyType secondOfEndOfOverallTimeInterval{"secondOfEndOfOverallTimeInterval"};
    const KeyType typeOfStatisticalProcessing{"typeOfStatisticalProcessing"};
    const KeyType lengthOfTimeRange{"lengthOfTimeRange"};
    const KeyType indicatorOfUnitForTimeIncrement{"indicatorOfUnitForTimeIncrement"};
    const KeyType timeIncrement{"timeIncrement"};

    // Eccodes grib2 grid
    const KeyType unstructuredGridType{"unstructuredGridType"};
    const KeyType unstructuredGridSubtype{"unstructuredGridSubtype"};
    const KeyType uuidOfHGrid{"uuidOfHGrid"};

    // Eccodes grib horizontal + vertial
    const KeyType level{"level"};
    const KeyType scaledValueOfFirstFixedSurface{"scaledValueOfFirstFixedSurface"};
    const KeyType scaledValueOfSecondFixedSurface{"scaledValueOfSecondFixedSurface"};

    // Time model
    const KeyType startTime{"startTime"};
    const KeyType startDate{"startDate"};
    const KeyType previousTime{"previousTime"};
    const KeyType previousDate{"previousDate"};
    const KeyType currentTime{"currentTime"};
    const KeyType currentDate{"currentDate"};

    static const Glossary<Traits>& instance() {
        static Glossary<Traits> glossary;
        return glossary;
    }
};

//-----------------------------------------------------------------------------

}  // namespace multio::message::details

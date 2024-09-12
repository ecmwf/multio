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

#include "multio/message/MetadataTypes.h"

#include <string>

namespace multio::message {


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
 * TODO Think about another option: create namespcae `glosasry` and declare all keys as `const static KeyType
 * keyName{"keyName"}`
 *
 * This class is ment to keep track of different metadata keys used within the action provided through multio.
 * Reasons to have this:
 *  - Keep track of metadata keys that are used - with a proper IDE we can jump to all places a key is used
 *  - Just using strings at multiple places is error prone (typos can happen)
 *  - In the future also type information and specialized access operations should be added
 *  - We can do proper benchmark of metadata operations with typical keys. Moreover its easy to benchmark different key
 * (fixed strings, prehashed strings in case of hashmaps) and maptypes
 */
struct Glossary {
    using KeyType = typename MetadataTypes::KeyType;

    // template <typename ValidTypes>
    // using Description = KeyValueDescription<KeyType, ValidTypes>;

    // General keys
    const KeyType name{"name"};
    const KeyType paramId{"paramId"};
    const KeyType param{"param"};
    const KeyType globalSize{"globalSize"};
    const KeyType domain{"domain"};
    const KeyType date{"date"};
    const KeyType time{"time"};
    const KeyType precision{"precision"};


    // Nemo
    const KeyType nemoParam{"nemoParam"};
    const KeyType category{"category"};

    // Mars keys
    const KeyType type{"type"};
    const KeyType marsType{"marsType"};
    const KeyType classKey{"class"};
    const KeyType marsClass{"marsClass"};
    const KeyType stream{"stream"};
    const KeyType marsStream{"marsStream"};
    const KeyType expver{"expver"};
    const KeyType experimentVersionNumber{"experimentVersionNumber"};
    const KeyType levelist{"levelist"};
    const KeyType levtype{"levtype"};
    const KeyType levtypeWam{"levtype_wam"};
    const KeyType dataset{"dataset"};
    const KeyType resolution{"resolution"};
    const KeyType activity{"activity"};
    const KeyType experiment{"experiment"};
    const KeyType generation{"generation"};
    const KeyType model{"model"};
    const KeyType realization{"realization"};
    const KeyType methodNumber{"methodNumber"};
    const KeyType systemNumber{"systemNumber"};
    const KeyType methodNumberKC{"method-number"};  // Kebap case
    const KeyType systemNumberKC{"system-number"};  // Kebap case

    // Eccodes specific
    const KeyType gribEdition{"gribEdition"};
    const KeyType tablesVersion{"tablesVersion"};
    const KeyType localTablesVersion{"localTablesVersion"};
    const KeyType setLocalDefinition{"setLocalDefinition"};
    const KeyType grib2LocalSectionNumber{"grib2LocalSectionNumber"};
    const KeyType extraLocalSectionNumber{"extraLocalSectionNumber"};
    const KeyType deleteExtraLocalSection{"deleteExtraLocalSection"};
    const KeyType productDefinitionTemplateNumber{"productDefinitionTemplateNumber"};
    const KeyType productionStatusOfProcessedData{"productionStatusOfProcessedData"};

    // Eccodes concepts
    const KeyType gridType{"gridType"};
    const KeyType typeOfLevel{"typeOfLevel"};
    const KeyType localDefinitionNumber{"localDefinitionNumber"};

    // Additional eccodes keys
    const KeyType setPackingType{"setPackingType"};
    const KeyType complexPacking{"complexPacking"};
    const KeyType missingValue{"missingValue"};
    const KeyType bitsPerValue{"bitsPerValue"};
    const KeyType bitmapPresent{"bitmapPresent"};

    // Grib general
    const KeyType typeOfGeneratingProcess{"typeOfGeneratingProcess"};  // Analog to mars type
    const KeyType generatingProcessIdentifier{"generatingProcessIdentifier"};
    const KeyType subCentre{"subCentre"};

    const KeyType perturbationNumber{"perturbationNumber"};
    const KeyType numberOfForecastsInEnsemble{"numberOfForecastsInEnsemble"};
    const KeyType ensembleMember{"ensembleMember"};
    const KeyType ensembleSize{"ensembleSize"};
    const KeyType ensembleMemberKC{"ensemble-member"};  // Kebap case
    const KeyType ensembleSizeKC{"ensemble-size"};      // Kebap case
    const KeyType offsetToEndOf4DvarWindow{"offsetToEndOf4DvarWindow"};
    const KeyType lengthOf4DvarWindow{"lengthOf4DvarWindow"};

    const KeyType anoffset{"anoffset"};
    const KeyType anlength{"anlength"};

    const KeyType componentIndex{"componentIndex"};
    const KeyType numberOfComponents{"numberOfComponents"};
    const KeyType modelErrorType{"modelErrorType"};
    const KeyType iterationNumber{"iterationNumber"};
    const KeyType totalNumberOfIterations{"totalNumberOfIterations"};


    // Eccodes grib reference date/time - direct setting (alternative to date & time)
    const KeyType year{"year"};
    const KeyType month{"month"};
    const KeyType day{"day"};
    const KeyType hour{"hour"};
    const KeyType minute{"minute"};
    const KeyType second{"second"};

    const KeyType forecastTime{"forecastTime"};

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
    const KeyType scaleFactorOfFirstFixedSurface{"scaleFactorOfFirstFixedSurface"};
    const KeyType scaleFactorOfSecondFixedSurface{"scaleFactorOfSecondFixedSurface"};
    const KeyType typeOfFirstFixedSurface{"typeOfFirstFixedSurface"};
    const KeyType typeOfSecondFixedSurface{"typeOfSecondFixedSurface"};

    // Time model
    const KeyType startTime{"startTime"};
    const KeyType startDate{"startDate"};
    const KeyType previousTime{"previousTime"};
    const KeyType previousDate{"previousDate"};
    const KeyType currentTime{"currentTime"};
    const KeyType currentDate{"currentDate"};

    const KeyType sampleInterval{"sampleInterval"};
    const KeyType sampleIntervalInSeconds{"sampleIntervalInSeconds"};

    // legacy & conversion
    const KeyType timeStep{"timeStep"};
    const KeyType step{"step"};
    const KeyType stepUnits{"stepUnits"};
    const KeyType stepRange{"stepRange"};
    const KeyType startStep{"startStep"};
    const KeyType endStep{"endStep"};
    const KeyType dataTime{"dataTime"};
    const KeyType dataDate{"dataDate"};
    const KeyType indicatorOfUnitForTimeRange{"indicatorOfUnitForTimeRange"};

    const KeyType dateOfAnalysis{"date-of-analysis"};
    const KeyType timeOfAnalysis{"time-of-analysis"};

    // Statistic
    const KeyType operation{"operation"};
    const KeyType restartStep{"restart-step"};
    const KeyType stepFrequency{"step-frequency"};

    // Healpix
    const KeyType nside{"Nside"};
    const KeyType orderingConvention{"orderingConvention"};

    // Spherical harmonics
    const KeyType sphericalHarmonics{"sphericalHarmonics"};
    const KeyType pentagonalResolutionParameterJ{"pentagonalResolutionParameterJ"};
    const KeyType pentagonalResolutionParameterK{"pentagonalResolutionParameterK"};
    const KeyType pentagonalResolutionParameterM{"pentagonalResolutionParameterM"};
    const KeyType j{"J"};
    const KeyType k{"K"};
    const KeyType m{"M"};
    const KeyType subSetJ{"subSetJ"};
    const KeyType subSetK{"subSetK"};
    const KeyType subSetM{"subSetM"};
    const KeyType js{"JS"};
    const KeyType ks{"KS"};
    const KeyType ms{"MS"};


    // Regular ll
    const KeyType ni{"Ni"};
    const KeyType nj{"Nj"};
    const KeyType north{"north"};
    const KeyType west{"west"};
    const KeyType south{"south"};
    const KeyType east{"east"};
    const KeyType westEastIncrement{"west_east_increment"};
    const KeyType southNorthIncrement{"south_north_increment"};


    // IFS/ATM mapping
    const KeyType ifsPrefix{"ifsPrefix"};
    const KeyType ifsRepres{"ifsRepres"};
    const KeyType ifsCurrStep{"currentStep"};
    const KeyType ifsPrevPP{"previousProcessingPoint"};


    static const Glossary& instance() {
        static Glossary glossary;
        return glossary;
    }
};

const Glossary& glossary();

//-----------------------------------------------------------------------------

}  // namespace multio::message

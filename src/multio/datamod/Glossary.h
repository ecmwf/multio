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

#include "multio/datamod/DataModelling.h"

#include <chrono>
#include <string>


namespace multio::datamod {

//-----------------------------------------------------------------------------
// To be refactored / replaced
//-----------------------------------------------------------------------------

/**
 * TODO old glossary ... will be hopefully reploced with keysets
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
    using KeyType = util::PrehashedKey<std::string>;

    template <typename ValueType>
    struct KV {
        using KeyType = util::PrehashedKey<std::string>;
        using This = KV<ValueType>;

        // To be removed in future when glossary is refactored
        operator const KeyType&() const { return key_; }
        operator const std::string&() const { return key_; }

        const KeyType& key() const { return key_; }

        // Members
        KeyType key_;
    };


    // General keys
    const KeyType name{"name"};
    const KeyType paramId{"paramId"};
    const KeyType param{"param"};
    const KeyType globalSize{"misc-globalSize"};
    const KeyType domain{"domain"};
    const KeyType date{"date"};
    const KeyType time{"time"};
    const KeyType precision{"misc-precision"};

    // Added missing 08/04/2025
    const KeyType shortName{"shortName"};
    const KeyType unpackedSubsetPrecision{"unpackedSubsetPrecision"};
    const KeyType representation{"representation"};
    const KeyType trigger{"trigger"};
    const KeyType gridded{"gridded"};

    // Nemo
    const KeyType nemoParam{"nemoParam"};
    const KeyType category{"category"};

    // Mars keys
    const KV<std::string> type{"type"};
    const KV<std::string> marsType{"marsType"};
    const KV<std::string> classKey{"class"};
    const KV<std::string> marsClass{"marsClass"};
    const KV<std::string> stream{"stream"};
    const KV<std::string> marsStream{"marsStream"};
    const KV<std::string> expver{"expver"};
    const KV<std::string> experimentVersionNumber{"experimentVersionNumber"};
    const KV<std::int64_t> levelist{"levelist"};
    const KV<std::string> levtype{"levtype"};
    const KV<std::string> levtypeWam{"levtype_wam"};
    const KV<std::string> dataset{"dataset"};
    const KV<std::string> resolution{"resolution"};
    const KV<std::string> activity{"activity"};
    const KV<std::string> experiment{"experiment"};
    const KV<std::string> generation{"generation"};
    const KV<std::string> model{"model"};
    const KV<std::string> realization{"realization"};
    const KV<std::int64_t> methodNumber{"methodNumber"};
    const KV<std::int64_t> systemNumber{"systemNumber"};
    const KV<std::int64_t> methodNumberKC{"method-number"};  // Kebap case
    const KV<std::int64_t> systemNumberKC{"system-number"};  // Kebap case

    // Eccodes specific
    const KV<std::string> gribEdition{"gribEdition"};
    const KV<std::int64_t> tablesVersion{"tablesVersion"};
    const KV<std::int64_t> localTablesVersion{"localTablesVersion"};
    const KV<bool> setLocalDefinition{"setLocalDefinition"};
    const KV<std::int64_t> grib2LocalSectionNumber{"grib2LocalSectionNumber"};
    const KV<std::int64_t> extraLocalSectionNumber{"extraLocalSectionNumber"};
    const KV<bool> deleteExtraLocalSection{"deleteExtraLocalSection"};
    const KV<std::int64_t> productDefinitionTemplateNumber{"productDefinitionTemplateNumber"};
    const KV<std::int64_t> productionStatusOfProcessedData{"productionStatusOfProcessedData"};

    // Eccodes concepts
    const KV<std::string> gridName{"gridName"};
    const KV<std::string> gridType{"gridType"};
    const KV<std::string> typeOfLevel{"typeOfLevel"};
    const KV<std::int64_t> localDefinitionNumber{"localDefinitionNumber"};

    // Additional eccodes keys
    const KV<std::string> setPackingType{"setPackingType"};
    const KV<std::int64_t> complexPacking{"complexPacking"};
    const KV<double> missingValue{"missingValue"};
    const KV<std::int64_t> bitsPerValue{"bitsPerValue"};
    const KV<bool> bitmapPresent{"bitmapPresent"};

    // Grib general
    const KV<std::int64_t> typeOfGeneratingProcess{"typeOfGeneratingProcess"};  // Analog to mars type
    const KV<std::int64_t> generatingProcessIdentifier{"generatingProcessIdentifier"};
    const KV<std::string> subCentre{"subCentre"};

    const KV<std::int64_t> perturbationNumber{"perturbationNumber"};
    const KV<std::int64_t> numberOfForecastsInEnsemble{"numberOfForecastsInEnsemble"};
    const KV<std::int64_t> ensembleMember{"ensembleMember"};
    const KV<std::int64_t> ensembleSize{"ensembleSize"};
    const KV<std::int64_t> ensembleMemberKC{"ensemble-member"};  // Kebap case
    const KV<std::int64_t> ensembleSizeKC{"ensemble-size"};      // Kebap case
    const KV<std::int64_t> offsetToEndOf4DvarWindow{"offsetToEndOf4DvarWindow"};
    const KV<std::int64_t> lengthOf4DvarWindow{"lengthOf4DvarWindow"};

    const KV<std::int64_t> anoffset{"anoffset"};
    const KV<std::int64_t> anlength{"anlength"};

    const KV<std::int64_t> componentIndex{"componentIndex"};
    const KV<std::int64_t> numberOfComponents{"numberOfComponents"};
    const KV<std::int64_t> modelErrorType{"modelErrorType"};
    const KV<std::int64_t> iterationNumber{"iterationNumber"};
    const KV<std::int64_t> totalNumberOfIterations{"totalNumberOfIterations"};


    // Eccodes grib reference date/time - direct setting (alternative to date & time)
    const KV<std::int64_t> year{"year"};
    const KV<std::int64_t> month{"month"};
    const KV<std::int64_t> day{"day"};
    const KV<std::int64_t> hour{"hour"};
    const KV<std::int64_t> minute{"minute"};
    const KV<std::int64_t> second{"second"};

    const KV<std::int64_t> forecastTime{"forecastTime"};

    // Eccodes analysis date/time - direct setting (alternative to dateOfAnalysis & timeOfAnalysis) -- ONLY VALID FOR A
    // SPECIFIC localDefinitionNumber
    const KV<std::int64_t> yearOfAnalysis{"yearOfAnalysis"};
    const KV<std::int64_t> monthOfAnalysis{"monthOfAnalysis"};
    const KV<std::int64_t> dayOfAnalysis{"dayOfAnalysis"};
    const KV<std::int64_t> hourOfAnalysis{"hourOfAnalysis"};
    const KV<std::int64_t> minuteOfAnalysis{"minuteOfAnalysis"};
    const KV<std::int64_t> secondOfAnalysis{"secondOfAnalysis"};

    // Eccodes grib2 stat
    const KV<std::int64_t> yearOfEndOfOverallTimeInterval{"yearOfEndOfOverallTimeInterval"};
    const KV<std::int64_t> monthOfEndOfOverallTimeInterval{"monthOfEndOfOverallTimeInterval"};
    const KV<std::int64_t> dayOfEndOfOverallTimeInterval{"dayOfEndOfOverallTimeInterval"};
    const KV<std::int64_t> hourOfEndOfOverallTimeInterval{"hourOfEndOfOverallTimeInterval"};
    const KV<std::int64_t> minuteOfEndOfOverallTimeInterval{"minuteOfEndOfOverallTimeInterval"};
    const KV<std::int64_t> secondOfEndOfOverallTimeInterval{"secondOfEndOfOverallTimeInterval"};
    const KV<std::string> typeOfStatisticalProcessing{"typeOfStatisticalProcessing"};
    const KV<std::int64_t> lengthOfTimeRange{"lengthOfTimeRange"};
    const KV<std::int64_t> indicatorOfUnitForTimeIncrement{"indicatorOfUnitForTimeIncrement"};
    const KV<std::int64_t> timeIncrement{"timeIncrement"};

    // Eccodes grib2 grid
    const KV<std::string> unstructuredGridType{"unstructuredGridType"};
    const KV<std::string> unstructuredGridSubtype{"unstructuredGridSubtype"};
    const KV<std::string> uuidOfHGrid{"uuidOfHGrid"};

    // Eccodes grib horizontal + vertial
    const KV<std::int64_t> level{"level"};
    const KV<std::int64_t> scaledValueOfFirstFixedSurface{"scaledValueOfFirstFixedSurface"};
    const KV<std::int64_t> scaledValueOfSecondFixedSurface{"scaledValueOfSecondFixedSurface"};
    const KV<std::int64_t> scaleFactorOfFirstFixedSurface{"scaleFactorOfFirstFixedSurface"};
    const KV<std::int64_t> scaleFactorOfSecondFixedSurface{"scaleFactorOfSecondFixedSurface"};
    const KV<std::int64_t> typeOfFirstFixedSurface{"typeOfFirstFixedSurface"};
    const KV<std::int64_t> typeOfSecondFixedSurface{"typeOfSecondFixedSurface"};

    // Time model
    const KV<std::int64_t> startTime{"startTime"};
    const KV<std::int64_t> startDate{"startDate"};
    const KV<std::int64_t> previousTime{"previousTime"};
    const KV<std::int64_t> previousDate{"previousDate"};
    const KV<std::int64_t> currentTime{"currentTime"};
    const KV<std::int64_t> currentDate{"currentDate"};

    const KV<std::int64_t> sampleInterval{"sampleInterval"};
    const KV<std::int64_t> sampleIntervalInSeconds{"sampleIntervalInSeconds"};

    // legacy & conversion
    const KV<std::int64_t> timeStep{"timeStep"};
    const KV<std::int64_t> step{"step"};
    const KV<std::int64_t> stepUnits{"stepUnits"};
    const KV<std::string> stepRange{"stepRange"};
    const KV<std::int64_t> startStep{"startStep"};
    const KV<std::int64_t> endStep{"endStep"};
    const KV<std::int64_t> dataTime{"dataTime"};
    const KV<std::int64_t> dataDate{"dataDate"};
    const KV<std::int64_t> indicatorOfUnitForTimeRange{"indicatorOfUnitForTimeRange"};

    const KV<std::int64_t> dateOfAnalysis{"date-of-analysis"};
    const KV<std::int64_t> timeOfAnalysis{"time-of-analysis"};

    // Statistic
    const KV<std::string> operation{"operation"};
    const KV<std::int64_t> restartStep{"restart-step"};
    const KV<std::int64_t> stepFrequency{"step-frequency"};

    // Healpix
    const KV<std::int64_t> nside{"Nside"};
    const KV<std::string> orderingConvention{"orderingConvention"};

    // Spherical harmonics
    const KeyType sphericalHarmonics{"sphericalHarmonics"};
    const KV<std::int64_t> pentagonalResolutionParameterJ{"pentagonalResolutionParameterJ"};
    const KV<std::int64_t> pentagonalResolutionParameterK{"pentagonalResolutionParameterK"};
    const KV<std::int64_t> pentagonalResolutionParameterM{"pentagonalResolutionParameterM"};
    const KV<std::int64_t> j{"J"};
    const KV<std::int64_t> k{"K"};
    const KV<std::int64_t> m{"M"};
    const KV<std::int64_t> subSetJ{"subSetJ"};
    const KV<std::int64_t> subSetK{"subSetK"};
    const KV<std::int64_t> subSetM{"subSetM"};
    const KV<std::int64_t> js{"JS"};
    const KV<std::int64_t> ks{"KS"};
    const KV<std::int64_t> ms{"MS"};


    // Regular ll
    const KV<std::int64_t> ni{"Ni"};
    const KV<std::int64_t> nj{"Nj"};

    // Regular ll - mapped
    const KV<double> north{"north"};
    const KV<double> west{"west"};
    const KV<double> south{"south"};
    const KV<double> east{"east"};
    const KV<double> westEastIncrement{"west_east_increment"};
    const KV<double> southNorthIncrement{"south_north_increment"};

    // Regular ll - direct
    const KV<double> latitudeOfFirstGridPointInDegrees{"latitudeOfFirstGridPointInDegrees"};
    const KV<double> latitudeOfLastGridPointInDegrees{"latitudeOfLastGridPointInDegrees"};
    const KV<double> longitudeOfFirstGridPointInDegrees{"longitudeOfFirstGridPointInDegrees"};
    const KV<double> longitudeOfLastGridPointInDegrees{"longitudeOfLastGridPointInDegrees"};
    const KV<double> jDirectionIncrementInDegrees{"jDirectionIncrementInDegrees"};
    const KV<double> iDirectionIncrementInDegrees{"iDirectionIncrementInDegrees"};


    static const Glossary& instance() {
        static Glossary glossary;
        return glossary;
    }
};

const Glossary& glossary();


//-----------------------------------------------------------------------------

}  // namespace multio::datamod


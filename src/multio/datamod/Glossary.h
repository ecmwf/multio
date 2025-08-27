/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#pragma once

#include "multio/util/PrehashedKey.h"


namespace multio::datamod::legacy {

//-----------------------------------------------------------------------------
// To be refactored / replaced
//
// Should be separated in MARS MISC GRIB keys and structured properly
//
//-----------------------------------------------------------------------------


using KeyType = util::PrehashedKey<std::string_view>;

template <typename ValueType>
struct KV {
    using This = KV<ValueType>;

    // To be removed in future when glossary is refactored
    operator const KeyType&() const { return key_; }
    operator std::string() const { return std::string(key_); }

    const KeyType& key() const { return key_; }

    // Members
    KeyType key_;
};


// General keys
constexpr KeyType Name{"name"};
constexpr KeyType ParamId{"paramId"};
constexpr KeyType Param{"param"};
constexpr KeyType GlobalSize{"misc-globalSize"};
constexpr KeyType Domain{"domain"};
constexpr KeyType Date{"date"};
constexpr KeyType Time{"time"};
constexpr KeyType Precision{"misc-precision"};

// Added missing 08/04/2025
constexpr KeyType ShortName{"shortName"};
constexpr KeyType UnpackedSubsetPrecision{"unpackedSubsetPrecision"};
constexpr KeyType Representation{"representation"};
constexpr KeyType Trigger{"trigger"};
constexpr KeyType Gridded{"gridded"};

// Nemo
constexpr KeyType NemoParam{"nemoParam"};
constexpr KeyType Category{"category"};

// Mars keys
constexpr auto Type = KV<std::string>{"type"};
constexpr auto MarsType = KV<std::string>{"marsType"};
constexpr auto ClassKey = KV<std::string>{"class"};
constexpr auto MarsClass = KV<std::string>{"marsClass"};
constexpr auto Stream = KV<std::string>{"stream"};
constexpr auto MarsStream = KV<std::string>{"marsStream"};
constexpr auto Expver = KV<std::string>{"expver"};
constexpr auto ExperimentVersionNumber = KV<std::string>{"experimentVersionNumber"};
constexpr auto Levelist = KV<std::int64_t>{"levelist"};
constexpr auto Levtype = KV<std::string>{"levtype"};
constexpr auto LevtypeWam = KV<std::string>{"levtype_wam"};
constexpr auto Dataset = KV<std::string>{"dataset"};
constexpr auto Resolution = KV<std::string>{"resolution"};
constexpr auto Activity = KV<std::string>{"activity"};
constexpr auto Experiment = KV<std::string>{"experiment"};
constexpr auto Generation = KV<std::string>{"generation"};
constexpr auto Model = KV<std::string>{"model"};
constexpr auto Realization = KV<std::string>{"realization"};
constexpr auto MethodNumber = KV<std::int64_t>{"methodNumber"};
constexpr auto SystemNumber = KV<std::int64_t>{"systemNumber"};
constexpr auto MethodNumberKC = KV<std::int64_t>{"method-number"};  // Kebap case
constexpr auto SystemNumberKC = KV<std::int64_t>{"system-number"};  // Kebap case

// Eccodes specific
constexpr auto GribEdition = KV<std::string>{"gribEdition"};
constexpr auto TablesVersion = KV<std::int64_t>{"tablesVersion"};
constexpr auto LocalTablesVersion = KV<std::int64_t>{"localTablesVersion"};
constexpr auto SetLocalDefinition = KV<bool>{"setLocalDefinition"};
constexpr auto Grib2LocalSectionNumber = KV<std::int64_t>{"grib2LocalSectionNumber"};
constexpr auto ExtraLocalSectionNumber = KV<std::int64_t>{"extraLocalSectionNumber"};
constexpr auto DeleteExtraLocalSection = KV<bool>{"deleteExtraLocalSection"};
constexpr auto ProductDefinitionTemplateNumber = KV<std::int64_t>{"productDefinitionTemplateNumber"};
constexpr auto ProductionStatusOfProcessedData = KV<std::int64_t>{"productionStatusOfProcessedData"};

// Eccodes concepts
constexpr auto GridName = KV<std::string>{"gridName"};
constexpr auto GridType = KV<std::string>{"gridType"};
constexpr auto TypeOfLevel = KV<std::string>{"typeOfLevel"};
constexpr auto LocalDefinitionNumber = KV<std::int64_t>{"localDefinitionNumber"};

// Additional eccodes keys
constexpr auto SetPackingType = KV<std::string>{"setPackingType"};
constexpr auto ComplexPacking = KV<std::int64_t>{"complexPacking"};
constexpr auto MissingValue = KV<double>{"missingValue"};
constexpr auto BitsPerValue = KV<std::int64_t>{"bitsPerValue"};
constexpr auto BitmapPresent = KV<bool>{"bitmapPresent"};

// Grib general
constexpr auto TypeOfGeneratingProcess = KV<std::int64_t>{"typeOfGeneratingProcess"};  // Analog to mars type
constexpr auto GeneratingProcessIdentifier = KV<std::int64_t>{"generatingProcessIdentifier"};
constexpr auto SubCentre = KV<std::string>{"subCentre"};

constexpr auto PerturbationNumber = KV<std::int64_t>{"perturbationNumber"};
constexpr auto NumberOfForecastsInEnsemble = KV<std::int64_t>{"numberOfForecastsInEnsemble"};
constexpr auto EnsembleMember = KV<std::int64_t>{"ensembleMember"};
constexpr auto EnsembleSize = KV<std::int64_t>{"ensembleSize"};
constexpr auto EnsembleMemberKC = KV<std::int64_t>{"ensemble-member"};  // Kebap case
constexpr auto EnsembleSizeKC = KV<std::int64_t>{"ensemble-size"};      // Kebap case
constexpr auto OffsetToEndOf4DvarWindow = KV<std::int64_t>{"offsetToEndOf4DvarWindow"};
constexpr auto LengthOf4DvarWindow = KV<std::int64_t>{"lengthOf4DvarWindow"};

constexpr auto Anoffset = KV<std::int64_t>{"anoffset"};
constexpr auto Anlength = KV<std::int64_t>{"anlength"};

constexpr auto ComponentIndex = KV<std::int64_t>{"componentIndex"};
constexpr auto NumberOfComponents = KV<std::int64_t>{"numberOfComponents"};
constexpr auto ModelErrorType = KV<std::int64_t>{"modelErrorType"};
constexpr auto IterationNumber = KV<std::int64_t>{"iterationNumber"};
constexpr auto TotalNumberOfIterations = KV<std::int64_t>{"totalNumberOfIterations"};


// Eccodes grib reference date/time - direct setting (alternative to date & time)
constexpr auto Year = KV<std::int64_t>{"year"};
constexpr auto Month = KV<std::int64_t>{"month"};
constexpr auto Day = KV<std::int64_t>{"day"};
constexpr auto Hour = KV<std::int64_t>{"hour"};
constexpr auto Minute = KV<std::int64_t>{"minute"};
constexpr auto Second = KV<std::int64_t>{"second"};

constexpr auto ForecastTime = KV<std::int64_t>{"forecastTime"};

// Eccodes analysis date/time - direct setting (alternative to dateOfAnalysis & timeOfAnalysis) -- ONLY VALID FOR A
// SPECIFIC localDefinitionNumber
constexpr auto YearOfAnalysis = KV<std::int64_t>{"yearOfAnalysis"};
constexpr auto MonthOfAnalysis = KV<std::int64_t>{"monthOfAnalysis"};
constexpr auto DayOfAnalysis = KV<std::int64_t>{"dayOfAnalysis"};
constexpr auto HourOfAnalysis = KV<std::int64_t>{"hourOfAnalysis"};
constexpr auto MinuteOfAnalysis = KV<std::int64_t>{"minuteOfAnalysis"};
constexpr auto SecondOfAnalysis = KV<std::int64_t>{"secondOfAnalysis"};

// Eccodes grib2 stat
constexpr auto YearOfEndOfOverallTimeInterval = KV<std::int64_t>{"yearOfEndOfOverallTimeInterval"};
constexpr auto MonthOfEndOfOverallTimeInterval = KV<std::int64_t>{"monthOfEndOfOverallTimeInterval"};
constexpr auto DayOfEndOfOverallTimeInterval = KV<std::int64_t>{"dayOfEndOfOverallTimeInterval"};
constexpr auto HourOfEndOfOverallTimeInterval = KV<std::int64_t>{"hourOfEndOfOverallTimeInterval"};
constexpr auto MinuteOfEndOfOverallTimeInterval = KV<std::int64_t>{"minuteOfEndOfOverallTimeInterval"};
constexpr auto SecondOfEndOfOverallTimeInterval = KV<std::int64_t>{"secondOfEndOfOverallTimeInterval"};
constexpr auto TypeOfStatisticalProcessing = KV<std::string>{"typeOfStatisticalProcessing"};
constexpr auto LengthOfTimeRange = KV<std::int64_t>{"lengthOfTimeRange"};
constexpr auto IndicatorOfUnitForTimeIncrement = KV<std::int64_t>{"indicatorOfUnitForTimeIncrement"};
constexpr auto TimeIncrement = KV<std::int64_t>{"timeIncrement"};

// Eccodes grib2 grid
constexpr auto UnstructuredGridType = KV<std::string>{"unstructuredGridType"};
constexpr auto UnstructuredGridSubtype = KV<std::string>{"unstructuredGridSubtype"};
constexpr auto UuidOfHGrid = KV<std::string>{"uuidOfHGrid"};

// Eccodes grib horizontal + vertial
constexpr auto Level = KV<std::int64_t>{"level"};
constexpr auto ScaledValueOfFirstFixedSurface = KV<std::int64_t>{"scaledValueOfFirstFixedSurface"};
constexpr auto ScaledValueOfSecondFixedSurface = KV<std::int64_t>{"scaledValueOfSecondFixedSurface"};
constexpr auto ScaleFactorOfFirstFixedSurface = KV<std::int64_t>{"scaleFactorOfFirstFixedSurface"};
constexpr auto ScaleFactorOfSecondFixedSurface = KV<std::int64_t>{"scaleFactorOfSecondFixedSurface"};
constexpr auto TypeOfFirstFixedSurface = KV<std::int64_t>{"typeOfFirstFixedSurface"};
constexpr auto TypeOfSecondFixedSurface = KV<std::int64_t>{"typeOfSecondFixedSurface"};

// Time model
constexpr auto StartTime = KV<std::int64_t>{"startTime"};
constexpr auto StartDate = KV<std::int64_t>{"startDate"};
constexpr auto PreviousTime = KV<std::int64_t>{"previousTime"};
constexpr auto PreviousDate = KV<std::int64_t>{"previousDate"};
constexpr auto CurrentTime = KV<std::int64_t>{"currentTime"};
constexpr auto CurrentDate = KV<std::int64_t>{"currentDate"};

constexpr auto SampleInterval = KV<std::int64_t>{"sampleInterval"};
constexpr auto SampleIntervalInSeconds = KV<std::int64_t>{"sampleIntervalInSeconds"};

// legacy & conversion
constexpr auto TimeStep = KV<std::int64_t>{"timeStep"};
constexpr auto Step = KV<std::int64_t>{"step"};
constexpr auto StepUnits = KV<std::int64_t>{"stepUnits"};
constexpr auto StepRange = KV<std::string>{"stepRange"};
constexpr auto StartStep = KV<std::int64_t>{"startStep"};
constexpr auto EndStep = KV<std::int64_t>{"endStep"};
constexpr auto DataTime = KV<std::int64_t>{"dataTime"};
constexpr auto DataDate = KV<std::int64_t>{"dataDate"};
constexpr auto IndicatorOfUnitForTimeRange = KV<std::int64_t>{"indicatorOfUnitForTimeRange"};

constexpr auto DateOfAnalysis = KV<std::int64_t>{"date-of-analysis"};
constexpr auto TimeOfAnalysis = KV<std::int64_t>{"time-of-analysis"};

// Statistic
constexpr auto Operation = KV<std::string>{"operation"};
constexpr auto RestartStep = KV<std::int64_t>{"restart-step"};
constexpr auto StepFrequency = KV<std::int64_t>{"step-frequency"};

// Healpix
constexpr auto Nside = KV<std::int64_t>{"Nside"};
constexpr auto OrderingConvention = KV<std::string>{"orderingConvention"};

// Spherical harmonics
constexpr KeyType SphericalHarmonics{"sphericalHarmonics"};
constexpr auto PentagonalResolutionParameterJ = KV<std::int64_t>{"pentagonalResolutionParameterJ"};
constexpr auto PentagonalResolutionParameterK = KV<std::int64_t>{"pentagonalResolutionParameterK"};
constexpr auto PentagonalResolutionParameterM = KV<std::int64_t>{"pentagonalResolutionParameterM"};
constexpr auto J{" = KV<std::int64_t>J"};
constexpr auto K{" = KV<std::int64_t>K"};
constexpr auto M{" = KV<std::int64_t>M"};
constexpr auto SubSetJ = KV<std::int64_t>{"subSetJ"};
constexpr auto SubSetK = KV<std::int64_t>{"subSetK"};
constexpr auto SubSetM = KV<std::int64_t>{"subSetM"};
constexpr auto Js = KV<std::int64_t>{"JS"};
constexpr auto Ks = KV<std::int64_t>{"KS"};
constexpr auto Ms = KV<std::int64_t>{"MS"};


// Regular ll
constexpr auto Ni = KV<std::int64_t>{"Ni"};
constexpr auto Nj = KV<std::int64_t>{"Nj"};

// Regular ll - mapped
constexpr auto North = KV<double>{"north"};
constexpr auto West = KV<double>{"west"};
constexpr auto South = KV<double>{"south"};
constexpr auto East = KV<double>{"east"};
constexpr auto WestEastIncrement = KV<double>{"west_east_increment"};
constexpr auto SouthNorthIncrement = KV<double>{"south_north_increment"};

// Regular ll - direct
constexpr auto LatitudeOfFirstGridPointInDegrees = KV<double>{"latitudeOfFirstGridPointInDegrees"};
constexpr auto LatitudeOfLastGridPointInDegrees = KV<double>{"latitudeOfLastGridPointInDegrees"};
constexpr auto LongitudeOfFirstGridPointInDegrees = KV<double>{"longitudeOfFirstGridPointInDegrees"};
constexpr auto LongitudeOfLastGridPointInDegrees = KV<double>{"longitudeOfLastGridPointInDegrees"};
constexpr auto JDirectionIncrementInDegrees = KV<double>{"jDirectionIncrementInDegrees"};
constexpr auto IDirectionIncrementInDegrees = KV<double>{"iDirectionIncrementInDegrees"};


//-----------------------------------------------------------------------------

}  // namespace multio::datamod::legacy


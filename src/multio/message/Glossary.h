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

#include "multio/message/DataModelling.h"

#include <chrono>
#include <string>


namespace multio::message {

//-----------------------------------------------------------------------------


using TimeDuration = std::variant<std::chrono::hours, std::chrono::seconds>;


namespace mapper {
struct TimeDurationMapper {
    std::string write(const TimeDuration&) const noexcept;


    template <typename T, std::enable_if_t<!std::is_same_v<std::decay_t<T>, std::string>, bool> = true>
    TimeDuration read(T&& t) const {
        throw MetadataException("TimeDuration must be an int or string, not " + util::typeToString<T>(), Here());
    }

    TimeDuration read(std::int64_t hours) const noexcept;
    TimeDuration read(const std::string& s) const;
};
struct ParamMapper {
    std::int64_t write(std::int64_t) const noexcept;
    std::int64_t read(std::int64_t) const noexcept;
    std::int64_t read(const std::string&) const;

    template <typename T, std::enable_if_t<!std::is_same_v<std::decay_t<T>, std::string>, bool> = true>
    std::int64_t read(T&& t) const {
        throw MetadataException("Param must be an int or string, not " + util::typeToString<T>(), Here());
    }
};
struct IntToBoolMapper {
    inline bool write(bool v) const noexcept { return v; };
    inline bool read(bool v) const noexcept { return v; };
    inline bool read(std::int64_t v) const { return v > 0; };
    template <typename T>
    bool read(T&& t) const {
        throw MetadataException("Value must be an int or bool, not " + util::typeToString<T>(), Here());
    }
};
}  // namespace mapper


//-----------------------------------------------------------------------------
// Mars Keys
//-----------------------------------------------------------------------------


enum class MarsKeys : std::uint64_t
{
    EXPVER,
    STREAM,
    TYPE,
    CLASS,
    PARAM,
    ORIGIN,
    ANOFFSET,
    PACKING,
    NUMBER,
    IDENT,
    INSTRUMENT,
    CHANNEL,
    CHEM,
    MODEL,
    LEVTYPE,
    LEVELIST,
    DIRECTION,
    FREQUENCY,
    DATE,
    TIME,
    STEP,
    TIMEPROC,
    HDATE,
    GRID,
    TRUNCATION,
};

template <>
struct KeySetDescription<MarsKeys> {
    static constexpr std::string_view name = "mars";

    static const auto& keys() {
        static const auto keys = std::make_tuple(
            describeKeyValue<MarsKeys::EXPVER, std::string, KVTag::Required>("expver"),
            describeKeyValue<MarsKeys::STREAM, std::string, KVTag::Required>("stream"),
            describeKeyValue<MarsKeys::TYPE, std::string, KVTag::Required>("type"),
            describeKeyValue<MarsKeys::CLASS, std::string, KVTag::Required>("class"),

            describeKeyValue<MarsKeys::PARAM, std::int64_t, KVTag::Required>("param", mapper::ParamMapper{}),

            describeKeyValue<MarsKeys::ORIGIN, std::string, KVTag::Optional>("origin"),
            describeKeyValue<MarsKeys::ANOFFSET, std::int64_t, KVTag::Optional>("anoffset"),
            describeKeyValue<MarsKeys::PACKING, std::string, KVTag::Optional>("packing"),
            describeKeyValue<MarsKeys::NUMBER, std::int64_t, KVTag::Optional>("number"),
            describeKeyValue<MarsKeys::IDENT, std::int64_t, KVTag::Optional>("ident"),
            describeKeyValue<MarsKeys::INSTRUMENT, std::int64_t, KVTag::Optional>("instrument"),
            describeKeyValue<MarsKeys::CHANNEL, std::int64_t, KVTag::Optional>("channel"),
            describeKeyValue<MarsKeys::CHEM, std::int64_t, KVTag::Optional>("chem"),

            describeKeyValue<MarsKeys::MODEL, std::string, KVTag::Optional>("model"),
            describeKeyValue<MarsKeys::LEVTYPE, std::string, KVTag::Optional>("levtype"),

            describeKeyValue<MarsKeys::LEVELIST, std::int64_t, KVTag::Optional>("levelist"),
            describeKeyValue<MarsKeys::DIRECTION, std::int64_t, KVTag::Optional>("direction"),
            describeKeyValue<MarsKeys::FREQUENCY, std::int64_t, KVTag::Optional>("frequency"),
            describeKeyValue<MarsKeys::DATE, std::int64_t, KVTag::Required>("date"),
            describeKeyValue<MarsKeys::TIME, std::int64_t, KVTag::Required>("time"),
            describeKeyValue<MarsKeys::STEP, TimeDuration, KVTag::Required>("step", mapper::TimeDurationMapper{}),
            describeKeyValue<MarsKeys::TIMEPROC, TimeDuration, KVTag::Optional>("timeproc",
                                                                                mapper::TimeDurationMapper{}),
            describeKeyValue<MarsKeys::HDATE, std::int64_t, KVTag::Optional>("hdate"),

            describeKeyValue<MarsKeys::GRID, std::string, KVTag::Optional>("grid"),
            describeKeyValue<MarsKeys::TRUNCATION, std::int64_t, KVTag::Optional>("truncation"));
        return keys;
    }
};


using MarsKeySet = std::decay_t<decltype(keySet<MarsKeys>())>;
using MarsKeyValueSet = std::decay_t<decltype(reify(keySet<MarsKeys>()))>;


//-----------------------------------------------------------------------------
// MARS encoder hash keys
//-----------------------------------------------------------------------------

// TODO implement some utilites to exclude types from a list
using EncoderCacheMarsKeySet
    = std::decay_t<decltype(keySet<MarsKeys::EXPVER, MarsKeys::STREAM, MarsKeys::TYPE, MarsKeys::CLASS, MarsKeys::PARAM,
                                   MarsKeys::ORIGIN, MarsKeys::ANOFFSET, MarsKeys::PACKING, MarsKeys::NUMBER,
                                   MarsKeys::IDENT, MarsKeys::INSTRUMENT, MarsKeys::CHANNEL, MarsKeys::CHEM,
                                   MarsKeys::MODEL, MarsKeys::LEVTYPE, MarsKeys::LEVELIST,
                                   // MarsKeys::DIRECTION,
                                   // MarsKeys::FREQUENCY,
                                   MarsKeys::DATE, MarsKeys::TIME, MarsKeys::STEP, MarsKeys::TIMEPROC, MarsKeys::HDATE,
                                   MarsKeys::GRID, MarsKeys::TRUNCATION>())>;

using EncoderCacheMarsKeyValueSet = std::decay_t<decltype(reify(std::declval<EncoderCacheMarsKeySet>()))>;

// TODO put this logic in a customized validator on the keyset directly
// Usue this function to populate a cache
EncoderCacheMarsKeyValueSet getEncoderCacheKeys(const MarsKeyValueSet& mk);


//-----------------------------------------------------------------------------
// Parametrization keys
//-----------------------------------------------------------------------------


enum class MiscKeys : std::uint64_t
{
    TablesVersion,
    GeneratingProcessIdentifier,
    Typeofprocesseddata,
    EncodeStepZero,
    InitialStep,
    LengthOfTimeRange,
    LengthOfTimeStep,
    LengthOfTimeRangeInSeconds,
    LengthOfTimeStepInSeconds,
    ValuesScaleFactor,
    Pv,
    NumberOfMissingValues,
    ValueOfMissingValues,
    TypeOfEnsembleForecast,
    NumberOfForecastsInEnsemble,
    LengthOfTimeWindow,
    LengthOfTimeWindowInSeconds,
    BitsPerValue,
    PeriodMin,
    PeriodMax,
    WaveDirections,
    WaveFrequencies,
    SatelliteSeries,
    ScaleFactorOfCentralWavenumber,
    ScaledValueOfCentralWavenumber,

    // TBD - move to mars
    MethodNumber,
    SystemNumber
};

template <>
struct KeySetDescription<MiscKeys> {
    static constexpr std::string_view name = "misc";

    static const auto& keys() {
        static const auto keys = std::make_tuple(
            describeKeyValue<MiscKeys::TablesVersion, std::int64_t, KVTag::Optional>("tablesVersion"),
            describeKeyValue<MiscKeys::GeneratingProcessIdentifier, std::int64_t, KVTag::Optional>(
                "generatingProcessIdentifier"),
            describeKeyValue<MiscKeys::Typeofprocesseddata, std::int64_t, KVTag::Optional>("typeofprocesseddata"),
            describeKeyValue<MiscKeys::EncodeStepZero, bool, KVTag::Optional>("encodeStepZero",
                                                                              mapper::IntToBoolMapper{}),
            describeKeyValue<MiscKeys::InitialStep, std::int64_t, KVTag::Optional>("initialStep"),
            describeKeyValue<MiscKeys::LengthOfTimeRange, std::int64_t, KVTag::Optional>("lengthOfTimeRange"),
            describeKeyValue<MiscKeys::LengthOfTimeStep, std::int64_t, KVTag::Optional>("lengthOfTimeStep"),
            describeKeyValue<MiscKeys::LengthOfTimeRangeInSeconds, std::int64_t, KVTag::Optional>(
                "lengthOfTimeRangeInSeconds"),
            describeKeyValue<MiscKeys::LengthOfTimeStepInSeconds, std::int64_t, KVTag::Optional>(
                "lengthOfTimeStepInSeconds"),
            describeKeyValue<MiscKeys::ValuesScaleFactor, double, KVTag::Optional>("valuesScaleFactor"),
            describeKeyValue<MiscKeys::Pv, std::vector<double>, KVTag::Optional>("pv"),
            describeKeyValue<MiscKeys::NumberOfMissingValues, std::int64_t, KVTag::Optional>("numberOfMissingValues"),
            describeKeyValue<MiscKeys::ValueOfMissingValues, double, KVTag::Optional>("valueOfMissingValues"),
            describeKeyValue<MiscKeys::TypeOfEnsembleForecast, std::int64_t, KVTag::Optional>("typeOfEnsembleForecast"),
            describeKeyValue<MiscKeys::NumberOfForecastsInEnsemble, std::int64_t, KVTag::Optional>(
                "numberOfForecastsInEnsemble"),
            describeKeyValue<MiscKeys::LengthOfTimeWindow, std::int64_t, KVTag::Optional>("lengthOfTimeWindow"),
            describeKeyValue<MiscKeys::LengthOfTimeWindowInSeconds, std::int64_t, KVTag::Optional>(
                "lengthOfTimeWindowInSeconds"),
            describeKeyValue<MiscKeys::BitsPerValue, std::int64_t, KVTag::Optional>("bitsPerValue"),
            describeKeyValue<MiscKeys::PeriodMin, std::int64_t, KVTag::Optional>("periodMin"),
            describeKeyValue<MiscKeys::PeriodMax, std::int64_t, KVTag::Optional>("periodMax"),
            describeKeyValue<MiscKeys::WaveDirections, std::vector<double>, KVTag::Optional>("waveDirections"),
            describeKeyValue<MiscKeys::WaveFrequencies, std::vector<double>, KVTag::Optional>("waveFrequencies"),
            describeKeyValue<MiscKeys::SatelliteSeries, std::int64_t, KVTag::Optional>("satelliteSeries"),
            describeKeyValue<MiscKeys::ScaleFactorOfCentralWavenumber, std::int64_t, KVTag::Optional>(
                "scaleFactorOfCentralWavenumber"),
            describeKeyValue<MiscKeys::ScaledValueOfCentralWavenumber, std::int64_t, KVTag::Optional>(
                "scaledValueOfCentralWavenumber"),

            // TBD - move to marse
            describeKeyValue<MiscKeys::MethodNumber, std::int64_t, KVTag::Optional>("methodNumber"),
            describeKeyValue<MiscKeys::SystemNumber, std::int64_t, KVTag::Optional>("systemNumber"));
        return keys;
    }
};


//-----------------------------------------------------------------------------
// Geometry keys - gg
//-----------------------------------------------------------------------------

enum class GeoGG : std::uint64_t
{
    TruncateDegrees,
    NumberOfPointsAlongAMeridian,
    NumberOfParallelsBetweenAPoleAndTheEquator,
    LatitudeOfFirstGridPointInDegrees,
    LongitudeOfFirstGridPointInDegrees,
    LatitudeOfLastGridPointInDegrees,
    LongitudeOfLastGridPointInDegrees,
    Pl
};

template <>
struct KeySetDescription<GeoGG> {
    static constexpr std::string_view name = "geo-gg";

    static const auto& keys() {
        static const auto keys = std::make_tuple(
            describeKeyValue<GeoGG::TruncateDegrees, std::int64_t, KVTag::Optional>("truncateDegrees"),
            describeKeyValue<GeoGG::NumberOfPointsAlongAMeridian, std::int64_t, KVTag::Required>(
                "numberOfPointsAlongAMeridian"),
            describeKeyValue<GeoGG::NumberOfParallelsBetweenAPoleAndTheEquator, std::int64_t, KVTag::Optional>(
                "numberOfParallelsBetweenAPoleAndTheEquator"),
            describeKeyValue<GeoGG::LatitudeOfFirstGridPointInDegrees, double, KVTag::Required>(
                "latitudeOfFirstGridPointInDegrees"),
            describeKeyValue<GeoGG::LongitudeOfFirstGridPointInDegrees, double, KVTag::Required>(
                "longitudeOfFirstGridPointInDegrees"),
            describeKeyValue<GeoGG::LatitudeOfLastGridPointInDegrees, double, KVTag::Required>(
                "latitudeOfLastGridPointInDegrees"),
            describeKeyValue<GeoGG::LongitudeOfLastGridPointInDegrees, double, KVTag::Required>(
                "longitudeOfLastGridPointInDegrees"),
            describeKeyValue<GeoGG::Pl, std::vector<std::int64_t>, KVTag::Optional>("pl"));
        return keys;
    }
};

//-----------------------------------------------------------------------------
// Geometry keys - sh
//-----------------------------------------------------------------------------

enum class GeoSH : std::uint64_t
{
    PentagonalResolutionParameterJ,
    PentagonalResolutionParameterK,
    PentagonalResolutionParameterM
};

template <>
struct KeySetDescription<GeoSH> {
    static constexpr std::string_view name = "geo-sh";

    static const auto& keys() {
        static const auto keys
            = std::make_tuple(describeKeyValue<GeoSH::PentagonalResolutionParameterJ, std::int64_t, KVTag::Required>(
                                  "pentagonalResolutionParameterJ"),
                              describeKeyValue<GeoSH::PentagonalResolutionParameterK, std::int64_t, KVTag::Required>(
                                  "pentagonalResolutionParameterK"),
                              describeKeyValue<GeoSH::PentagonalResolutionParameterM, std::int64_t, KVTag::Required>(
                                  "pentagonalResolutionParameterM"));
        return keys;
    }
};

//-----------------------------------------------------------------------------
// Geometry keys - ll
//-----------------------------------------------------------------------------

// enum class GeoLL : std::uint64_t
// {
// };

// template <>
// struct KeySetDescription<GeoLL> {
//     static constexpr std::string_view name = "geo-ll";

//     static const auto& keys() {
//         static auto keys = std::make_tuple();
//         return keys;
//     }
// };


//-----------------------------------------------------------------------------
// Evaluate geometry from mars
//-----------------------------------------------------------------------------

enum class GridType : std::size_t
{
    GG,
    LL,
    SH
};

std::tuple<GridType, std::string> gridTypeAndScopeFromGrid(const std::string& grid);

template <typename KVS, typename Func>
decltype(auto) withScopedGeometryKeySet(const KVS& kvs, Func&& func) {
    const auto& grid = key<MarsKeys::GRID>(kvs);
    const auto& trunc = key<MarsKeys::TRUNCATION>(kvs);

    if (grid.isMissing() && trunc.isMissing()) {
        throw MetadataException(
            "Either mars key 'grid' (x)or 'truncation' must to be given to describe geometry - both are missing",
            Here());
    }
    if (!grid.isMissing() && !trunc.isMissing()) {
        throw MetadataException(
            "Either mars key 'grid' or 'truncation' needs to be given to describe geometry - both ore given", Here());
    }

    if (!grid.isMissing()) {
        auto [gridType, scope] = gridTypeAndScopeFromGrid(grid.get());

        switch (gridType) {
            case GridType::GG: {
                std::forward<Func>(func)(GridType::GG, keySet<GeoGG>().scoped(std::move(scope)));
                return;
            }
            // TODO uncomment once there are keys specified...
            // case GridType::LL: {
            //     std::forward<Func>(func)(GridType::LL, keySet<GeoLL>().scoped(std::move(scope)));
            //     return;
            // }
            default:
                throw MetadataException("Unhandled gridType", Here());
        }
    }
    else if (!trunc.isMissing()) {
        std::forward<Func>(func)(GridType::SH,
                                 keySet<GeoSH>().scoped(std::string("geo-TCO") + std::to_string(trunc.get())));
    }
}


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
    using KeyType = typename MetadataTypes::KeyType;

    template <typename ValueType, typename Mapper = void>
    using KV = KeyValueDescription<0, ValueType, KVTag::Required, Mapper>;

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

}  // namespace multio::message


template <>
struct std::hash<multio::message::TimeDuration> {
    std::size_t operator()(const multio::message::TimeDuration& td) const noexcept {
        return std::visit(
            eckit::Overloaded{[&](const std::chrono::hours& h) { return multio::util::hashCombine(h.count(), 'h'); },
                              [&](const std::chrono::seconds& s) { return multio::util::hashCombine(s.count(), 's'); }},
            td);
    }
};

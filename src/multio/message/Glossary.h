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

#include "multio/message/Metadata.h"
#include <type_traits>
#include <tuple>


namespace multio::message {


template <typename KeyType, typename ValueType, typename Mapper = void>
struct KeyValueDescription;

template <typename KeyType, typename ValueType>
struct KeyValueDescription<KeyType, ValueType, void> {
    KeyType key;

    operator KeyType&() { return key; }
    operator const KeyType&() const { return key; }
    operator std::string&() { return key; }
    operator const std::string&() const { return key; }

    template<typename MD, std::enable_if_t<std::is_base_of_v<BaseMetadata, std::decay_t<MD>>, bool> = true>
    decltype(auto) get(MD&& md) const {
        return std::forward<MD>(md).template get<ValueType>(key);;
    }
    template<typename MD, std::enable_if_t<std::is_base_of_v<BaseMetadata, std::decay_t<MD>>, bool> = true>
    decltype(auto) getOpt(MD&& md) const {
        return std::forward<MD>(md).template getOpt<ValueType>(key);;
    }

    template<typename MD, std::enable_if_t<std::is_base_of_v<BaseMetadata, std::decay_t<MD>>, bool> = true>
    decltype(auto) set(MD&& md, ValueType&& val) const {
        return std::forward<MD>(md).set(key, std::move(val));
    }
    template<typename MD, std::enable_if_t<std::is_base_of_v<BaseMetadata, std::decay_t<MD>>, bool> = true>
    decltype(auto) set(MD&& md, const ValueType& val) const {
        return std::forward<MD>(md).set(key, val);
    }

    template<typename MDV, std::enable_if_t<std::is_base_of_v<MetadataValue, std::decay_t<MDV>>, bool> = true>
    decltype(auto) get(MDV&& md) const {
        return std::forward<MDV>(md).template get<ValueType>();;
    }

};


template<typename KeyType, typename ValueType, typename Mapper>
struct KeyValueDescription {
    KeyType key;
    Mapper mapper;

    operator KeyType&() { return key; }
    operator const KeyType&() const { return key; }
    operator std::string&() { return key; }
    operator const std::string&() const { return key; }

    template<typename MD, std::enable_if_t<std::is_base_of_v<BaseMetadata, std::decay_t<MD>>, bool> = true>
    decltype(auto) get(MD&& md) const {
        return std::forward<MD>(md).visit(key, mapper);;
    }
    template<typename MD, std::enable_if_t<std::is_base_of_v<BaseMetadata, std::decay_t<MD>>, bool> = true>
    std::optional<ValueType> getOpt(MD&& md) const {
        if (auto search = std::forward<MD>(md).find(key); search != md.end()) {
            return search->second.visit(mapper);
        }
        return {};
    }

    template<typename MD, std::enable_if_t<std::is_base_of_v<BaseMetadata, std::decay_t<MD>>, bool> = true>
    decltype(auto) set(MD&& md, ValueType&& val) const {
        return std::forward<MD>(md).set(mapper(key), std::move(val));
    }
    template<typename MD, std::enable_if_t<std::is_base_of_v<BaseMetadata, std::decay_t<MD>>, bool> = true>
    decltype(auto) set(MD&& md, const ValueType& val) const {
        return std::forward<MD>(md).set(mapper(key), val);
    }

    template<typename MDV, std::enable_if_t<std::is_base_of_v<MetadataValue, std::decay_t<MDV>>, bool> = true>
    decltype(auto) get(MDV&& md) const {
        return std::forward<MDV>(md).visit(mapper);
    }

};


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

    template<typename ValueType, typename Mapper = void>
    using KV = KeyValueDescription<KeyType, ValueType, Mapper>;

    // template <typename ValidTypes>
    // using Description = KeyValueDescription<KeyType, ValidTypes>;

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




namespace Mtg2 {
    using KeyType = typename MetadataTypes::KeyType;

    template<typename ValueType, typename Mapper = void>
    using KV = KeyValueDescription<KeyType, ValueType, Mapper>;

    enum class Repres: long {
        GG,
        LL,
        SH,
    };
    Repres parseRepres(const std::string& repres);
    std::string toString(Repres);

    struct ParamMapper {
        std::int64_t operator()(std::int64_t) const noexcept;
        std::int64_t operator()(const std::string&) const;
        template <typename T>
        std::int64_t operator()(T&& t) const {
            throw MetadataException("Param must be an int or string", Here());
        }
    };
    struct IntToBoolMapper {
        inline bool operator()(bool v) const noexcept { return v; };
        inline bool operator()(std::int64_t v) const { return v > 0; };
        template <typename T>
        bool operator()(T&& t) const {
            throw MetadataException("Value must be an int or string", Here());
        }
    };

    namespace mars {
        // Model
        static const KV<std::string> expver{"expver"};
        static const KV<std::string> stream{"stream"};
        static const KV<std::string> type{"type"};
        static const KV<std::string> marsClass{"class"};

        static const KV<std::string> origin{"origin"};
        static const KV<std::int64_t> anoffset{"anoffset"};
        static const KV<std::string> packing{"packing"};
        static const KV<std::int64_t> number{"number"};
        static const KV<std::int64_t> ident{"ident"};
        static const KV<std::int64_t> instrument{"instrument"};
        static const KV<std::int64_t> channel{"channel"};
        static const KV<std::int64_t> chem{"chem"};
        static const KV<std::int64_t,ParamMapper> param{"param", ParamMapper{}};

        static const KV<std::string> model{"model"};
        static const KV<std::string> levtype{"levtype"};

        static const KV<std::int64_t> levelist{"levelist"};
        static const KV<std::int64_t> direction{"direction"};
        static const KV<std::int64_t> frequency{"frequency"};
        static const KV<std::int64_t> date{"date"};
        static const KV<std::int64_t> time{"time"};
        static const KV<std::int64_t> step{"step"};
        static const KV<std::int64_t> timespan{"timeproc"};
        static const KV<std::int64_t> hdate{"hdate"};

        static const KV<std::string> grid{"grid"};
    }

    namespace marsCustom {
        static const KV<std::string> gridName{"gridName"};
    }

    namespace marsLegacy {
        static const KV<std::string> repres{"repres"};
        static const KV<std::int64_t> truncation{"truncation"};
    }

    template<typename Func>
    void withMarsKeys(Func&& func) {
        func(mars::expver);
        func(mars::stream);
        func(mars::type);
        func(mars::marsClass);
        func(mars::origin);
        func(mars::anoffset);
        func(mars::packing);
        func(mars::number);
        func(mars::ident);
        func(mars::instrument);
        func(mars::channel);
        func(mars::chem);
        func(mars::param);
        func(mars::model);
        func(mars::levtype);
        func(mars::levelist);
        func(mars::direction);
        func(mars::frequency);
        func(mars::date);
        func(mars::time);
        func(mars::step);
        func(mars::timespan);
        func(mars::hdate);
        func(mars::grid);
        func(marsLegacy::repres);
    }


    template<typename TP>
    struct Prefixed {
        template<typename ... Args>
        Prefixed(const std::string& prefix, const std::string& key, Args&& ... args):
            plain{key, args...}, prefixed{prefix + std::string("-") + key, args...} {}

        TP plain;
        TP prefixed;
    };

    namespace misc {
        static const std::string prefix{"misc"};

        static const Prefixed<KV<std::int64_t>> tablesVersion{prefix, "tablesVersion"};
        static const Prefixed<KV<std::int64_t>> generatingProcessIdentifier{prefix, "generatingProcessIdentifier"};
        static const Prefixed<KV<std::int64_t>> typeofprocesseddata{prefix, "typeofprocesseddata"};
        static const Prefixed<KV<bool, IntToBoolMapper>> encodeStepZero{prefix, "encodeStepZero", IntToBoolMapper{}};
        static const Prefixed<KV<std::int64_t>> initialStep{prefix, "initialStep"};
        static const Prefixed<KV<std::int64_t>> lengthOfTimeRange{prefix, "lengthOfTimeRange"};
        static const Prefixed<KV<std::int64_t>> lengthOfTimeStep{prefix, "lengthOfTimeStep"};
        static const Prefixed<KV<std::int64_t>> lengthOfTimeRangeInSeconds{prefix, "lengthOfTimeRangeInSeconds"};
        static const Prefixed<KV<std::int64_t>> lengthOfTimeStepInSeconds{prefix, "lengthOfTimeStepInSeconds"};
        static const Prefixed<KV<double>> valuesScaleFactor{prefix, "valuesScaleFactor"};
        static const Prefixed<KV<std::vector<double>>> pv{prefix, "pv"};
        static const Prefixed<KV<std::int64_t>> numberOfMissingValues{prefix, "numberOfMissingValues"};
        static const Prefixed<KV<double>> valueOfMissingValues{prefix, "valueOfMissingValues"};
        static const Prefixed<KV<std::int64_t>> typeOfEnsembleForecast{prefix, "typeOfEnsembleForecast"};
        static const Prefixed<KV<std::int64_t>> numberOfForecastsInEnsemble{prefix, "numberOfForecastsInEnsemble"};
        static const Prefixed<KV<std::int64_t>> lengthOfTimeWindow{prefix, "lengthOfTimeWindow"};
        static const Prefixed<KV<std::int64_t>> lengthOfTimeWindowInSeconds{prefix, "lengthOfTimeWindowInSeconds"};
        static const Prefixed<KV<std::int64_t>> bitsPerValue{prefix, "bitsPerValue"};
        static const Prefixed<KV<std::int64_t>> periodMin{prefix, "periodMin"};
        static const Prefixed<KV<std::int64_t>> periodMax{prefix, "periodMax"};
        static const Prefixed<KV<std::vector<double>>> waveDirections{prefix, "waveDirections"};
        static const Prefixed<KV<std::vector<double>>> waveFrequencies{prefix, "waveFrequencies"};
        static const Prefixed<KV<std::int64_t>> satelliteSeries{prefix, "satelliteSeries"};
        static const Prefixed<KV<std::int64_t>> scaleFactorOfCentralWavenumber{prefix, "scaleFactorOfCentralWavenumber"};
        static const Prefixed<KV<std::int64_t>> scaledValueOfCentralWavenumber{prefix, "scaledValueOfCentralWavenumber"};

        // TBD - move to marse
        static const Prefixed<KV<std::int64_t>> methodNumber{prefix, "methodNumber"};
        static const Prefixed<KV<std::int64_t>> systemNumber{prefix, "systemNumber"};
    }

    template<typename Func>
    void withParametrizationKeys(Func&& func) {
        func(misc::tablesVersion);
        func(misc::generatingProcessIdentifier);
        func(misc::typeofprocesseddata);
        func(misc::encodeStepZero);
        func(misc::initialStep);
        func(misc::lengthOfTimeRange);
        func(misc::lengthOfTimeStep);
        func(misc::lengthOfTimeRangeInSeconds);
        func(misc::lengthOfTimeStepInSeconds);
        func(misc::valuesScaleFactor);
        func(misc::pv);
        func(misc::numberOfMissingValues);
        func(misc::valueOfMissingValues);
        func(misc::typeOfEnsembleForecast);
        func(misc::numberOfForecastsInEnsemble);
        func(misc::lengthOfTimeWindow);
        func(misc::lengthOfTimeWindowInSeconds);
        func(misc::bitsPerValue);
        func(misc::periodMin);
        func(misc::periodMax);
        func(misc::waveDirections);
        func(misc::waveFrequencies);
        func(misc::satelliteSeries);
        func(misc::scaleFactorOfCentralWavenumber);
        func(misc::scaledValueOfCentralWavenumber);

        func(misc::methodNumber);
        func(misc::systemNumber);
    }

    namespace gg {
        static const KV<std::int64_t> truncateDegrees{"truncateDegrees"};
        static const KV<std::int64_t> numberOfPointsAlongAMeridian{"numberOfPointsAlongAMeridian"};
        static const KV<std::int64_t> numberOfParallelsBetweenAPoleAndTheEquator{"numberOfParallelsBetweenAPoleAndTheEquator"};
        static const KV<double> latitudeOfFirstGridPointInDegrees{"latitudeOfFirstGridPointInDegrees"};
        static const KV<double> longitudeOfFirstGridPointInDegrees{"longitudeOfFirstGridPointInDegrees"};
        static const KV<double> latitudeOfLastGridPointInDegrees{"latitudeOfLastGridPointInDegrees"};
        static const KV<double> longitudeOfLastGridPointInDegrees{"longitudeOfLastGridPointInDegrees"};
        static const KV<std::vector<std::int64_t>> pl{"pl"};
    }

    namespace sh {
        static const KV<std::int64_t> pentagonalResolutionParameterJ{"pentagonalResolutionParameterJ"};
        static const KV<std::int64_t> pentagonalResolutionParameterK{"pentagonalResolutionParameterK"};
        static const KV<std::int64_t> pentagonalResolutionParameterM{"pentagonalResolutionParameterM"};
    }

    template<typename Func>
    void withGGKeys(Func&& func) {
        func(gg::truncateDegrees);
        func(gg::numberOfPointsAlongAMeridian);
        func(gg::numberOfParallelsBetweenAPoleAndTheEquator);
        func(gg::latitudeOfFirstGridPointInDegrees);
        func(gg::longitudeOfFirstGridPointInDegrees);
        func(gg::latitudeOfLastGridPointInDegrees);
        func(gg::longitudeOfLastGridPointInDegrees);
        func(gg::pl);
    }

    template<typename Func>
    void withLLKeys(Func&& func) {
    }

    template<typename Func>
    void withSHKeys(Func&& func) {
        func(sh::pentagonalResolutionParameterJ);
        func(sh::pentagonalResolutionParameterK);
        func(sh::pentagonalResolutionParameterM);
    }

    std::tuple<Repres, std::string> represAndPrefixFromGridName(const std::string& gridName);

    template<typename Func>
    void withGeometryKeys(Repres repres, Func&& func) {
        switch (repres) {
            case Repres::GG: {
                withGGKeys(std::forward<Func>(func));
                return;
            }
            case Repres::LL: {
                withLLKeys(std::forward<Func>(func));
                return;
            }
            case Repres::SH: {
                withSHKeys(std::forward<Func>(func));
                return;
            }
        }
    }
};

//-----------------------------------------------------------------------------

}  // namespace multio::message

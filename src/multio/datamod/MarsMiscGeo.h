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

#include "multio/datamod/Glossary.h"
#include "multio/datamod/GribKeys.h"
#include "multio/datamod/MarsKeys.h"
#include "multio/datamod/core/Compare.h"
#include "multio/datamod/core/DataModellingException.h"
#include "multio/datamod/core/EntryDef.h"
#include "multio/datamod/core/Hash.h"
#include "multio/datamod/core/Print.h"
#include "multio/datamod/core/Record.h"

#include "multio/util/Print.h"


#include <sstream>
#include <variant>


namespace multio::datamod {

/// \defgroup datamod_models_mars_records Mars records
/// \ingroup datamod_models_mars
///
/// # Mars record
///
/// ## TO BE DONE
/// TODO pgeier ... need to find a good way to handle doxygen sphinx and breath for these things

//-----------------------------------------------------------------------------
// Rough categorization on keys that stay mostly constant for a model run
//-----------------------------------------------------------------------------

struct MarsGeneral {
    EntryType_t<decltype(ORIGIN)> origin;
    EntryType_t<decltype(CLASS)> klass;
    EntryType_t<decltype(STREAM)> stream;
    EntryType_t<decltype(TYPE)> type;
    EntryType_t<decltype(EXPVER)> expver;
    EntryType_t<decltype(MODEL)> model;

    static constexpr std::string_view record_name_ = "mars-general";
    static constexpr auto record_entries_ = std::make_tuple(ORIGIN, CLASS, STREAM, TYPE, EXPVER, MODEL);
};

struct MarsSeasonal {
    EntryType_t<decltype(METHOD)> method;
    EntryType_t<decltype(SYSTEM)> system;

    static constexpr std::string_view record_name_ = "mars-seasonal";
    static constexpr auto record_entries_ = std::make_tuple(METHOD, SYSTEM);
};


struct MarsDestine {
    EntryType_t<decltype(DATASET)> dataset;
    EntryType_t<decltype(RESOLUTION)> resolution;
    EntryType_t<decltype(ACTIVITY)> activity;
    EntryType_t<decltype(EXPERIMENT)> experiment;
    EntryType_t<decltype(GENERATION)> generation;
    EntryType_t<decltype(REALIZATION)> realization;


    static constexpr std::string_view record_name_ = "mars-destine";
    static constexpr auto record_entries_
        = std::make_tuple(DATASET, RESOLUTION, ACTIVITY, EXPERIMENT, GENERATION, REALIZATION);
};


// Composed id object

struct MarsId : ComposedRecord<MarsGeneral, MarsSeasonal, MarsDestine> {
    static constexpr std::string_view record_name_ = "mars-id";
};


//-----------------------------------------------------------------------------
// Time axes
//-----------------------------------------------------------------------------

struct MarsTime {
    EntryType_t<decltype(DATE)> date;
    EntryType_t<decltype(TIME)> time;
    EntryType_t<decltype(STEP)> step;

    // Analysis
    EntryType_t<decltype(ANOFFSET)> anoffset;

    // Hindcast/reforecast dates...
    EntryType_t<decltype(HDATE)> hdate;
    EntryType_t<decltype(REFDATE)> refdate;

    EntryType_t<decltype(FCMONTH)> fcmonth;
    EntryType_t<decltype(FCPERIOD)> fcperiod;


    static constexpr std::string_view record_name_ = "mars-time";
    static constexpr auto record_entries_
        = std::make_tuple(DATE, TIME, STEP, ANOFFSET, HDATE, REFDATE, FCMONTH, FCPERIOD);
};


//-----------------------------------------------------------------------------
// Field identification and details
//-----------------------------------------------------------------------------

/// Mars field details
struct MarsFieldId {
    // Param
    EntryType_t<decltype(PARAM)> param;

    // Horizontal & Vertical
    EntryType_t<decltype(LEVTYPE)> levtype;

    // Additional statistical properties
    EntryType_t<decltype(TIMESPAN)> timespan;
    EntryType_t<decltype(STATTYPE)> stattype;


    static constexpr std::string_view record_name_ = "mars-field-id";
    static constexpr auto record_entries_ = std::make_tuple(PARAM, LEVTYPE, TIMESPAN, STATTYPE);
};


// Misc Details
struct MarsFieldDetails {
    // More product information
    EntryType_t<decltype(LEVELIST)> levelist;

    // Ensemble
    EntryType_t<decltype(NUMBER)> number;

    // Chemical
    EntryType_t<decltype(CHEM)> chem;

    // Aerosol
    EntryType_t<decltype(WAVELENGTH)> wavelength;

    // Wave
    EntryType_t<decltype(DIRECTION)> direction;
    EntryType_t<decltype(FREQUENCY)> frequency;

    // Sensitivity forecast
    EntryType_t<decltype(ITERATION)> iteration;
    EntryType_t<decltype(DIAGNOSTIC)> diagnostic;

    static constexpr std::string_view record_name_ = "mars-field-details";
    static constexpr auto record_entries_
        = std::make_tuple(LEVELIST, NUMBER, CHEM, WAVELENGTH, DIRECTION, FREQUENCY, ITERATION, DIAGNOSTIC);
};


struct MarsSatellite {
    EntryType_t<decltype(IDENT)> ident;
    EntryType_t<decltype(INSTRUMENT)> instrument;
    EntryType_t<decltype(CHANNEL)> channel;


    static constexpr std::string_view record_name_ = "mars-satellite";
    static constexpr auto record_entries_ = std::make_tuple(IDENT, INSTRUMENT, CHANNEL);
};


struct MarsField : ComposedRecord<MarsFieldId, MarsFieldDetails, MarsSatellite> {
    static constexpr std::string_view record_name_ = "mars-field";
};


//-----------------------------------------------------------------------------
// Additional information on data representationi and compression
//-----------------------------------------------------------------------------

struct MarsEncodingDetails {
    EntryType_t<decltype(GRID)> grid;
    EntryType_t<decltype(TRUNCATION)> truncation;

    EntryType_t<decltype(PACKING)> packing;


    static constexpr std::string_view record_name_ = "mars-encoding";
    static constexpr auto record_entries_ = std::make_tuple(GRID, TRUNCATION, PACKING);
};


//-----------------------------------------------------------------------------
// The final big composed record containing all MARS keys we use so far encoding
//-----------------------------------------------------------------------------

struct FullMarsRecord;

}  // namespace multio::datamod

template <>
struct multio::util::Print<multio::datamod::FullMarsRecord> : multio::datamod::PrintRecord {};


namespace multio::datamod {

struct FullMarsRecord : ComposedRecord<MarsId, MarsEncodingDetails, MarsField, MarsTime> {
    static constexpr std::string_view record_name_ = "mars";

    static void applyDefaults(FullMarsRecord& mars) {
        const auto& grid = mars.grid;
        const auto& trunc = mars.truncation;

        if (!grid.isSet() && !trunc.isSet()) {
            std::ostringstream oss;
            oss << "Either mars key 'grid' (x)or 'truncation' need to be given to describe geometry - both are "
                   "missing: ";
            util::print(oss, mars);
            throw DataModellingException(oss.str(), Here());
        }
        if (grid.isSet() && trunc.isSet()) {
            std::ostringstream oss;
            oss << "Either mars key 'grid' or 'truncation' needs to be given to describe geometry - both are "
                   "given: ";
            util::print(oss, mars);
            throw DataModellingException(oss.str(), Here());
        }
    }
};

//-----------------------------------------------------------------------------


/// \defgroup datamod_models_misc Misc Keys
/// \ingroup datamod_models
///
/// # Additional Keys
///
/// This page explains the set of additional keys used
///
/// ## TO BE DONE

//-----------------------------------------------------------------------------
// Parametrization keys
//-----------------------------------------------------------------------------

constexpr auto InitialStep =               //
    EntryDef<std::int64_t>{"initialStep"}  //
        .withDefault(0)
        .withAccessor([](auto&& v) { return &v.initialStep; });

constexpr auto TimeIncrementInSeconds =               //
    EntryDef<std::int64_t>{"timeIncrementInSeconds"}  //
        .withDefault(3600)
        .withAccessor([](auto&& v) { return &v.timeIncrementInSeconds; });

constexpr auto LengthOfTimeWindow =               //
    EntryDef<std::int64_t>{"lengthOfTimeWindow"}  //
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.lengthOfTimeWindow; });

constexpr auto LengthOfTimeWindowInSeconds =               //
    EntryDef<std::int64_t>{"lengthOfTimeWindowInSeconds"}  //
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.lengthOfTimeWindowInSeconds; });

constexpr auto ScaleFactorOfWaveDirections =               //
    EntryDef<std::int64_t>{"scaleFactorOfWaveDirections"}  //
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.scaleFactorOfWaveDirections; });

constexpr auto ScaleFactorOfWaveFrequencies =               //
    EntryDef<std::int64_t>{"scaleFactorOfWaveFrequencies"}  //
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.scaleFactorOfWaveFrequencies; });

constexpr auto WaveDirections =                      //
    EntryDef<std::vector<double>>{"waveDirections"}  //
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.waveDirections; });

constexpr auto WaveFrequencies =                      //
    EntryDef<std::vector<double>>{"waveFrequencies"}  //
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.waveFrequencies; });

// BitsPerValue defined in GribKeys

// LaplacianOperator defined in GribKeys

struct MiscRecord {
    EntryType_t<decltype(TablesVersion)> tablesVersion;
    EntryType_t<decltype(GeneratingProcessIdentifier)> generatingProcessIdentifier;
    EntryType_t<decltype(TypeOfProcessedDataEntry)> typeOfProcessedData;
    EntryType_t<decltype(InitialStep)> initialStep;
    EntryType_t<decltype(TimeIncrementInSeconds)> timeIncrementInSeconds;
    EntryType_t<decltype(LengthOfTimeWindow)> lengthOfTimeWindow;
    EntryType_t<decltype(LengthOfTimeWindowInSeconds)> lengthOfTimeWindowInSeconds;
    EntryType_t<decltype(BitmapPresent)> bitmapPresent;
    EntryType_t<decltype(MissingValue)> missingValue;
    EntryType_t<decltype(TypeOfEnsembleForecast)> typeOfEnsembleForecast;
    EntryType_t<decltype(NumberOfForecastsInEnsemble)> numberOfForecastsInEnsemble;
    EntryType_t<decltype(SatelliteSeries)> satelliteSeries;
    EntryType_t<decltype(ScaleFactorOfCentralWaveNumber)> scaleFactorOfCentralWaveNumber;
    EntryType_t<decltype(ScaledValueOfCentralWaveNumber)> scaledValueOfCentralWaveNumber;
    EntryType_t<decltype(Pv)> pv;
    Entry<int64_t> scaleFactorOfWaveDirections;
    Entry<int64_t> scaleFactorOfWaveFrequencies;
    Entry<std::vector<double>> waveDirections;
    Entry<std::vector<double>> waveFrequencies;
    EntryType_t<decltype(BitsPerValue)> bitsPerValue;
    EntryType_t<decltype(LaplacianOperator)> laplacianOperator;


    static constexpr std::string_view record_name_ = "misc";
    static constexpr auto record_entries_ = std::make_tuple(
        TablesVersion, GeneratingProcessIdentifier, TypeOfProcessedDataEntry, InitialStep, TimeIncrementInSeconds,
        LengthOfTimeWindow, LengthOfTimeWindowInSeconds, BitmapPresent, MissingValue, TypeOfEnsembleForecast,
        NumberOfForecastsInEnsemble, SatelliteSeries, ScaleFactorOfCentralWaveNumber, ScaledValueOfCentralWaveNumber,
        Pv, entryDef("scaleFactorOfWaveDirections", &MiscRecord::scaleFactorOfWaveDirections).tagOptional(),
        entryDef("scaleFactorOfWaveFrequencies", &MiscRecord::scaleFactorOfWaveFrequencies).tagOptional(),
        entryDef("waveDirections", &MiscRecord::waveDirections).tagOptional(),
        entryDef("waveFrequencies", &MiscRecord::waveFrequencies).tagOptional(), BitsPerValue, LaplacianOperator);
};


//-----------------------------------------------------------------------------
// Geometry keys - gg
//-----------------------------------------------------------------------------

struct GeoRegularGGRecord {
    Entry<std::int64_t> numberOfPointsAlongAMeridian;
    Entry<std::int64_t> numberOfPointsAlongAParallel;
    Entry<std::int64_t> numberOfParallelsBetweenAPoleAndTheEquator;
    Entry<double> latitudeOfFirstGridPointInDegrees;
    Entry<double> longitudeOfFirstGridPointInDegrees;
    Entry<double> latitudeOfLastGridPointInDegrees;
    Entry<double> longitudeOfLastGridPointInDegrees;

    Entry<double> iDirectionIncrementInDegrees;

    Entry<std::int64_t> shapeOfTheEarth;


    static constexpr std::string_view record_name_ = "geo-regular-gg";
    static constexpr auto record_entries_ = std::make_tuple(
        entryDef("numberOfPointsAlongAMeridian", &GeoRegularGGRecord::numberOfPointsAlongAMeridian),
        entryDef("numberOfPointsAlongAParallel", &GeoRegularGGRecord::numberOfPointsAlongAParallel),
        entryDef("numberOfParallelsBetweenAPoleAndTheEquator",
                 &GeoRegularGGRecord::numberOfParallelsBetweenAPoleAndTheEquator),
        entryDef("latitudeOfFirstGridPointInDegrees", &GeoRegularGGRecord::latitudeOfFirstGridPointInDegrees),
        entryDef("longitudeOfFirstGridPointInDegrees", &GeoRegularGGRecord::longitudeOfFirstGridPointInDegrees),
        entryDef("latitudeOfLastGridPointInDegrees", &GeoRegularGGRecord::latitudeOfLastGridPointInDegrees),
        entryDef("longitudeOfLastGridPointInDegrees", &GeoRegularGGRecord::longitudeOfLastGridPointInDegrees),
        entryDef("iDirectionIncrementInDegrees", &GeoRegularGGRecord::iDirectionIncrementInDegrees),
        entryDef("shapeOfTheEarth", &GeoRegularGGRecord::shapeOfTheEarth).withDefault(6));
};

struct GeoReducedGGRecord {
    Entry<std::int64_t> numberOfPointsAlongAMeridian;
    Entry<std::int64_t> numberOfParallelsBetweenAPoleAndTheEquator;
    Entry<double> latitudeOfFirstGridPointInDegrees;
    Entry<double> longitudeOfFirstGridPointInDegrees;
    Entry<double> latitudeOfLastGridPointInDegrees;
    Entry<double> longitudeOfLastGridPointInDegrees;

    Entry<std::vector<std::int64_t>> pl;

    Entry<std::int64_t> shapeOfTheEarth;


    static constexpr std::string_view record_name_ = "geo-regular-gg";
    static constexpr auto record_entries_ = std::make_tuple(
        entryDef("numberOfPointsAlongAMeridian", &GeoReducedGGRecord::numberOfPointsAlongAMeridian),
        entryDef("numberOfParallelsBetweenAPoleAndTheEquator",
                 &GeoReducedGGRecord::numberOfParallelsBetweenAPoleAndTheEquator),
        entryDef("latitudeOfFirstGridPointInDegrees", &GeoReducedGGRecord::latitudeOfFirstGridPointInDegrees),
        entryDef("longitudeOfFirstGridPointInDegrees", &GeoReducedGGRecord::longitudeOfFirstGridPointInDegrees),
        entryDef("latitudeOfLastGridPointInDegrees", &GeoReducedGGRecord::latitudeOfLastGridPointInDegrees),
        entryDef("longitudeOfLastGridPointInDegrees", &GeoReducedGGRecord::longitudeOfLastGridPointInDegrees),
        entryDef("pl", &GeoReducedGGRecord::pl),
        entryDef("shapeOfTheEarth", &GeoReducedGGRecord::shapeOfTheEarth).withDefault(6));


    static void validateRecord(const GeoReducedGGRecord& g) {
        if (g.pl.get().size() != g.numberOfParallelsBetweenAPoleAndTheEquator.get()) {
            std::ostringstream oss;
            oss << "The size of the passed pl array is different from the numberOfParallelsBetweenAPoleAndTheEquator: "
                << g.pl.get().size() << " != " << g.numberOfParallelsBetweenAPoleAndTheEquator.get();
            throw DataModellingException(oss.str(), Here());
        }
    }
};


//
//-----------------------------------------------------------------------------
// Geometry keys - sh
//-----------------------------------------------------------------------------


struct GeoSHRecord {
    Entry<double> stretchingFactor;
    Entry<double> latitudeOfStretchingPoleInDegrees;
    Entry<double> longitudeOfStretchingPoleInDegrees;
    Entry<std::int64_t> pentagonalResolutionParameterJ;
    Entry<std::int64_t> pentagonalResolutionParameterK;
    Entry<std::int64_t> pentagonalResolutionParameterM;

    static constexpr std::string_view record_name_ = "geo-stretched-rotated-sh";
    static constexpr auto record_entries_ = std::make_tuple(
        entryDef("stretchingFactor", &GeoSHRecord::stretchingFactor).tagOptional(),
        entryDef("latitudeOfStretchingPoleInDegrees", &GeoSHRecord::latitudeOfStretchingPoleInDegrees).tagOptional(),
        entryDef("longitudeOfStretchingPoleInDegrees", &GeoSHRecord::longitudeOfStretchingPoleInDegrees).tagOptional(),
        entryDef("pentagonalResolutionParameterJ", &GeoSHRecord::pentagonalResolutionParameterJ),
        entryDef("pentagonalResolutionParameterK", &GeoSHRecord::pentagonalResolutionParameterK),
        entryDef("pentagonalResolutionParameterM", &GeoSHRecord::pentagonalResolutionParameterM));

    static void applyDefaults(GeoSHRecord& g) {}

    GridType gridType() const;

    static void validate(GeoSHRecord& g) {
        if (g.gridType() == GridType::StretchedRotatedSH) {
            // Check that both latitudeOfStretchingPoleInDegrees and longitudeOfStretchingPoleInDegrees are defined
            if (!g.latitudeOfStretchingPoleInDegrees.isSet()) {
                throw DataModellingException(
                    "latitudeOfStretchingPoleInDegrees is not set but required for stretched_rotated_sh", Here());
            }
            if (!g.longitudeOfStretchingPoleInDegrees.isSet()) {
                throw DataModellingException(
                    "longitudeOfStretchingPoleInDegrees is not set but required for stretched_rotated_sh", Here());
            }
        }
    }
};


//-----------------------------------------------------------------------------
// Geometry keys - ll
//-----------------------------------------------------------------------------

struct GeoRegularLLRecord {
    Entry<std::int64_t> numberOfPointsAlongAMeridian;
    Entry<std::int64_t> numberOfPointsAlongAParallel;

    Entry<double> latitudeOfFirstGridPointInDegrees;
    Entry<double> longitudeOfFirstGridPointInDegrees;
    Entry<double> latitudeOfLastGridPointInDegrees;
    Entry<double> longitudeOfLastGridPointInDegrees;

    Entry<double> iDirectionIncrementInDegrees;
    Entry<double> jDirectionIncrementInDegrees;

    Entry<std::int64_t> shapeOfTheEarth;

    static constexpr std::string_view record_name_ = "geo-regular-ll";
    static constexpr auto record_entries_ = std::make_tuple(
        entryDef("numberOfPointsAlongAMeridian", &GeoRegularLLRecord::numberOfPointsAlongAMeridian),
        entryDef("numberOfPointsAlongAParallel", &GeoRegularLLRecord::numberOfPointsAlongAParallel),
        entryDef("latitudeOfFirstGridPointInDegrees", &GeoRegularLLRecord::latitudeOfFirstGridPointInDegrees),
        entryDef("longitudeOfFirstGridPointInDegrees", &GeoRegularLLRecord::longitudeOfFirstGridPointInDegrees),
        entryDef("latitudeOfLastGridPointInDegrees", &GeoRegularLLRecord::latitudeOfLastGridPointInDegrees),
        entryDef("longitudeOfLastGridPointInDegrees", &GeoRegularLLRecord::longitudeOfLastGridPointInDegrees),
        entryDef("iDirectionIncrementInDegrees", &GeoRegularLLRecord::iDirectionIncrementInDegrees),
        entryDef("jDirectionIncrementInDegrees", &GeoRegularLLRecord::jDirectionIncrementInDegrees),
        entryDef("shapeOfTheEarth", &GeoRegularLLRecord::shapeOfTheEarth).withDefault(6));
};

//-----------------------------------------------------------------------------
// Geometry keys - HEALPix
//-----------------------------------------------------------------------------

struct GeoHEALPixRecord {
    Entry<GridType> gridType;

    Entry<std::int64_t> nside;
    Entry<std::string> orderingConvention;
    Entry<double> longitudeOfFirstGridPointInDegrees;

    static constexpr std::string_view record_name_ = "geo-healpix";
    static constexpr auto record_entries_ = std::make_tuple(
        entryDef("gridType", &GeoHEALPixRecord::gridType).tagDefaulted(), entryDef("nside", &GeoHEALPixRecord::nside),
        entryDef("orderingConvention", &GeoHEALPixRecord::orderingConvention),
        entryDef("longitudeOfFirstGridPointInDegrees", &GeoHEALPixRecord::longitudeOfFirstGridPointInDegrees));
};


//-----------------------------------------------------------------------------
// Evaluate geometry from mars
//-----------------------------------------------------------------------------

using Geometry
    = std::variant<GeoReducedGGRecord, GeoRegularGGRecord, GeoRegularLLRecord, GeoSHRecord, GeoHEALPixRecord>;
using ScopedGeometry
    = std::variant<ScopedRecord<GeoReducedGGRecord>, ScopedRecord<GeoRegularGGRecord>, ScopedRecord<GeoRegularLLRecord>,
                   ScopedRecord<GeoSHRecord>, ScopedRecord<GeoHEALPixRecord>>;

GridType gridTypeFromGeometry(const Geometry& geo);

ScopedGeometry getGeometryRecord(const FullMarsRecord& mars);


//-----------------------------------------------------------------------------


}  // namespace multio::datamod

namespace multio::util {
template <>
struct Print<multio::datamod::MiscRecord> : multio::datamod::PrintRecord {};
template <>
struct Print<multio::datamod::GeoReducedGGRecord> : multio::datamod::PrintRecord {};
template <>
struct Print<multio::datamod::GeoRegularGGRecord> : multio::datamod::PrintRecord {};
template <>
struct Print<multio::datamod::GeoRegularLLRecord> : multio::datamod::PrintRecord {};
template <>
struct Print<multio::datamod::GeoSHRecord> : multio::datamod::PrintRecord {};
template <>
struct Print<multio::datamod::GeoHEALPixRecord> : multio::datamod::PrintRecord {};
};  // namespace multio::util

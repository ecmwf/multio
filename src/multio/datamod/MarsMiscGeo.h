/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#pragma once

#include "multio/datamod/MarsKeys.h"
#include "multio/datamod/GribKeys.h"
#include "multio/datamod/MarsTypes.h"
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

struct MarsRecord {
    EntryType_t<decltype(ORIGIN)> origin;
    EntryType_t<decltype(CLASS)> klass;
    EntryType_t<decltype(STREAM)> stream;
    EntryType_t<decltype(TYPE)> type;
    EntryType_t<decltype(EXPVER)> expver;
    EntryType_t<decltype(PARAM)> param;
    EntryType_t<decltype(DATE)> date;
    EntryType_t<decltype(TIME)> time;
    EntryType_t<decltype(STEP)> step;
    EntryType_t<decltype(LEVTYPE)> levtype;
    EntryType_t<decltype(LEVELIST)> levelist;
    EntryType_t<decltype(MODEL)> model;
    EntryType_t<decltype(RESOLUTION)> resolution;
    EntryType_t<decltype(ACTIVITY)> activity;
    EntryType_t<decltype(EXPERIMENT)> experiment;
    EntryType_t<decltype(GENERATION)> generation;
    EntryType_t<decltype(REALIZATION)> realization;
    EntryType_t<decltype(TIMESPAN)> timespan;
    EntryType_t<decltype(ANOFFSET)> anoffset;
    EntryType_t<decltype(PACKING)> packing;
    EntryType_t<decltype(NUMBER)> number;
    EntryType_t<decltype(IDENT)> ident;
    EntryType_t<decltype(INSTRUMENT)> instrument;
    EntryType_t<decltype(CHANNEL)> channel;
    EntryType_t<decltype(CHEM)> chem;
    EntryType_t<decltype(WAVELENGTH)> wavelength;
    EntryType_t<decltype(DIRECTION)> direction;
    EntryType_t<decltype(FREQUENCY)> frequency;
    EntryType_t<decltype(HDATE)> hdate;
    EntryType_t<decltype(DATASET)> dataset;
    EntryType_t<decltype(METHOD)> method;
    EntryType_t<decltype(SYSTEM)> system;
    EntryType_t<decltype(GRID)> grid;
    EntryType_t<decltype(TRUNCATION)> truncation;
    EntryType_t<decltype(REPRES)> repres;

    static constexpr std::string_view record_name_ = "mars";
    static constexpr auto record_entries_ = std::make_tuple(
        ORIGIN, CLASS, STREAM, TYPE, EXPVER, PARAM, DATE, TIME, STEP, LEVTYPE, LEVELIST, MODEL, RESOLUTION, ACTIVITY,
        EXPERIMENT, GENERATION, REALIZATION, TIMESPAN, ANOFFSET, PACKING, NUMBER, IDENT, INSTRUMENT, CHANNEL, CHEM,
        WAVELENGTH, DIRECTION, FREQUENCY, HDATE, DATASET, METHOD, SYSTEM, GRID, TRUNCATION, REPRES);
};

}  // namespace multio::datamod

template <>
struct multio::util::Print<multio::datamod::MarsRecord> : multio::datamod::PrintRecord {};

namespace multio::datamod {

template <>
struct ApplyRecordDefaults<MarsRecord> {
    static void applyDefaults(MarsRecord& mars) {
        // TODO setting conditional defaults and perform validation
        const auto& grid = mars.grid;
        const auto& trunc = mars.truncation;
        auto& repres = mars.repres;

        if (grid.isUnset() && trunc.isUnset()) {
            std::ostringstream oss;
            oss << "Either mars key 'grid' (x)or 'truncation' need to be given to describe geometry - both are "
                   "missing: ";
            util::print(oss, mars);
            throw DataModellingException(oss.str(), Here());
        }
        if (!grid.isUnset() && !trunc.isUnset()) {
            std::ostringstream oss;
            oss << "Either mars key 'grid' or 'truncation' needs to be given to describe geometry - both are "
                   "given: ";
            util::print(oss, mars);
            throw DataModellingException(oss.str(), Here());
        }

        if (!grid.isUnset()) {
            auto detRepres = represFromGrid(grid.get());
            if (!repres.isUnset() && (detRepres != repres.get())) {
                std::ostringstream oss;
                oss << "Passed value for repres is ";
                util::print(oss, repres.get());
                oss << " but derived value  ";
                util::print(oss, detRepres);
                oss << " from grid " << grid.get();
                throw DataModellingException(oss.str(), Here());
            }
            repres.set(detRepres);
        }
        else if (!trunc.isUnset()) {
            auto detRepres = Repres::SH;
            if (!repres.isUnset() && (detRepres != repres.get())) {
                std::ostringstream oss;
                oss << "Passed value for repres is ";
                util::print(oss, repres.get());
                oss << " but derived value  ";
                util::print(oss, detRepres);
                oss << " from truncation " << std::to_string(trunc.get());
                throw DataModellingException(oss.str(), Here());
            }
            repres.set(detRepres);
        }
    }
};

}  // namespace multio::datamod


namespace multio::datamod {


//-----------------------------------------------------------------------------
// Parametrization keys
//-----------------------------------------------------------------------------

// Userfacing keys

// TablesVersion defined in GribKeys

// GeneratingProcessIdentifier defined in GribKeys

// TypeOfProcessedData defined in GribKeys

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

constexpr auto BitmapPresent =                                //
    EntryDef<bool, mapper::BoolMapper>{"bitmapPresent"}  //
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.bitmapPresent; });

constexpr auto MissingValue =         //
    EntryDef<double>{"missingValue"}  //
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.missingValue; });

constexpr auto TypeOfEnsembleForecast =               //
    EntryDef<std::int64_t>{"typeOfEnsembleForecast"}  //
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.typeOfEnsembleForecast; });

constexpr auto NumberOfForecastsInEnsemble =               //
    EntryDef<std::int64_t>{"numberOfForecastsInEnsemble"}  //
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.numberOfForecastsInEnsemble; });

constexpr auto SatelliteSeries =               //
    EntryDef<std::int64_t>{"satelliteSeries"}  //
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.satelliteSeries; });

constexpr auto ScaleFactorOfCentralWavenumber =               //
    EntryDef<std::int64_t>{"scaleFactorOfCentralWavenumber"}  //
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.scaleFactorOfCentralWavenumber; });

constexpr auto ScaledValueOfCentralWavenumber =               //
    EntryDef<std::int64_t>{"scaledValueOfCentralWavenumber"}  //
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.scaledValueOfCentralWavenumber; });

// Pv defined in GribKeys

constexpr auto WaveDirections =                      //
    EntryDef<std::vector<double>>{"waveDirections"}  //
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.waveDirections; });

constexpr auto WaveFrequencies =                      //
    EntryDef<std::vector<double>>{"waveFrequencies"}  //
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.waveFrequencies; });

constexpr auto BitsPerValue =               //
    EntryDef<std::int64_t>{"bitsPerValue"}  //
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.bitsPerValue; });

constexpr auto LaplacianOperator =               //
    EntryDef<std::int64_t>{"laplacianOperator"}  //
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.laplacianOperator; });

struct MiscRecord {
    EntryType_t<decltype(TablesVersion)> tablesVersion;
    EntryType_t<decltype(GeneratingProcessIdentifier)> generatingProcessIdentifier;
    EntryType_t<decltype(TypeOfProcessedData)> typeOfProcessedData;
    EntryType_t<decltype(InitialStep)> initialStep;
    EntryType_t<decltype(TimeIncrementInSeconds)> timeIncrementInSeconds;
    EntryType_t<decltype(LengthOfTimeWindow)> lengthOfTimeWindow;
    EntryType_t<decltype(LengthOfTimeWindowInSeconds)> lengthOfTimeWindowInSeconds;
    EntryType_t<decltype(BitmapPresent)> bitmapPresent;
    EntryType_t<decltype(MissingValue)> missingValue;
    EntryType_t<decltype(TypeOfEnsembleForecast)> typeOfEnsembleForecast;
    EntryType_t<decltype(NumberOfForecastsInEnsemble)> numberOfForecastsInEnsemble;
    EntryType_t<decltype(SatelliteSeries)> satelliteSeries;
    EntryType_t<decltype(ScaleFactorOfCentralWavenumber)> scaleFactorOfCentralWavenumber;
    EntryType_t<decltype(ScaledValueOfCentralWavenumber)> scaledValueOfCentralWavenumber;
    EntryType_t<decltype(Pv)> pv;
    EntryType_t<decltype(WaveDirections)> waveDirections;
    EntryType_t<decltype(WaveFrequencies)> waveFrequencies;
    EntryType_t<decltype(BitsPerValue)> bitsPerValue;
    EntryType_t<decltype(LaplacianOperator)> laplacianOperator;


    static constexpr std::string_view record_name_ = "misc";
    static constexpr auto record_entries_ = std::make_tuple(
        TablesVersion, GeneratingProcessIdentifier, TypeOfProcessedData, InitialStep, TimeIncrementInSeconds,
        LengthOfTimeWindow, LengthOfTimeWindowInSeconds, BitmapPresent, MissingValue, TypeOfEnsembleForecast,
        NumberOfForecastsInEnsemble, SatelliteSeries, ScaleFactorOfCentralWavenumber, ScaledValueOfCentralWavenumber,
        Pv, WaveDirections, WaveFrequencies, BitsPerValue, LaplacianOperator);
};


//-----------------------------------------------------------------------------
// Geometry keys - gg
//-----------------------------------------------------------------------------

struct GeoGGRecord {
    EntryType_t<decltype(TruncateDegrees)> truncateDegrees;
    EntryType_t<decltype(NumberOfPointsAlongAMeridian)> numberOfPointsAlongAMeridian;
    EntryType_t<decltype(NumberOfParallelsBetweenAPoleAndTheEquator)> numberOfParallelsBetweenAPoleAndTheEquator;
    EntryType_t<decltype(LatitudeOfFirstGridPointInDegrees)> latitudeOfFirstGridPointInDegrees;
    EntryType_t<decltype(LongitudeOfFirstGridPointInDegrees)> longitudeOfFirstGridPointInDegrees;
    EntryType_t<decltype(LatitudeOfLastGridPointInDegrees)> latitudeOfLastGridPointInDegrees;
    EntryType_t<decltype(LongitudeOfLastGridPointInDegrees)> longitudeOfLastGridPointInDegrees;
    EntryType_t<decltype(Pl)> pl;


    static constexpr std::string_view record_name_ = "geo-gg";
    static constexpr auto record_entries_
        = std::make_tuple(TruncateDegrees, NumberOfPointsAlongAMeridian, NumberOfParallelsBetweenAPoleAndTheEquator,
                          LatitudeOfFirstGridPointInDegrees, LongitudeOfFirstGridPointInDegrees,
                          LatitudeOfLastGridPointInDegrees, LongitudeOfLastGridPointInDegrees, Pl);
};

//
//-----------------------------------------------------------------------------
// Geometry keys - sh
//-----------------------------------------------------------------------------


struct GeoSHRecord {
    EntryType_t<decltype(PentagonalResolutionParameterJ)> pentagonalResolutionParameterJ;
    EntryType_t<decltype(PentagonalResolutionParameterK)> pentagonalResolutionParameterK;
    EntryType_t<decltype(PentagonalResolutionParameterM)> pentagonalResolutionParameterM;

    static constexpr std::string_view record_name_ = "geo-sh";
    static constexpr auto record_entries_ = std::make_tuple(
        PentagonalResolutionParameterJ, PentagonalResolutionParameterK, PentagonalResolutionParameterM);
};


//-----------------------------------------------------------------------------
// Geometry keys - ll
//-----------------------------------------------------------------------------

// TBD

struct GeoLLRecord {
    static constexpr std::string_view record_name_ = "geo-ll";
    static constexpr auto record_entries_ = std::make_tuple();
};

//-----------------------------------------------------------------------------
// Geometry keys - HEALPix
//-----------------------------------------------------------------------------

struct GeoHEALPixRecord {
    EntryType_t<decltype(NSide)> nside;
    EntryType_t<decltype(OrderingConvention)> orderingConvention;
    EntryType_t<decltype(LongitudeOfFirstGridPointInDegrees)> longitudeOfFirstGridPointInDegrees;

    static constexpr std::string_view record_name_ = "geo-healpix";
    static constexpr auto record_entries_
        = std::make_tuple(NSide, OrderingConvention, LongitudeOfFirstGridPointInDegrees.tagOptional());
};


//-----------------------------------------------------------------------------
// Evaluate geometry from mars
//-----------------------------------------------------------------------------

using Geometry = std::variant<GeoGGRecord, GeoLLRecord, GeoSHRecord, GeoHEALPixRecord>;
using ScopedGeometry = std::variant<ScopedRecord<GeoGGRecord>, ScopedRecord<GeoLLRecord>, ScopedRecord<GeoSHRecord>,
                                    ScopedRecord<GeoHEALPixRecord>>;

ScopedGeometry getGeometryRecord(const MarsRecord& mars);


//-----------------------------------------------------------------------------

}  // namespace multio::datamod

namespace multio::util {
template <>
struct Print<multio::datamod::MiscRecord> : multio::datamod::PrintRecord {};
template <>
struct Print<multio::datamod::GeoGGRecord> : multio::datamod::PrintRecord {};
template <>
struct Print<multio::datamod::GeoLLRecord> : multio::datamod::PrintRecord {};
template <>
struct Print<multio::datamod::GeoSHRecord> : multio::datamod::PrintRecord {};
template <>
struct Print<multio::datamod::GeoHEALPixRecord> : multio::datamod::PrintRecord {};
};  // namespace multio::util

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

#include "multio/datamod/DataModelling.h"
#include "multio/datamod/DataModellingException.h"
#include "multio/datamod/MarsTypes.h"
#include "multio/util/TypeTraits.h"

#include <sstream>


namespace multio::datamod {

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
    WAVELENGTH,
    MODEL,
    LEVTYPE,
    LEVELIST,
    DIRECTION,
    FREQUENCY,
    DATE,
    TIME,
    STEP,
    TIMESPAN,
    HDATE,
    DATASET,
    RESOLUTION,
    ACTIVITY,
    EXPERIMENT,
    GENERATION,
    REALIZATION,
    METHOD,
    SYSTEM,
    GRID,
    TRUNCATION,
    REPRES
};


MULTIO_KEY_SET_DESCRIPTION(
    MarsKeys,                                                                  //
    "mars",                                                                    //
                                                                               //
    KeyDef<MarsKeys::EXPVER, std::string>{"expver"},                           //
    KeyDef<MarsKeys::STREAM, std::string>{"stream"},                           //
    KeyDef<MarsKeys::TYPE, std::string>{"type"},                               //
    KeyDef<MarsKeys::CLASS, std::string>{"class"},                             //
    KeyDef<MarsKeys::PARAM, std::int64_t, mapper::ParamMapper>{"param"},       //
    KeyDef<MarsKeys::ORIGIN, Origin>{"origin"}.withDefault("ecmf"),       //
    KeyDef<MarsKeys::ANOFFSET, std::int64_t>{"anoffset"}.tagOptional(),        //
    KeyDef<MarsKeys::PACKING, std::string>{"packing"}.tagOptional(),           //
    KeyDef<MarsKeys::NUMBER, std::int64_t>{"number"}.tagOptional(),            //
    KeyDef<MarsKeys::IDENT, std::int64_t>{"ident"}.tagOptional(),              //
    KeyDef<MarsKeys::INSTRUMENT, std::int64_t>{"instrument"}.tagOptional(),    //
    KeyDef<MarsKeys::CHANNEL, std::int64_t>{"channel"}.tagOptional(),          //
    KeyDef<MarsKeys::CHEM, std::int64_t>{"chem"}.tagOptional(),                //
    KeyDef<MarsKeys::WAVELENGTH, std::int64_t>{"wavelength"}.tagOptional(),    //
    KeyDef<MarsKeys::MODEL, std::string>{"model"}.tagOptional(),               //
    KeyDef<MarsKeys::LEVTYPE, LevType>{"levtype"}.tagOptional(),               //
    KeyDef<MarsKeys::LEVELIST, std::int64_t>{"levelist"}.tagOptional(),        //
    KeyDef<MarsKeys::DIRECTION, std::int64_t>{"direction"}.tagOptional(),      //
    KeyDef<MarsKeys::FREQUENCY, std::int64_t>{"frequency"}.tagOptional(),      //
    KeyDef<MarsKeys::DATE, std::int64_t>{"date"},                              //
    KeyDef<MarsKeys::TIME, std::int64_t>{"time"},                              //
    KeyDef<MarsKeys::STEP, TimeDuration>{"step"},                              //
    KeyDef<MarsKeys::TIMESPAN, TimeDuration>{"timespan"}.tagOptional(),        //
    KeyDef<MarsKeys::HDATE, std::int64_t>{"hdate"}.tagOptional(),              //
    KeyDef<MarsKeys::DATASET, std::string>{"dataset"}.tagOptional(),           //
    KeyDef<MarsKeys::RESOLUTION, std::string>{"resolution"}.tagOptional(),     //
    KeyDef<MarsKeys::ACTIVITY, std::string>{"activity"}.tagOptional(),         //
    KeyDef<MarsKeys::EXPERIMENT, std::string>{"experiment"}.tagOptional(),     //
    KeyDef<MarsKeys::GENERATION, std::int64_t>{"generation"}.tagOptional(),    //
    KeyDef<MarsKeys::REALIZATION, std::int64_t>{"realization"}.tagOptional(),  //
    KeyDef<MarsKeys::METHOD, std::int64_t>{"method"}.tagOptional(),            //
    KeyDef<MarsKeys::SYSTEM, std::int64_t>{"system"}.tagOptional(),            //
    KeyDef<MarsKeys::GRID, std::string>{"grid"}.tagOptional(),                 //
    KeyDef<MarsKeys::TRUNCATION, std::int64_t>{"truncation"}.tagOptional(),    //
    // TODO this key has been modified and is used internally (with the encoder rules...) should not be handled as
    // official mars key
    KeyDef<MarsKeys::REPRES, Repres>{"repres"}.tagDefaulted().withDescription(
        "`repres` describes the type of representation (e.g. gaussian grid, longitude/latitude, spherical harmonics) "
        "without defining resolution. It can be derived from `grid` and `truncation`. If passed its value is compared "
        "against the derived value."));

using MarsKeySet = KeySet<MarsKeys>;
using MarsKeyValueSet = KeyValueSet<MarsKeySet>;


template <>
struct KeySetAlter<MarsKeySet> {
    static void alter(MarsKeyValueSet& mars) {
        // TODO setting conditional defaults and perform validation
        const auto& grid = key<MarsKeys::GRID>(mars);
        const auto& trunc = key<MarsKeys::TRUNCATION>(mars);
        auto& repres = key<MarsKeys::REPRES>(mars);

        if (grid.isMissing() && trunc.isMissing()) {
            std::ostringstream oss;
            oss << "Either mars key 'grid' (x)or 'truncation' need to be given to describe geometry - both are "
                   "missing: "
                << mars;
            throw DataModellingException(oss.str(), Here());
        }
        if (!grid.isMissing() && !trunc.isMissing()) {
            std::ostringstream oss;
            oss << "Either mars key 'grid' or 'truncation' needs to be given to describe geometry - both are "
                   "given: "
                << mars;
            throw DataModellingException(oss.str(), Here());
        }

        if (!grid.isMissing()) {
            auto detRepres = represFromGrid(grid.get());
            if (!repres.isMissing() && (detRepres != repres.get())) {
                std::ostringstream oss;
                oss << "Passed value for repres is " << repres.get() << " but derived value  " << detRepres
                    << " from grid " << grid.get();
                throw DataModellingException(oss.str(), Here());
            }
            repres.set(detRepres);
        }
        else if (!trunc.isMissing()) {
            auto detRepres = Repres::SH;
            if (!repres.isMissing() && (detRepres != repres.get())) {
                std::ostringstream oss;
                oss << "Passed value for repres is " << repres.get() << " but derived value  " << detRepres
                    << " from truncation " << std::to_string(trunc.get());
                throw DataModellingException(oss.str(), Here());
            }
            repres.set(detRepres);
        }
    }
};


//-----------------------------------------------------------------------------
// MARS encoder hash keys
//-----------------------------------------------------------------------------

// TODO implement some utilites to exclude types from a list
using EncoderCacheMarsKeySet = CustomKeySet<MarsKeys::EXPVER, MarsKeys::STREAM, MarsKeys::TYPE, MarsKeys::CLASS,
                                            MarsKeys::PARAM, MarsKeys::ORIGIN, MarsKeys::ANOFFSET, MarsKeys::PACKING,
                                            MarsKeys::NUMBER, MarsKeys::IDENT, MarsKeys::INSTRUMENT, MarsKeys::CHANNEL,
                                            MarsKeys::CHEM, MarsKeys::MODEL, MarsKeys::LEVTYPE, MarsKeys::LEVELIST,
                                            // MarsKeys::DIRECTION,
                                            // MarsKeys::FREQUENCY,
                                            MarsKeys::DATE, MarsKeys::TIME, MarsKeys::STEP, MarsKeys::TIMESPAN,
                                            MarsKeys::HDATE, MarsKeys::GRID, MarsKeys::TRUNCATION>;

using EncoderCacheMarsKeyValueSet = KeyValueSet<EncoderCacheMarsKeySet>;

template <>
struct KeySetAlter<EncoderCacheMarsKeySet> {
    static void alter(EncoderCacheMarsKeyValueSet& cacheKeys) {

        const auto& levtype = key<MarsKeys::LEVTYPE>(cacheKeys);

        if (!levtype.isMissing() && levtype.get() == LevType::ML) {
            key<MarsKeys::LEVELIST>(cacheKeys).setMissing();
        }

        // Explicitly acquire because all the whole data structure is ment to be stored in a container
        acquire(cacheKeys);
    }
};


//-----------------------------------------------------------------------------
// Parametrization keys
//-----------------------------------------------------------------------------


// Userfacing keys

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
    ScaledValueOfCentralWavenumber
};


MULTIO_KEY_SET_DESCRIPTION(
    MiscKeys,                                                                                                  //
    "misc",                                                                                                    //
    KeyDef<MiscKeys::TablesVersion, std::int64_t>{"tablesVersion"}.tagOptional(),                              //
    KeyDef<MiscKeys::GeneratingProcessIdentifier, std::int64_t>{"generatingProcessIdentifier"}.tagOptional(),  //
    KeyDef<MiscKeys::Typeofprocesseddata, std::int64_t>{"typeofprocesseddata"}.tagOptional(),                  //
    KeyDef<MiscKeys::EncodeStepZero, bool, mapper::IntToBoolMapper>{"encodeStepZero"}.tagOptional(),           //
    KeyDef<MiscKeys::InitialStep, std::int64_t>{"initialStep"}.withDefault(0),                                 //
    KeyDef<MiscKeys::LengthOfTimeRange, std::int64_t>{"lengthOfTimeRange"}.tagOptional(),                      //
    KeyDef<MiscKeys::LengthOfTimeStep, std::int64_t>{"lengthOfTimeStep"}.tagOptional(),                        //
    KeyDef<MiscKeys::LengthOfTimeRangeInSeconds, std::int64_t>{"lengthOfTimeRangeInSeconds"}.tagOptional(),    //
    KeyDef<MiscKeys::LengthOfTimeStepInSeconds, std::int64_t>{"lengthOfTimeStepInSeconds"}.withDefault(3600),  //
    KeyDef<MiscKeys::ValuesScaleFactor, double>{"valuesScaleFactor"}.tagOptional(),                            //
    KeyDef<MiscKeys::Pv, std::vector<double>>{"pv"}.tagOptional(),                                             //
    KeyDef<MiscKeys::NumberOfMissingValues, std::int64_t>{"numberOfMissingValues"}.tagOptional(),              //
    KeyDef<MiscKeys::ValueOfMissingValues, double>{"valueOfMissingValues"}.tagOptional(),                      //
    KeyDef<MiscKeys::TypeOfEnsembleForecast, std::int64_t>{"typeOfEnsembleForecast"}.tagOptional(),            //
    KeyDef<MiscKeys::NumberOfForecastsInEnsemble, std::int64_t>{"numberOfForecastsInEnsemble"}.tagOptional(),  //
    KeyDef<MiscKeys::LengthOfTimeWindow, std::int64_t>{"lengthOfTimeWindow"}.tagOptional(),                    //
    KeyDef<MiscKeys::LengthOfTimeWindowInSeconds, std::int64_t>{"lengthOfTimeWindowInSeconds"}.tagOptional(),  //
    KeyDef<MiscKeys::BitsPerValue, std::int64_t>{"bitsPerValue"}.tagOptional(),                                //
    KeyDef<MiscKeys::PeriodMin, std::int64_t>{"periodMin"}.tagOptional().withDescription(
        "`periodMin` usually is depending on `paramId` and derived by ECCODES. in some cases it is "
        "passed through."),  //
    KeyDef<MiscKeys::PeriodMax, std::int64_t>{"periodMax"}.tagOptional().withDescription(
        "`periodMax` usually is depending on `paramId` and derived by ECCODES. In some cases it is "
        "passed through."),                                                                                          //
    KeyDef<MiscKeys::WaveDirections, std::vector<double>>{"waveDirections"}.tagOptional(),                           //
    KeyDef<MiscKeys::WaveFrequencies, std::vector<double>>{"waveFrequencies"}.tagOptional(),                         //
    KeyDef<MiscKeys::SatelliteSeries, std::int64_t>{"satelliteSeries"}.tagOptional(),                                //
    KeyDef<MiscKeys::ScaleFactorOfCentralWavenumber, std::int64_t>{"scaleFactorOfCentralWavenumber"}.tagOptional(),  //
    KeyDef<MiscKeys::ScaledValueOfCentralWavenumber, std::int64_t>{"scaledValueOfCentralWavenumber"}.tagOptional())  //


using MiscKeySet = KeySet<MiscKeys>;
using MiscKeyValueSet = KeyValueSet<MiscKeySet>;

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

MULTIO_KEY_SET_DESCRIPTION(
    GeoGG,                                                                                                    //
    "geo-gg",                                                                                                 //
                                                                                                              //
    KeyDef<GeoGG::TruncateDegrees, std::int64_t>{"truncateDegrees"}.tagOptional(),                            //
    KeyDef<GeoGG::NumberOfPointsAlongAMeridian, std::int64_t>{"numberOfPointsAlongAMeridian"}.tagOptional(),  //
    KeyDef<GeoGG::NumberOfParallelsBetweenAPoleAndTheEquator, std::int64_t>{
        "numberOfParallelsBetweenAPoleAndTheEquator"},                                                //
    KeyDef<GeoGG::LatitudeOfFirstGridPointInDegrees, double>{"latitudeOfFirstGridPointInDegrees"},    //
    KeyDef<GeoGG::LongitudeOfFirstGridPointInDegrees, double>{"longitudeOfFirstGridPointInDegrees"},  //
    KeyDef<GeoGG::LatitudeOfLastGridPointInDegrees, double>{"latitudeOfLastGridPointInDegrees"},      //
    KeyDef<GeoGG::LongitudeOfLastGridPointInDegrees, double>{"longitudeOfLastGridPointInDegrees"},    //
    KeyDef<GeoGG::Pl, std::vector<std::int64_t>>{"pl"}.tagOptional());                                //

//-----------------------------------------------------------------------------
// Geometry keys - sh
//-----------------------------------------------------------------------------

enum class GeoSH : std::uint64_t
{
    PentagonalResolutionParameterJ,
    PentagonalResolutionParameterK,
    PentagonalResolutionParameterM
};

MULTIO_KEY_SET_DESCRIPTION(
    GeoSH,                                                                                           //
    "geo-sh",                                                                                        //
                                                                                                     //
    KeyDef<GeoSH::PentagonalResolutionParameterJ, std::int64_t>{"pentagonalResolutionParameterJ"},   //
    KeyDef<GeoSH::PentagonalResolutionParameterK, std::int64_t>{"pentagonalResolutionParameterK"},   //
    KeyDef<GeoSH::PentagonalResolutionParameterM, std::int64_t>{"pentagonalResolutionParameterM"});  //

//-----------------------------------------------------------------------------
// Geometry keys - ll
//-----------------------------------------------------------------------------

// enum class GeoLL : std::uint64_t
// {
// };

// MULTIO_KEY_SET_DESCRIPTION(GeoLL,     //
//                            "geo-ll",  //
//                                       //

//-----------------------------------------------------------------------------
// Geometry keys - HEALPix
//-----------------------------------------------------------------------------

enum class GeoHEALPix : std::uint64_t
{
    NSide,
    OrderingConvention,
    LongitudeOfFirstGridPointInDegrees,
};

MULTIO_KEY_SET_DESCRIPTION(
    GeoHEALPix,  //
    "geo-sh",    //
                 //
    KeyDef<GeoHEALPix::NSide, std::int64_t>{"nside"},
    KeyDef<GeoHEALPix::OrderingConvention, std::string>{"orderingConvention"}.tagOptional(),
    KeyDef<GeoHEALPix::LongitudeOfFirstGridPointInDegrees, double>{"longitudeOfFirstGridPointInDegrees"}.tagOptional());


//-----------------------------------------------------------------------------
// Evaluate geometry from mars
//-----------------------------------------------------------------------------

using GeometryEnums = util::TypeList<GeoGG, GeoSH, GeoHEALPix>;
using GeometryKeySets = util::ApplyTypeList_t<std::variant, util::MapTypeList_t<KeySet, GeometryEnums>>;
using Geometry
    = util::ApplyTypeList_t<std::variant, util::MapTypeList_t<KeyValueSet, util::MapTypeList_t<KeySet, GeometryEnums>>>;

template<typename KVS, std::enable_if_t<std::is_same_v<std::decay_t<KVS>, MarsKeyValueSet>, bool> = true>
GeometryKeySets getGeometryKeySet(const KVS& mars) {
    const auto& grid = key<MarsKeys::GRID>(mars);
    const auto& trunc = key<MarsKeys::TRUNCATION>(mars);
    const auto& repres = key<MarsKeys::REPRES>(mars);

    switch (repres.get()) {
        case Repres::GG: {
            std::string scope = std::string("geo-") + grid.get();
            return keySet<GeoGG>().scoped(scope);
        }
        case Repres::HEALPix: {
            std::string scope = std::string("geo-") + grid.get();
            return keySet<GeoHEALPix>().scoped(scope);
        }
        case Repres::SH: {
            std::string scope = std::string("geo-TCO") + std::to_string(trunc.get());
            return keySet<GeoSH>().scoped(scope);
        }
        // TODO uncomment once there are keys specified...
        // case Repres::GG: {
        //     std::string scope = std::string("geo-") + grid.get();
        //     std::forward<Func>(func)(Repres::GG, scope, keySet<GeoGG>().scoped(scope));
        //     return;
        // }
        default:
            throw DataModellingException(
                std::string("getGeometryKeySet: Unhandled repres ") + Writer<Repres>::write(repres.get()), Here());
    }
}

template <typename Func>
decltype(auto) withScopedGeometryKeySet(const MarsKeyValueSet& mars, Func&& func) {
    const auto& grid = key<MarsKeys::GRID>(mars);
    const auto& trunc = key<MarsKeys::TRUNCATION>(mars);
    const auto& repres = key<MarsKeys::REPRES>(mars);

    switch (repres.get()) {
        case Repres::GG: {
            std::string scope = std::string("geo-") + grid.get();
            return std::forward<Func>(func)(Repres::GG, scope, keySet<GeoGG>().scoped(scope));
        }
        case Repres::HEALPix: {
            std::string scope = std::string("geo-") + grid.get();
            return std::forward<Func>(func)(Repres::HEALPix, scope, keySet<GeoHEALPix>().scoped(scope));
        }
        case Repres::SH: {
            std::string scope = std::string("geo-TCO") + std::to_string(trunc.get());
            return std::forward<Func>(func)(Repres::SH, scope, keySet<GeoSH>().scoped(scope));
        }
        // TODO uncomment once there are keys specified...
        // case Repres::GG: {
        //     std::string scope = std::string("geo-") + grid.get();
        //     std::forward<Func>(func)(Repres::GG, scope, keySet<GeoGG>().scoped(scope));
        //     return;
        // }
        default:
            throw DataModellingException(
                std::string("withScopedGeometryKeySet: Unhandled repres ") + Writer<Repres>::write(repres.get()),
                Here());
    }
}


//-----------------------------------------------------------------------------

}  // namespace multio::datamod


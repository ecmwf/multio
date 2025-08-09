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

#include "multio/datamod/core/TypeParserDumper.h"
#include "multio/util/Hash.h"
#include "multio/util/MioGribHandle.h"
#include "multio/util/Print.h"
#include "multio/util/TypeToString.h"


namespace multio::datamod {

//-----------------------------------------------------------------------------


enum class TypeOfLevel : std::size_t
{
    Surface,
    Troposphere,
    EntireAtmosphere,
    EntireOcean,
    EntireLake,
    CloudBase,
    CloudTop,
    IsothermZero,
    AdiabaticCondensation,
    MaxWind,
    Tropopause,
    Stratosphere,
    NominalTop,
    SeaBottom,
    Atmosphere,
    CumulonimbusBase,
    CumulonimbusTop,
    FreeConvection,
    ConvectiveCondensation,
    NeutralBuoyancy,
    MostUnstableParcel,
    MixedLayerParcel,
    LowestLevelOfCloudCoverExceedance,
    Isothermal,
    IsobaricInPa,
    IsobaricInhPa,
    IsobaricLayer,
    LowCloudLayer,
    MediumCloudLayer,
    HighCloudLayer,
    MeanSea,
    HeightAboveSea,
    HeightAboveSeaLayer,
    HeightAboveGround,
    HeightAboveGroundLayer,
    Sigma,
    SigmaLayer,
    Hybrid,
    HybridLayer,
    DepthBelowLand,
    DepthBelowLandLayer,
    Theta,
    ThetaLayer,
    PressureFromGround,
    PressureFromGroundLayer,
    PotentialVorticity,
    Eta,
    Snow,
    SnowLayer,
    MixedLayerDepthGeneric,
    HybridHeight,
    HybridPressure,
    GeneralVertical,
    GeneralVerticalLayer,
    Soil,
    SoilLayer,
    SeaIce,
    SeaIceLayer,
    DepthBelowSea,
    OceanSurface,
    DepthBelowSeaLayer,
    OceanSurfaceToBottom,
    LakeBottom,
    MixingLayer,
    OceanModel,
    OceanModelLayer,
    MixedLayerDepthByDensity,
    MixedLayerDepthByTemperature,
    MixedLayerDepthByDiffusivity,
    SnowTopOverIceOnWater,
    SnowLayerOverIceOnWater,
    IceTopOnWater,
    IceLayerOnWater,
    IceTopUnderSnowOnWater,
    IceLayerUnderSnowOnWater,
    IceBottomOnWater,
    IndefiniteSoilDepth,
    MeltPondTop,
    MeltPondBottom,
    EntireMeltPond,
    IceLayerAboveWaterSurface,
    WaterSurfaceToIsothermalOceanLayer,
    TotalSoilLayer,
    RootZone,
    Roof,
    RoofLayer,
    Wall,
    WallLayer,
    Road,
    RoadLayer,
    UrbanCanyon,
    AbstractSingleLevel,
    AbstractMultipleLevels,

    // HACKED to be removed after encoder migration
    HeightAboveGroundAt2m,
    HeightAboveGroundAt10m,
    HeightAboveSeaAt10m
};


// TOL defined with official integer representation
enum class TypeOfStatisticalProcessing : std::size_t
{
    Average = 0,
    Accumulation = 1,
    Maximum = 2,
    Minimum = 3,
    Difference = 4,
    RootMeanSquare = 5,
    StandardDeviation = 6,
    Covariance = 7,
    InverseDifference = 8,
    Ratio = 9,
    StandardizedAnomaly = 10,
    Summation = 11,
    ReturnPeriod = 12,
    Median = 13,
    Severity = 100,
    Mode = 101,
    IndexProcessing = 102,
};

}  // namespace multio::datamod


namespace multio::util {

template <>
struct Print<datamod::TypeOfLevel> {
    static void print(PrintStream&, const datamod::TypeOfLevel&);
};
template <>
struct Print<datamod::TypeOfStatisticalProcessing> {
    static void print(PrintStream&, const datamod::TypeOfStatisticalProcessing&);
};

template <>
struct TypeToString<datamod::TypeOfLevel> {
    std::string operator()() const { return "datamod::TypeOfLevel"; };
};
template <>
struct TypeToString<datamod::TypeOfStatisticalProcessing> {
    std::string operator()() const { return "datamod::TypeOfStatisticalProcessing"; };
};
}  // namespace multio::util

namespace multio::datamod {


template <>
struct DumpType<TypeOfLevel> {
    static std::string dump(TypeOfLevel);
};

template <>
struct ParseType<TypeOfLevel> {
    static inline TypeOfLevel parse(TypeOfLevel v) noexcept { return v; };
    static TypeOfLevel parse(const std::string& s);
};


template <>
struct DumpType<TypeOfStatisticalProcessing> {
    static std::string dump(TypeOfStatisticalProcessing);
};

// TBD - add dump spec for writing to grib (dump as int)
// template <>
// struct DumpType<TypeOfStatisticalProcessing, util::MioGribHandle> {
//     static std::int64_t dump(TypeOfStatisticalProcessing);
// };

template <>
struct ParseType<TypeOfStatisticalProcessing> {
    static inline TypeOfStatisticalProcessing parse(TypeOfStatisticalProcessing v) noexcept { return v; };
    static TypeOfStatisticalProcessing parse(const std::string& s);
    static TypeOfStatisticalProcessing parse(std::int64_t i);
};


}  // namespace multio::datamod


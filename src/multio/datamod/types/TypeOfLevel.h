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

#include "multio/datamod/core/TypeParserDumper.h"
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

}  // namespace multio::datamod


namespace multio::util {

template <>
struct Print<datamod::TypeOfLevel> {
    static void print(PrintStream&, const datamod::TypeOfLevel&);
};

template <>
struct TypeToString<datamod::TypeOfLevel> {
    std::string operator()() const { return "datamod::TypeOfLevel"; };
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


}  // namespace multio::datamod


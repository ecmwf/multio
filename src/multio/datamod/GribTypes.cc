/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "multio/datamod/GribTypes.h"
#include "multio/datamod/DataModellingException.h"


namespace multio::datamod {

std::string WriteSpec<TypeOfLevel>::write(TypeOfLevel v) {
    switch (v) {
        case TypeOfLevel::Surface:
            return "surface";
        case TypeOfLevel::Troposphere:
            return "troposphere";
        case TypeOfLevel::EntireAtmosphere:
            return "entireAtmosphere";
        case TypeOfLevel::EntireOcean:
            return "entireOcean";
        case TypeOfLevel::EntireLake:
            return "entireLake";
        case TypeOfLevel::CloudBase:
            return "cloudBase";
        case TypeOfLevel::CloudTop:
            return "cloudTop";
        case TypeOfLevel::IsothermZero:
            return "isothermZero";
        case TypeOfLevel::AdiabaticCondensation:
            return "adiabaticCondensation";
        case TypeOfLevel::MaxWind:
            return "maxWind";
        case TypeOfLevel::Tropopause:
            return "tropopause";
        case TypeOfLevel::Stratosphere:
            return "stratosphere";
        case TypeOfLevel::NominalTop:
            return "nominalTop";
        case TypeOfLevel::SeaBottom:
            return "seaBottom";
        case TypeOfLevel::Atmosphere:
            return "atmosphere";
        case TypeOfLevel::CumulonimbusBase:
            return "cumulonimbusBase";
        case TypeOfLevel::CumulonimbusTop:
            return "cumulonimbusTop";
        case TypeOfLevel::FreeConvection:
            return "freeConvection";
        case TypeOfLevel::ConvectiveCondensation:
            return "convectiveCondensation";
        case TypeOfLevel::NeutralBuoyancy:
            return "neutralBuoyancy";
        case TypeOfLevel::MostUnstableParcel:
            return "mostUnstableParcel";
        case TypeOfLevel::MixedLayerParcel:
            return "mixedLayerParcel";
        case TypeOfLevel::LowestLevelOfCloudCoverExceedance:
            return "lowestLevelOfCloudCoverExceedance";
        case TypeOfLevel::Isothermal:
            return "isothermal";
        case TypeOfLevel::IsobaricInPa:
            return "isobaricInPa";
        case TypeOfLevel::IsobaricInhPa:
            return "isobaricInhPa";
        case TypeOfLevel::IsobaricLayer:
            return "isobaricLayer";
        case TypeOfLevel::LowCloudLayer:
            return "lowCloudLayer";
        case TypeOfLevel::MediumCloudLayer:
            return "mediumCloudLayer";
        case TypeOfLevel::HighCloudLayer:
            return "highCloudLayer";
        case TypeOfLevel::MeanSea:
            return "meanSea";
        case TypeOfLevel::HeightAboveSea:
            return "heightAboveSea";
        case TypeOfLevel::HeightAboveSeaLayer:
            return "heightAboveSeaLayer";
        case TypeOfLevel::HeightAboveGround:
            return "heightAboveGround";
        case TypeOfLevel::HeightAboveGroundLayer:
            return "heightAboveGroundLayer";
        case TypeOfLevel::Sigma:
            return "sigma";
        case TypeOfLevel::SigmaLayer:
            return "sigmaLayer";
        case TypeOfLevel::Hybrid:
            return "hybrid";
        case TypeOfLevel::HybridLayer:
            return "hybridLayer";
        case TypeOfLevel::DepthBelowLand:
            return "depthBelowLand";
        case TypeOfLevel::DepthBelowLandLayer:
            return "depthBelowLandLayer";
        case TypeOfLevel::Theta:
            return "theta";
        case TypeOfLevel::ThetaLayer:
            return "thetaLayer";
        case TypeOfLevel::PressureFromGround:
            return "pressureFromGround";
        case TypeOfLevel::PressureFromGroundLayer:
            return "pressureFromGroundLayer";
        case TypeOfLevel::PotentialVorticity:
            return "potentialVorticity";
        case TypeOfLevel::Eta:
            return "eta";
        case TypeOfLevel::Snow:
            return "snow";
        case TypeOfLevel::SnowLayer:
            return "snowLayer";
        case TypeOfLevel::MixedLayerDepthGeneric:
            return "mixedLayerDepthGeneric";
        case TypeOfLevel::HybridHeight:
            return "hybridHeight";
        case TypeOfLevel::HybridPressure:
            return "hybridPressure";
        case TypeOfLevel::GeneralVertical:
            return "generalVertical";
        case TypeOfLevel::GeneralVerticalLayer:
            return "generalVerticalLayer";
        case TypeOfLevel::Soil:
            return "soil";
        case TypeOfLevel::SoilLayer:
            return "soilLayer";
        case TypeOfLevel::SeaIce:
            return "seaIce";
        case TypeOfLevel::SeaIceLayer:
            return "seaIceLayer";
        case TypeOfLevel::DepthBelowSea:
            return "depthBelowSea";
        case TypeOfLevel::OceanSurface:
            return "oceanSurface";
        case TypeOfLevel::DepthBelowSeaLayer:
            return "depthBelowSeaLayer";
        case TypeOfLevel::OceanSurfaceToBottom:
            return "oceanSurfaceToBottom";
        case TypeOfLevel::LakeBottom:
            return "lakeBottom";
        case TypeOfLevel::MixingLayer:
            return "mixingLayer";
        case TypeOfLevel::OceanModel:
            return "oceanModel";
        case TypeOfLevel::OceanModelLayer:
            return "oceanModelLayer";
        case TypeOfLevel::MixedLayerDepthByDensity:
            return "mixedLayerDepthByDensity";
        case TypeOfLevel::MixedLayerDepthByTemperature:
            return "mixedLayerDepthByTemperature";
        case TypeOfLevel::MixedLayerDepthByDiffusivity:
            return "mixedLayerDepthByDiffusivity";
        case TypeOfLevel::SnowTopOverIceOnWater:
            return "snowTopOverIceOnWater";
        case TypeOfLevel::SnowLayerOverIceOnWater:
            return "snowLayerOverIceOnWater";
        case TypeOfLevel::IceTopOnWater:
            return "iceTopOnWater";
        case TypeOfLevel::IceLayerOnWater:
            return "iceLayerOnWater";
        case TypeOfLevel::IceTopUnderSnowOnWater:
            return "iceTopUnderSnowOnWater";
        case TypeOfLevel::IceLayerUnderSnowOnWater:
            return "iceLayerUnderSnowOnWater";
        case TypeOfLevel::IceBottomOnWater:
            return "iceBottomOnWater";
        case TypeOfLevel::IndefiniteSoilDepth:
            return "indefiniteSoilDepth";
        case TypeOfLevel::MeltPondTop:
            return "meltPondTop";
        case TypeOfLevel::MeltPondBottom:
            return "meltPondBottom";
        case TypeOfLevel::EntireMeltPond:
            return "entireMeltPond";
        case TypeOfLevel::IceLayerAboveWaterSurface:
            return "iceLayerAboveWaterSurface";
        case TypeOfLevel::WaterSurfaceToIsothermalOceanLayer:
            return "waterSurfaceToIsothermalOceanLayer";
        case TypeOfLevel::TotalSoilLayer:
            return "totalSoilLayer";
        case TypeOfLevel::RootZone:
            return "rootZone";
        case TypeOfLevel::Roof:
            return "roof";
        case TypeOfLevel::RoofLayer:
            return "roofLayer";
        case TypeOfLevel::Wall:
            return "wall";
        case TypeOfLevel::WallLayer:
            return "wallLayer";
        case TypeOfLevel::Road:
            return "road";
        case TypeOfLevel::RoadLayer:
            return "roadLayer";
        case TypeOfLevel::UrbanCanyon:
            return "urbanCanyon";
        case TypeOfLevel::AbstractSingleLevel:
            return "abstractSingleLevel";
        case TypeOfLevel::AbstractMultipleLevels:
            return "abstractMultipleLevels";
        // HACKED - to be removed after migration
        case TypeOfLevel::HeightAboveGroundAt2m:
            return "heightAboveGroundAt2m";
        case TypeOfLevel::HeightAboveGroundAt10m:
            return "heightAboveGroundAt10m";
        case TypeOfLevel::HeightAboveSeaAt10m:
            return "heightAboveSeaAt10m";
        default:
            throw DataModellingException("WriteSpec<TypeOfLevel>::write: Unexpected value for TypeOfLevel", Here());
    }
}

TypeOfLevel ReadSpec<TypeOfLevel>::read(const std::string& val) {
    // May use a vector
    static const std::vector<std::pair<std::string, TypeOfLevel>> typeOfLevels{
        {"surface", TypeOfLevel::Surface},
        {"troposphere", TypeOfLevel::Troposphere},
        {"entireAtmosphere", TypeOfLevel::EntireAtmosphere},
        {"entireOcean", TypeOfLevel::EntireOcean},
        {"entireLake", TypeOfLevel::EntireLake},
        {"cloudBase", TypeOfLevel::CloudBase},
        {"cloudTop", TypeOfLevel::CloudTop},
        {"isothermZero", TypeOfLevel::IsothermZero},
        {"adiabaticCondensation", TypeOfLevel::AdiabaticCondensation},
        {"maxWind", TypeOfLevel::MaxWind},
        {"tropopause", TypeOfLevel::Tropopause},
        {"stratosphere", TypeOfLevel::Stratosphere},
        {"nominalTop", TypeOfLevel::NominalTop},
        {"seaBottom", TypeOfLevel::SeaBottom},
        {"atmosphere", TypeOfLevel::Atmosphere},
        {"cumulonimbusBase", TypeOfLevel::CumulonimbusBase},
        {"cumulonimbusTop", TypeOfLevel::CumulonimbusTop},
        {"freeConvection", TypeOfLevel::FreeConvection},
        {"convectiveCondensation", TypeOfLevel::ConvectiveCondensation},
        {"neutralBuoyancy", TypeOfLevel::NeutralBuoyancy},
        {"mostUnstableParcel", TypeOfLevel::MostUnstableParcel},
        {"mixedLayerParcel", TypeOfLevel::MixedLayerParcel},
        {"lowestLevelOfCloudCoverExceedance", TypeOfLevel::LowestLevelOfCloudCoverExceedance},
        {"isothermal", TypeOfLevel::Isothermal},
        {"isobaricInPa", TypeOfLevel::IsobaricInPa},
        {"isobaricInhPa", TypeOfLevel::IsobaricInhPa},
        {"isobaricLayer", TypeOfLevel::IsobaricLayer},
        {"lowCloudLayer", TypeOfLevel::LowCloudLayer},
        {"mediumCloudLayer", TypeOfLevel::MediumCloudLayer},
        {"highCloudLayer", TypeOfLevel::HighCloudLayer},
        {"meanSea", TypeOfLevel::MeanSea},
        {"heightAboveSea", TypeOfLevel::HeightAboveSea},
        {"heightAboveSeaLayer", TypeOfLevel::HeightAboveSeaLayer},
        {"heightAboveGround", TypeOfLevel::HeightAboveGround},
        {"heightAboveGroundLayer", TypeOfLevel::HeightAboveGroundLayer},
        {"sigma", TypeOfLevel::Sigma},
        {"sigmaLayer", TypeOfLevel::SigmaLayer},
        {"hybrid", TypeOfLevel::Hybrid},
        {"hybridLayer", TypeOfLevel::HybridLayer},
        {"depthBelowLand", TypeOfLevel::DepthBelowLand},
        {"depthBelowLandLayer", TypeOfLevel::DepthBelowLandLayer},
        {"theta", TypeOfLevel::Theta},
        {"thetaLayer", TypeOfLevel::ThetaLayer},
        {"pressureFromGround", TypeOfLevel::PressureFromGround},
        {"pressureFromGroundLayer", TypeOfLevel::PressureFromGroundLayer},
        {"potentialVorticity", TypeOfLevel::PotentialVorticity},
        {"eta", TypeOfLevel::Eta},
        {"snow", TypeOfLevel::Snow},
        {"snowLayer", TypeOfLevel::SnowLayer},
        {"mixedLayerDepthGeneric", TypeOfLevel::MixedLayerDepthGeneric},
        {"hybridHeight", TypeOfLevel::HybridHeight},
        {"hybridPressure", TypeOfLevel::HybridPressure},
        {"generalVertical", TypeOfLevel::GeneralVertical},
        {"generalVerticalLayer", TypeOfLevel::GeneralVerticalLayer},
        {"soil", TypeOfLevel::Soil},
        {"soilLayer", TypeOfLevel::SoilLayer},
        {"seaIce", TypeOfLevel::SeaIce},
        {"seaIceLayer", TypeOfLevel::SeaIceLayer},
        {"depthBelowSea", TypeOfLevel::DepthBelowSea},
        {"oceanSurface", TypeOfLevel::OceanSurface},
        {"depthBelowSeaLayer", TypeOfLevel::DepthBelowSeaLayer},
        {"oceanSurfaceToBottom", TypeOfLevel::OceanSurfaceToBottom},
        {"lakeBottom", TypeOfLevel::LakeBottom},
        {"mixingLayer", TypeOfLevel::MixingLayer},
        {"oceanModel", TypeOfLevel::OceanModel},
        {"oceanModelLayer", TypeOfLevel::OceanModelLayer},
        {"mixedLayerDepthByDensity", TypeOfLevel::MixedLayerDepthByDensity},
        {"mixedLayerDepthByTemperature", TypeOfLevel::MixedLayerDepthByTemperature},
        {"mixedLayerDepthByDiffusivity", TypeOfLevel::MixedLayerDepthByDiffusivity},
        {"snowTopOverIceOnWater", TypeOfLevel::SnowTopOverIceOnWater},
        {"snowLayerOverIceOnWater", TypeOfLevel::SnowLayerOverIceOnWater},
        {"iceTopOnWater", TypeOfLevel::IceTopOnWater},
        {"iceLayerOnWater", TypeOfLevel::IceLayerOnWater},
        {"iceTopUnderSnowOnWater", TypeOfLevel::IceTopUnderSnowOnWater},
        {"iceLayerUnderSnowOnWater", TypeOfLevel::IceLayerUnderSnowOnWater},
        {"iceBottomOnWater", TypeOfLevel::IceBottomOnWater},
        {"indefiniteSoilDepth", TypeOfLevel::IndefiniteSoilDepth},
        {"meltPondTop", TypeOfLevel::MeltPondTop},
        {"meltPondBottom", TypeOfLevel::MeltPondBottom},
        {"entireMeltPond", TypeOfLevel::EntireMeltPond},
        {"iceLayerAboveWaterSurface", TypeOfLevel::IceLayerAboveWaterSurface},
        {"waterSurfaceToIsothermalOceanLayer", TypeOfLevel::WaterSurfaceToIsothermalOceanLayer},
        {"totalSoilLayer", TypeOfLevel::TotalSoilLayer},
        {"rootZone", TypeOfLevel::RootZone},
        {"roof", TypeOfLevel::Roof},
        {"roofLayer", TypeOfLevel::RoofLayer},
        {"wall", TypeOfLevel::Wall},
        {"wallLayer", TypeOfLevel::WallLayer},
        {"road", TypeOfLevel::Road},
        {"roadLayer", TypeOfLevel::RoadLayer},
        {"urbanCanyon", TypeOfLevel::UrbanCanyon},
        {"abstractSingleLevel", TypeOfLevel::AbstractSingleLevel},
        {"abstractMultipleLevel", TypeOfLevel::AbstractMultipleLevels},
        // HACKED - to be removed after migration
        {"heightAboveGroundAt2m", TypeOfLevel::HeightAboveGroundAt2m},
        {"heightAboveGroundAt10m", TypeOfLevel::HeightAboveGroundAt10m},
        {"heightAboveSeaAt10m", TypeOfLevel::HeightAboveSeaAt10m},
    };

    if (auto tol
        = std::find_if(typeOfLevels.begin(), typeOfLevels.end(), [&](const auto& pair) { return val == pair.first; });
        tol != typeOfLevels.end()) {
        return tol->second;
    }
    throw DataModellingException(std::string("ReadSpec<TypeOfLevel>::read Unknown value for TypeOfLevel: ") + val,
                                 Here());
}

std::string WriteSpec<TypeOfStatisticalProcessing>::write(TypeOfStatisticalProcessing v) {
    switch (v) {
        case TypeOfStatisticalProcessing::Average:
            return "average";
        case TypeOfStatisticalProcessing::Accumulation:
            return "accumul";
        case TypeOfStatisticalProcessing::Maximum:
            return "max";
        case TypeOfStatisticalProcessing::Minimum:
            return "min";
        case TypeOfStatisticalProcessing::Difference:
            return "difference";
        case TypeOfStatisticalProcessing::RootMeanSquare:
            return "root-mean-square";
        case TypeOfStatisticalProcessing::StandardDeviation:
            return "stddev";  // Currently used in MULTIOM...
        case TypeOfStatisticalProcessing::Covariance:
            return "covariance";
        case TypeOfStatisticalProcessing::InverseDifference:
            return "inverse-difference";  // TODO rename? stepType is sdiff
        case TypeOfStatisticalProcessing::Ratio:
            return "ratio";
        case TypeOfStatisticalProcessing::StandardizedAnomaly:
            return "standardized-anomaly";
        case TypeOfStatisticalProcessing::Summation:
            return "summation";
        case TypeOfStatisticalProcessing::ReturnPeriod:
            return "return-period";
        case TypeOfStatisticalProcessing::Median:
            return "median";
        case TypeOfStatisticalProcessing::Severity:
            return "severity";
        case TypeOfStatisticalProcessing::Mode:
            return "mode";
        case TypeOfStatisticalProcessing::IndexProcessing:
            return "index-processing";
        default:
            throw DataModellingException(
                "WriteSpec<TypeOfStatisticalProcessing>::write: Unexpected value for TypeOfStatisticalProcessing",
                Here());
    }
}


TypeOfStatisticalProcessing ReadSpec<TypeOfStatisticalProcessing>::read(const std::string& val) {
    // May use a vector
    static const std::vector<std::pair<std::string, TypeOfStatisticalProcessing>> typesOfStat{
        {"average", TypeOfStatisticalProcessing::Average},
        {"accumul", TypeOfStatisticalProcessing::Accumulation},
        {"max", TypeOfStatisticalProcessing::Maximum},
        {"min", TypeOfStatisticalProcessing::Minimum},
        {"difference", TypeOfStatisticalProcessing::Difference},
        {"root-mean-square", TypeOfStatisticalProcessing::RootMeanSquare},
        {"stddev", TypeOfStatisticalProcessing::StandardDeviation},
        {"covariance", TypeOfStatisticalProcessing::Covariance},
        {"inverse-difference", TypeOfStatisticalProcessing::InverseDifference},  // TODO rename? stepType is sdiff
        {"ratio", TypeOfStatisticalProcessing::Ratio},
        {"standardized-anomaly", TypeOfStatisticalProcessing::StandardizedAnomaly},
        {"summation", TypeOfStatisticalProcessing::Summation},
        {"return-period", TypeOfStatisticalProcessing::ReturnPeriod},
        {"median", TypeOfStatisticalProcessing::Median},
        {"severity", TypeOfStatisticalProcessing::Severity},
        {"mode", TypeOfStatisticalProcessing::Mode},
        {"index-processing", TypeOfStatisticalProcessing::IndexProcessing},
    };

    if (auto tos
        = std::find_if(typesOfStat.begin(), typesOfStat.end(), [&](const auto& pair) { return val == pair.first; });
        tos != typesOfStat.end()) {
        return tos->second;
    }
    throw DataModellingException(
        std::string("ReadSpec<TypeOfStatisticalProcessing>::read Unknown value for TypeOfStatisticalProcessing: ")
            + val,
        Here());
}


TypeOfStatisticalProcessing ReadSpec<TypeOfStatisticalProcessing>::read(std::int64_t i) {
    switch (i) {
        case static_cast<std::int64_t>(TypeOfStatisticalProcessing::Average):
            return TypeOfStatisticalProcessing::Average;
        case static_cast<std::int64_t>(TypeOfStatisticalProcessing::Accumulation):
            return TypeOfStatisticalProcessing::Accumulation;
        case static_cast<std::int64_t>(TypeOfStatisticalProcessing::Maximum):
            return TypeOfStatisticalProcessing::Maximum;
        case static_cast<std::int64_t>(TypeOfStatisticalProcessing::Minimum):
            return TypeOfStatisticalProcessing::Minimum;
        case static_cast<std::int64_t>(TypeOfStatisticalProcessing::Difference):
            return TypeOfStatisticalProcessing::Difference;
        case static_cast<std::int64_t>(TypeOfStatisticalProcessing::RootMeanSquare):
            return TypeOfStatisticalProcessing::RootMeanSquare;
        case static_cast<std::int64_t>(TypeOfStatisticalProcessing::StandardDeviation):
            return TypeOfStatisticalProcessing::StandardDeviation;
        case static_cast<std::int64_t>(TypeOfStatisticalProcessing::Covariance):
            return TypeOfStatisticalProcessing::Covariance;
        case static_cast<std::int64_t>(TypeOfStatisticalProcessing::InverseDifference):
            return TypeOfStatisticalProcessing::InverseDifference;
        case static_cast<std::int64_t>(TypeOfStatisticalProcessing::Ratio):
            return TypeOfStatisticalProcessing::Ratio;
        case static_cast<std::int64_t>(TypeOfStatisticalProcessing::StandardizedAnomaly):
            return TypeOfStatisticalProcessing::StandardizedAnomaly;
        case static_cast<std::int64_t>(TypeOfStatisticalProcessing::Summation):
            return TypeOfStatisticalProcessing::Summation;
        case static_cast<std::int64_t>(TypeOfStatisticalProcessing::ReturnPeriod):
            return TypeOfStatisticalProcessing::ReturnPeriod;
        case static_cast<std::int64_t>(TypeOfStatisticalProcessing::Median):
            return TypeOfStatisticalProcessing::Median;
        case static_cast<std::int64_t>(TypeOfStatisticalProcessing::Severity):
            return TypeOfStatisticalProcessing::Severity;
        case static_cast<std::int64_t>(TypeOfStatisticalProcessing::Mode):
            return TypeOfStatisticalProcessing::Mode;
        case static_cast<std::int64_t>(TypeOfStatisticalProcessing::IndexProcessing):
            return TypeOfStatisticalProcessing::IndexProcessing;
        default:
            throw DataModellingException(
                std::string(
                    "ReadSpec<TypeOfStatisticalProcessing>::read Unknown value for TypeOfStatisticalProcessing: ")
                    + std::to_string(i),
                Here());
    };
}

}  // namespace multio::datamod


namespace multio {

void util::Print<datamod::TypeOfLevel>::print(std::ostream& os, const datamod::TypeOfLevel& t) {
    util::print(os, datamod::Writer<datamod::TypeOfLevel>::write(t));
}

void util::Print<datamod::TypeOfStatisticalProcessing>::print(std::ostream& os, const datamod::TypeOfStatisticalProcessing& t) {
    util::print(os, datamod::Writer<datamod::TypeOfStatisticalProcessing>::write(t));
}

}  // namespace multio


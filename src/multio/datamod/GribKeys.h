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

#include "multio/datamod/Mapper.h"
#include "multio/datamod/core/EntryDef.h"
#include "multio/datamod/core/Print.h"
#include "multio/datamod/types/TypeOfLevel.h"
#include "multio/datamod/types/TypeOfStatisticalProcessing.h"


namespace multio::datamod {

// clang-format off

/// \defgroup datamod_models

/// \defgroup datamod_models_misc
/// \ingroup datamod_models

//-----------------------------------------------------------------------------
// Grib2 eccodes keys (lowlevel keys or concepts that perform useful naming)
//-----------------------------------------------------------------------------

constexpr auto Discipline =
    EntryDef<std::int64_t>{"discipline"}  
        .withDefault(0)
        .withAccessor([](auto&& v) { return &v.discipline; });

// Section1
constexpr auto TablesVersion =
    EntryDef<std::int64_t>{"tablesVersion"}  
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.tablesVersion; });
        
constexpr auto LocalTablesVersion =
    EntryDef<std::int64_t>{"localTablesVersion"}  
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.localTablesVersion; });
        
constexpr auto ProductionStatusOfProcessedData =
    EntryDef<std::int64_t>{"productionStatusOfProcessedData"}  
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.productionStatusOfProcessedData; });
        
constexpr auto TypeOfProcessedData =
    EntryDef<std::int64_t>{"typeOfProcessedData"}  
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.typeOfProcessedData; });


// Section3 (more to be moved here from MarsMiscGeo.h)

constexpr auto ShapeOfTheEarth =
    EntryDef<std::int64_t>{"shapeOfTheEarth"}  
        .withDefault(6)
        .withAccessor([](auto&& v) { return &v.shapeOfTheEarth; });

// Section 3 - GG

constexpr auto TruncateDegrees =
    EntryDef<std::int64_t>{"truncateDegrees"}  
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.truncateDegrees; });

constexpr auto NumberOfPointsAlongAMeridian =
    EntryDef<std::int64_t>{"numberOfPointsAlongAMeridian"}  
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.numberOfPointsAlongAMeridian; });

constexpr auto NumberOfParallelsBetweenAPoleAndTheEquator =
    EntryDef<std::int64_t>{"numberOfParallelsBetweenAPoleAndTheEquator"}  
        .withAccessor([](auto&& v) { return &v.numberOfParallelsBetweenAPoleAndTheEquator; });

constexpr auto LatitudeOfFirstGridPointInDegrees =
    EntryDef<double>{"latitudeOfFirstGridPointInDegrees"}  
        .withAccessor([](auto&& v) { return &v.latitudeOfFirstGridPointInDegrees; });

constexpr auto LongitudeOfFirstGridPointInDegrees =
    EntryDef<double>{"longitudeOfFirstGridPointInDegrees"}  
        .withAccessor([](auto&& v) { return &v.longitudeOfFirstGridPointInDegrees; });

constexpr auto LatitudeOfLastGridPointInDegrees =
    EntryDef<double>{"latitudeOfLastGridPointInDegrees"}  
        .withAccessor([](auto&& v) { return &v.latitudeOfLastGridPointInDegrees; });

constexpr auto LongitudeOfLastGridPointInDegrees =
    EntryDef<double>{"longitudeOfLastGridPointInDegrees"}  
        .withAccessor([](auto&& v) { return &v.longitudeOfLastGridPointInDegrees; });

constexpr auto Pl =
    EntryDef<std::vector<std::int64_t>>{"pl"}  
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.pl; });


// Section 3 - SH

constexpr auto PentagonalResolutionParameterJ = 
    EntryDef<std::int64_t>{"pentagonalResolutionParameterJ"}.withAccessor(
    [](auto&& v) { return &v.pentagonalResolutionParameterJ; });
constexpr auto PentagonalResolutionParameterK = EntryDef<std::int64_t>{"pentagonalResolutionParameterK"}.withAccessor(
    [](auto&& v) { return &v.pentagonalResolutionParameterK; });
constexpr auto PentagonalResolutionParameterM = EntryDef<std::int64_t>{"pentagonalResolutionParameterM"}.withAccessor(
    [](auto&& v) { return &v.pentagonalResolutionParameterM; });


// Section 3 - HEALpix

constexpr auto NSide =
    EntryDef<std::int64_t>{"nside"}  
        .withAccessor([](auto&& v) { return &v.nside; });

constexpr auto OrderingConvention =
    EntryDef<std::string>{"orderingConvention"}  
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.orderingConvention; });


// Section4
constexpr auto ProductDefinitionTemplateNumber =
    EntryDef<std::int64_t>{"productDefinitionTemplateNumber"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.productDefinitionTemplateNumber; });  
constexpr auto GeneratingProcessIdentifier =
    EntryDef<std::int64_t>{"generatingProcessIdentifier"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.generatingProcessIdentifier; });
constexpr auto TypeOfLevelEntry =
    EntryDef<TypeOfLevel>{"typeOfLevel"}  
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.typeOfLevel; });
constexpr auto TypeOfStatisticalProcessingEntry =
    EntryDef<TypeOfStatisticalProcessing>{"typeOfStatisticalProcessing"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.typeOfStatisticalProcessing; });  
constexpr auto ScaleFactorOfCentralWaveNumber =
    EntryDef<std::int64_t>{"scaleFactorOfCentralWaveNumber"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.scaleFactorOfCentralWaveNumber; });
constexpr auto ScaledValueOfCentralWaveNumber =
    EntryDef<std::int64_t>{"scaledValueOfCentralWaveNumber"}  
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.scaledValueOfCentralWaveNumber; });
// Ensemble

constexpr auto TypeOfEnsembleForecast =
    EntryDef<std::int64_t>{"typeOfEnsembleForecast"}  
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.typeOfEnsembleForecast; });

constexpr auto NumberOfForecastsInEnsemble =
    EntryDef<std::int64_t>{"numberOfForecastsInEnsemble"}  
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.numberOfForecastsInEnsemble; });

// Satellite 

constexpr auto SatelliteSeries =
    EntryDef<std::int64_t>{"satelliteSeries"}  
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.satelliteSeries; });
// Horizontal Keys
constexpr auto PressureUnits
    = EntryDef<std::string>{"pressureUnits"}  
          .tagOptional()
          .withAccessor([](auto&& v) { return &v.pressureUnits; });
constexpr auto TypeOfFirstFixedSurface
    = EntryDef<std::int64_t>{"typeOfFirstFixedSurface"}  
          .tagOptional()
          .withAccessor([](auto&& v) { return &v.typeOfFirstFixedSurface; });
constexpr auto TypeOfSecondFixedSurface
    = EntryDef<std::int64_t>{"typeOfSecondFixedSurface"}  
          .tagOptional()
          .withAccessor([](auto&& v) { return &v.typeOfSecondFixedSurface; });
constexpr auto ScaledValueOfFirstFixedSurface
    = EntryDef<std::int64_t>{"scaledValueOfFirstFixedSurface"}  
          .tagOptional()
          .withAccessor([](auto&& v) { return &v.scaledValueOfFirstFixedSurface; });
constexpr auto ScaledValueOfSecondFixedSurface
    = EntryDef<std::int64_t>{"scaledValueOfSecondFixedSurface"}  
          .tagOptional()
          .withAccessor([](auto&& v) { return &v.scaledValueOfSecondFixedSurface; });
constexpr auto ScaleFactorOfFirstFixedSurface
    = EntryDef<std::int64_t>{"scaleFactorOfFirstFixedSurface"}  
          .tagOptional()
          .withAccessor([](auto&& v) { return &v.scaleFactorOfFirstFixedSurface; });
constexpr auto ScaleFactorOfSecondFixedSurface
    = EntryDef<std::int64_t>{"scaleFactorOfSecondFixedSurface"}  
          .tagOptional()
          .withAccessor([](auto&& v) { return &v.scaleFactorOfSecondFixedSurface; });

// clang-format on

struct HorizontalGribKeys {
    EntryType_t<decltype(PressureUnits)> pressureUnits;
    EntryType_t<decltype(TypeOfFirstFixedSurface)> typeOfFirstFixedSurface;
    EntryType_t<decltype(TypeOfSecondFixedSurface)> typeOfSecondFixedSurface;
    EntryType_t<decltype(ScaledValueOfFirstFixedSurface)> scaledValueOfFirstFixedSurface;
    EntryType_t<decltype(ScaledValueOfSecondFixedSurface)> scaledValueOfSecondFixedSurface;
    EntryType_t<decltype(ScaleFactorOfFirstFixedSurface)> scaleFactorOfFirstFixedSurface;
    EntryType_t<decltype(ScaleFactorOfSecondFixedSurface)> scaleFactorOfSecondFixedSurface;


    static constexpr std::string_view record_name_ = "horizontal";
    static constexpr auto record_entries_ = std::make_tuple(
        PressureUnits, TypeOfFirstFixedSurface, TypeOfSecondFixedSurface, ScaledValueOfFirstFixedSurface,
        ScaledValueOfSecondFixedSurface, ScaleFactorOfFirstFixedSurface, ScaleFactorOfSecondFixedSurface);
};

//-----------------------------------------------------------------------------

// clang-format off

// Data Repres

constexpr auto BitmapPresent =
    EntryDef<bool, mapper::BoolMapper>{"bitmapPresent"}  
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.bitmapPresent; });

constexpr auto MissingValue =
    EntryDef<double>{"missingValue"}  
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.missingValue; });
constexpr auto BitsPerValue =
    EntryDef<std::int64_t>{"bitsPerValue"}  
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.bitsPerValue; });

constexpr auto LaplacianOperator =
    EntryDef<std::int64_t>{"laplacianOperator"}  
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.laplacianOperator; });


//-----------------------------------------------------------------------------


constexpr auto PVPresent =
    EntryDef<bool>{"PVPresent"}  
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.pvPresent; });

constexpr auto Pv =
    EntryDef<std::vector<double>>{"pv"}  
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.pv; });

// clang-format on

struct VerticalGribKeys {
    EntryType_t<decltype(PVPresent)> pvPresent;
    EntryType_t<decltype(Pv)> pv;

    static constexpr std::string_view record_name_ = "vertical";
    static constexpr auto record_entries_ = std::make_tuple(PVPresent, Pv);
    
    static void applyDefaults(VerticalGribKeys& v) {
        if (v.pv.isSet()) {
            v.pvPresent.set(true);
        }
        else {
            v.pvPresent.unset();
        }
    }
};


//-----------------------------------------------------------------------------

}  // namespace multio::datamod

namespace multio::util {
template <>
struct Print<multio::datamod::HorizontalGribKeys> : multio::datamod::PrintRecord {};
template <>
struct Print<multio::datamod::VerticalGribKeys> : multio::datamod::PrintRecord {};

}  // namespace multio::util


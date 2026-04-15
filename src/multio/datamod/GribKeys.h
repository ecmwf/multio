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
#include "multio/datamod/types/TypeOfProcessedData.h"


namespace multio::datamod {

// clang-format off

/// \defgroup datamod_models

/// \defgroup datamod_models_misc
/// \ingroup datamod_models

//-----------------------------------------------------------------------------
// Grib2 eccodes keys (lowlevel keys or concepts that perform useful naming)
//-----------------------------------------------------------------------------

constexpr auto Discipline =
    EntryDef<std::int64_t>{"misc-discipline"}
        .withDefault(0)
        .withAccessor([](auto&& v) { return &v.discipline; });

// Section1
constexpr auto TablesVersion =
    EntryDef<std::int64_t>{"misc-tablesVersion"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.tablesVersion; });

constexpr auto LocalTablesVersion =
    EntryDef<std::int64_t>{"misc-localTablesVersion"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.localTablesVersion; });

constexpr auto ProductionStatusOfProcessedData =
    EntryDef<std::int64_t>{"misc-productionStatusOfProcessedData"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.productionStatusOfProcessedData; });

constexpr auto SubCentre =
    EntryDef<std::int64_t>{"misc-subCentre"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.subCentre; });

constexpr auto TypeOfProcessedDataEntry =
    EntryDef<TypeOfProcessedData>{"misc-typeOfProcessedData"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.typeOfProcessedData; });


// Section3 (more to be moved here from MarsMiscGeo.h)

constexpr auto ShapeOfTheEarth =
    EntryDef<std::int64_t>{"misc-shapeOfTheEarth"}
        .withDefault(6)
        .withAccessor([](auto&& v) { return &v.shapeOfTheEarth; });

// Section 3 - GG

constexpr auto TruncateDegrees =
    EntryDef<std::int64_t>{"misc-truncateDegrees"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.truncateDegrees; });

constexpr auto NumberOfPointsAlongAMeridian =
    EntryDef<std::int64_t>{"misc-numberOfPointsAlongAMeridian"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.numberOfPointsAlongAMeridian; });

constexpr auto NumberOfParallelsBetweenAPoleAndTheEquator =
    EntryDef<std::int64_t>{"misc-numberOfParallelsBetweenAPoleAndTheEquator"}
        .withAccessor([](auto&& v) { return &v.numberOfParallelsBetweenAPoleAndTheEquator; });

constexpr auto LatitudeOfFirstGridPointInDegrees =
    EntryDef<double>{"misc-latitudeOfFirstGridPointInDegrees"}
        .withAccessor([](auto&& v) { return &v.latitudeOfFirstGridPointInDegrees; });

constexpr auto LongitudeOfFirstGridPointInDegrees =
    EntryDef<double>{"misc-longitudeOfFirstGridPointInDegrees"}
        .withAccessor([](auto&& v) { return &v.longitudeOfFirstGridPointInDegrees; });

constexpr auto LatitudeOfLastGridPointInDegrees =
    EntryDef<double>{"misc-latitudeOfLastGridPointInDegrees"}
        .withAccessor([](auto&& v) { return &v.latitudeOfLastGridPointInDegrees; });

constexpr auto LongitudeOfLastGridPointInDegrees =
    EntryDef<double>{"misc-longitudeOfLastGridPointInDegrees"}
        .withAccessor([](auto&& v) { return &v.longitudeOfLastGridPointInDegrees; });

constexpr auto IDirectionIncrementInDegrees =
    EntryDef<double>{"misc-iDirectionIncrementInDegrees"}
        .withAccessor([](auto&& v) { return &v.iDirectionIncrementInDegrees; });

constexpr auto JDirectionIncrementInDegrees =
    EntryDef<double>{"misc-jDirectionIncrementInDegrees"}
        .withAccessor([](auto&& v) { return &v.jDirectionIncrementInDegrees; });


constexpr auto Pl =
    EntryDef<std::vector<std::int64_t>>{"misc-pl"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.pl; });


// Section 3 - LL

constexpr auto NumberOfPointsAlongAParallel =
    EntryDef<std::int64_t>{"misc-numberOfPointsAlongAParallel"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.numberOfPointsAlongAParallel; });


// Section 3 - SH

constexpr auto PentagonalResolutionParameterJ =
    EntryDef<std::int64_t>{"misc-pentagonalResolutionParameterJ"}.withAccessor(
    [](auto&& v) { return &v.pentagonalResolutionParameterJ; });
constexpr auto PentagonalResolutionParameterK = EntryDef<std::int64_t>{"misc-pentagonalResolutionParameterK"}.withAccessor(
    [](auto&& v) { return &v.pentagonalResolutionParameterK; });
constexpr auto PentagonalResolutionParameterM = EntryDef<std::int64_t>{"misc-pentagonalResolutionParameterM"}.withAccessor(
    [](auto&& v) { return &v.pentagonalResolutionParameterM; });


// Section 3 - HEALpix

constexpr auto NSide =
    EntryDef<std::int64_t>{"misc-nside"}
        .withAccessor([](auto&& v) { return &v.nside; });

constexpr auto OrderingConvention =
    EntryDef<std::string>{"misc-orderingConvention"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.orderingConvention; });


// Section4
constexpr auto ProductDefinitionTemplateNumber =
    EntryDef<std::int64_t>{"misc-productDefinitionTemplateNumber"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.productDefinitionTemplateNumber; });
constexpr auto GeneratingProcessIdentifier =
    EntryDef<std::int64_t>{"misc-generatingProcessIdentifier"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.generatingProcessIdentifier; });
constexpr auto TypeOfLevelEntry =
    EntryDef<TypeOfLevel>{"misc-typeOfLevel"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.typeOfLevel; });
constexpr auto TypeOfStatisticalProcessingEntry =
    EntryDef<TypeOfStatisticalProcessing>{"misc-typeOfStatisticalProcessing"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.typeOfStatisticalProcessing; });
constexpr auto ScaleFactorOfCentralWaveNumber =
    EntryDef<std::int64_t>{"misc-scaleFactorOfCentralWaveNumber"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.scaleFactorOfCentralWaveNumber; });
constexpr auto ScaledValueOfCentralWaveNumber =
    EntryDef<std::int64_t>{"misc-scaledValueOfCentralWaveNumber"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.scaledValueOfCentralWaveNumber; });
// Ensemble

constexpr auto TypeOfEnsembleForecast =
    EntryDef<std::int64_t>{"misc-typeOfEnsembleForecast"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.typeOfEnsembleForecast; });

constexpr auto NumberOfForecastsInEnsemble =
    EntryDef<std::int64_t>{"misc-numberOfForecastsInEnsemble"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.numberOfForecastsInEnsemble; });

// Satellite

constexpr auto SatelliteSeries =
    EntryDef<std::int64_t>{"misc-satelliteSeries"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.satelliteSeries; });
// Horizontal Keys
constexpr auto PressureUnits
    = EntryDef<std::string>{"misc-pressureUnits"}
          .tagOptional()
          .withAccessor([](auto&& v) { return &v.pressureUnits; });
constexpr auto TypeOfFirstFixedSurface
    = EntryDef<std::int64_t>{"misc-typeOfFirstFixedSurface"}
          .tagOptional()
          .withAccessor([](auto&& v) { return &v.typeOfFirstFixedSurface; });
constexpr auto TypeOfSecondFixedSurface
    = EntryDef<std::int64_t>{"misc-typeOfSecondFixedSurface"}
          .tagOptional()
          .withAccessor([](auto&& v) { return &v.typeOfSecondFixedSurface; });
constexpr auto ScaledValueOfFirstFixedSurface
    = EntryDef<std::int64_t>{"misc-scaledValueOfFirstFixedSurface"}
          .tagOptional()
          .withAccessor([](auto&& v) { return &v.scaledValueOfFirstFixedSurface; });
constexpr auto ScaledValueOfSecondFixedSurface
    = EntryDef<std::int64_t>{"misc-scaledValueOfSecondFixedSurface"}
          .tagOptional()
          .withAccessor([](auto&& v) { return &v.scaledValueOfSecondFixedSurface; });
constexpr auto ScaleFactorOfFirstFixedSurface
    = EntryDef<std::int64_t>{"misc-scaleFactorOfFirstFixedSurface"}
          .tagOptional()
          .withAccessor([](auto&& v) { return &v.scaleFactorOfFirstFixedSurface; });
constexpr auto ScaleFactorOfSecondFixedSurface
    = EntryDef<std::int64_t>{"misc-scaleFactorOfSecondFixedSurface"}
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
    EntryDef<bool>{"misc-bitmapPresent"}
        .withDefault(false)
        .withAccessor([](auto&& v) { return &v.bitmapPresent; });

constexpr auto MissingValue =
    EntryDef<double>{"misc-missingValue"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.missingValue; });

constexpr auto BitsPerValue =
    EntryDef<std::int64_t>{"misc-bitsPerValue"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.bitsPerValue; });

constexpr auto LaplacianOperator =
    EntryDef<double>{"misc-laplacianOperator"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.laplacianOperator; });


//-----------------------------------------------------------------------------


constexpr auto PVPresent =
    EntryDef<bool>{"misc-PVPresent"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.pvPresent; });

constexpr auto Pv =
    EntryDef<std::vector<double>>{"misc-pv"}
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


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
#include "multio/datamod/Mapper.h"
#include "multio/datamod/core/EntryDef.h"
#include "multio/datamod/core/Print.h"
#include "multio/datamod/types/GridType.h"
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
    EntryDef<std::int64_t>{"discipline"}  
        .withDefault(0)
        .withAccessor([](auto&& v) { return &v.discipline; });

// Section1
constexpr auto TablesVersion =
    EntryDef<std::int64_t>{"tablesVersion"}  
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.tablesVersion; });

// Readonly ....
       
constexpr auto LocalTablesVersion =
    EntryDef<std::int64_t>{"localTablesVersion"}  
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.localTablesVersion; });
        
constexpr auto ProductionStatusOfProcessedData =
    EntryDef<std::int64_t>{"productionStatusOfProcessedData"}  
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.productionStatusOfProcessedData; });
        
constexpr auto TypeOfProcessedDataEntry =
    EntryDef<TypeOfProcessedData>{"typeOfProcessedData"}  
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.typeOfProcessedData; });



// Section4
constexpr auto GeneratingProcessIdentifier =
    EntryDef<std::int64_t>{"generatingProcessIdentifier"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.generatingProcessIdentifier; });
constexpr auto TypeOfLevelEntry =
    EntryDef<TypeOfLevel>{"typeOfLevel"}  
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.typeOfLevel; });
constexpr auto TypeOfStatisticalProcessingKey =
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


// clang-format on

struct HorizontalGribKeys {
    Entry<std::string> pressureUnits;
    Entry<std::int64_t> typeOfFirstFixedSurface;
    Entry<std::int64_t> typeOfSecondFixedSurface;
    Entry<std::int64_t> scaledValueOfFirstFixedSurface;
    Entry<std::int64_t> scaledValueOfSecondFixedSurface;
    Entry<std::int64_t> scaleFactorOfFirstFixedSurface;
    Entry<std::int64_t> scaleFactorOfSecondFixedSurface;


    static constexpr std::string_view record_name_ = "horizontal";
    static constexpr auto record_entries_ = std::make_tuple(
        entryDef("pressureUnits", &HorizontalGribKeys::pressureUnits).tagOptional(),
        entryDef("typeOfFirstFixedSurface", &HorizontalGribKeys::typeOfFirstFixedSurface).tagOptional(),
        entryDef("typeOfSecondFixedSurface", &HorizontalGribKeys::typeOfSecondFixedSurface).tagOptional(),
        entryDef("scaledValueOfFirstFixedSurface", &HorizontalGribKeys::scaledValueOfFirstFixedSurface).tagOptional(),
        entryDef("scaledValueOfSecondFixedSurface", &HorizontalGribKeys::scaledValueOfSecondFixedSurface).tagOptional(),
        entryDef("scaleFactorOfFirstFixedSurface", &HorizontalGribKeys::scaleFactorOfFirstFixedSurface).tagOptional(),
        entryDef("scaleFactorOfSecondFixedSurface", &HorizontalGribKeys::scaleFactorOfSecondFixedSurface)
            .tagOptional());
};

//-----------------------------------------------------------------------------

// clang-format off

// Data Repres

constexpr auto BitmapPresent =
    EntryDef<bool>{"bitmapPresent"}  
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
    EntryDef<double>{"laplacianOperator"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.laplacianOperator; });


//-----------------------------------------------------------------------------

constexpr auto Pv =
    EntryDef<std::vector<double>>{"pv"}  
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.pv; });

// clang-format on

struct VerticalGribKeys {
    Entry<bool> pvPresent;
    EntryType_t<decltype(Pv)> pv;

    static constexpr std::string_view record_name_ = "vertical";
    static constexpr auto record_entries_
        = std::make_tuple(entryDef("PVPresent", &VerticalGribKeys::pvPresent).tagOptional(), Pv);

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


//-----------------------------------------------------------------------------

}  // namespace multio::datamod

namespace multio::util {
template <>
struct Print<multio::datamod::HorizontalGribKeys> : multio::datamod::PrintRecord {};
template <>
struct Print<multio::datamod::VerticalGribKeys> : multio::datamod::PrintRecord {};

}  // namespace multio::util


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
#include "multio/datamod/GribTypes.h"
#include "multio/datamod/MarsMiscGeo.h"

#include <sstream>


namespace multio::datamod {

//-----------------------------------------------------------------------------
// Grib2 eccodes keys (lowlevel keys or concepts that perform useful naming)
//-----------------------------------------------------------------------------

enum class Grib2Keys : std::uint64_t
{
    // Section0
    Discipline,

    // Section1
    TablesVersion,
    LocalTablesVersion,
    ProductionStatusOfProcessedData,
    TypeOfProcessedData,


    // Section4
    ProductDefinitionTemplateNumber,

    // Horizatontal
    TypeOfLevel,

    //
    GeneratingProcessIdentifier,

    // Timerange
    TypeOfStatisticalProcessing,

    //
    ScaleFactorOfCentralWaveNumber,
    ScaledValueOfCentralWaveNumber,
};


// KeyDef<MiscKeys::EncodeStepZero, bool, mapper::IntToBoolMapper>{"encodeStepZero"}.tagOptional(),           //
// KeyDef<MiscKeys::InitialStep, std::int64_t>{"initialStep"}.withDefault(0),                                 //
// KeyDef<MiscKeys::LengthOfTimeRange, std::int64_t>{"lengthOfTimeRange"}.tagOptional(),                      //
// KeyDef<MiscKeys::LengthOfTimeStep, std::int64_t>{"lengthOfTimeStep"}.tagOptional(),                        //
// KeyDef<MiscKeys::LengthOfTimeRangeInSeconds, std::int64_t>{"lengthOfTimeRangeInSeconds"}.tagOptional(),    //
// KeyDef<MiscKeys::LengthOfTimeStepInSeconds, std::int64_t>{"lengthOfTimeStepInSeconds"}.withDefault(3600),  //
// KeyDef<MiscKeys::ValuesScaleFactor, double>{"valuesScaleFactor"}.tagOptional(),                            //
// KeyDef<MiscKeys::Pv, std::vector<double>>{"pv"}.tagOptional(),                                             //
// KeyDef<MiscKeys::NumberOfMissingValues, std::int64_t>{"numberOfMissingValues"}.tagOptional(),              //
// KeyDef<MiscKeys::ValueOfMissingValues, double>{"valueOfMissingValues"}.tagOptional(),                      //
// KeyDef<MiscKeys::TypeOfEnsembleForecast, std::int64_t>{"typeOfEnsembleForecast"}.tagOptional(),            //
// KeyDef<MiscKeys::NumberOfForecastsInEnsemble, std::int64_t>{"numberOfForecastsInEnsemble"}.tagOptional(),  //
// KeyDef<MiscKeys::LengthOfTimeWindow, std::int64_t>{"lengthOfTimeWindow"}.tagOptional(),                    //
// KeyDef<MiscKeys::LengthOfTimeWindowInSeconds, std::int64_t>{"lengthOfTimeWindowInSeconds"}.tagOptional(),  //
// KeyDef<MiscKeys::BitsPerValue, std::int64_t>{"bitsPerValue"}.tagOptional(),                                //
// KeyDef<MiscKeys::PeriodMin, std::int64_t>{"periodMin"}.tagOptional().withDescription(
//     "`periodMin` usually is depending on `paramId` and derived by ECCODES. in some cases it is "
//     "passed through."),  //
// KeyDef<MiscKeys::PeriodMax, std::int64_t>{"periodMax"}.tagOptional().withDescription(
//     "`periodMax` usually is depending on `paramId` and derived by ECCODES. In some cases it is "
//     "passed through."),                                                                                          //
// KeyDef<MiscKeys::WaveDirections, std::vector<double>>{"waveDirections"}.tagOptional(),                           //
// KeyDef<MiscKeys::WaveFrequencies, std::vector<double>>{"waveFrequencies"}.tagOptional(),                         //
// KeyDef<MiscKeys::SatelliteSeries, std::int64_t>{"satelliteSeries"}.tagOptional(),                                //


MULTIO_KEY_SET_DESCRIPTION(
    Grib2Keys,  //
    "grib2",    //
                //
    // Section0
    KeyDef<Grib2Keys::Discipline, std::int64_t>{"discipline"}.withDefault(0),  //

    // Section1
    KeyDef<Grib2Keys::TablesVersion, std::int64_t>{"tablesVersion"}.tagOptional(),            //
    KeyDef<Grib2Keys::LocalTablesVersion, std::int64_t>{"LocalTablesVersion"}.tagOptional(),  //
    KeyDef<Grib2Keys::ProductionStatusOfProcessedData, std::int64_t>{"productionStatusOfProcessedData"}
        .tagOptional(),                                                                         //
    KeyDef<Grib2Keys::TypeOfProcessedData, std::int64_t>{"typeOfProcessedData"}.tagOptional(),  //

    // Section4
    KeyDef<Grib2Keys::ProductDefinitionTemplateNumber, std::int64_t>{"productDefinitionTemplateNumber"}
        .tagOptional(),                                                                                         //
    KeyDef<Grib2Keys::GeneratingProcessIdentifier, std::int64_t>{"generatingProcessIdentifier"}.tagOptional(),  //
    KeyDef<Grib2Keys::TypeOfLevel, TypeOfLevel>{"typeOfLevel"}.tagOptional(),                                   //
    KeyDef<Grib2Keys::TypeOfStatisticalProcessing, TypeOfStatisticalProcessing>{"typeOfStatisticalProcessing"}
        .tagOptional(),                                                                                               //
    KeyDef<Grib2Keys::ScaleFactorOfCentralWaveNumber, std::int64_t>{"scaleFactorOfCentralWaveNumber"}.tagOptional(),  //
    KeyDef<Grib2Keys::ScaledValueOfCentralWaveNumber, std::int64_t>{"scaledValueOfCentralWaveNumber"}.tagOptional()   //
);

using Grib2KeySet = KeySet<Grib2Keys>;
using Grib2KeyValueSet = KeyValueSet<Grib2KeySet>;


template <>
struct KeySetAlter<Grib2KeySet> {
    static void alter(Grib2KeyValueSet& grib2) {}
};

//-----------------------------------------------------------------------------


// Alternative to typeOfLevel
enum class HorizontalKeys : std::uint64_t
{
    PressureUnits,
    TypeOfFirstFixedSurface,
    TypeOfSecondFixedSurface,
    ScaledValueOfFirstFixedSurface,
    ScaledValueOfSecondFixedSurface,
    ScaleFactorOfFirstFixedSurface,
    ScaleFactorOfSecondFixedSurface,
};


MULTIO_KEY_SET_DESCRIPTION(
    HorizontalKeys,                                                                                            //
    "horizontal",                                                                                              //
                                                                                                               //
    KeyDef<HorizontalKeys::PressureUnits, std::string>{"pressureUnits"}.tagOptional(),                         //
    KeyDef<HorizontalKeys::TypeOfFirstFixedSurface, std::int64_t>{"typeOfFirstFixedSurface"}.tagOptional(),    //
    KeyDef<HorizontalKeys::TypeOfSecondFixedSurface, std::int64_t>{"typeOfSecondFixedSurface"}.tagOptional(),  //
    KeyDef<HorizontalKeys::ScaledValueOfFirstFixedSurface, std::int64_t>{"scaledValueOfFirstFixedSurface"}
        .tagOptional(),  //
    KeyDef<HorizontalKeys::ScaledValueOfSecondFixedSurface, std::int64_t>{"scaledValueOfSecondFixedSurface"}
        .tagOptional(),  //
    KeyDef<HorizontalKeys::ScaleFactorOfFirstFixedSurface, std::int64_t>{"scaleFactorOfFirstFixedSurface"}
        .tagOptional(),  //
    KeyDef<HorizontalKeys::ScaleFactorOfSecondFixedSurface, std::int64_t>{"scaleFactorOfSecondFixedSurface"}
        .tagOptional());  //

using HorizontalKeySet = KeySet<HorizontalKeys>;
using HorizontalKeyValueSet = KeyValueSet<HorizontalKeySet>;

//-----------------------------------------------------------------------------


enum class VerticalKeys : std::uint64_t
{
    PVPresent,
    PV,
};


MULTIO_KEY_SET_DESCRIPTION(VerticalKeys,                                                                              //
                           "vertical",                                                                                //
                                                                                                                      //
                           KeyDef<VerticalKeys::PVPresent, bool>{"PVPresent"}.tagOptional(),                          //
                           KeyDef<VerticalKeys::PV, std::vector<double>>{"typeOfSecondFixedSurface"}.tagOptional());  //


using VerticalKeySet = KeySet<VerticalKeys>;
using VerticalKeyValueSet = KeyValueSet<VerticalKeySet>;


template <>
struct KeySetAlter<VerticalKeySet> {
    static void alter(VerticalKeyValueSet& v) {
        auto& pvPresent = key<VerticalKeys::PVPresent>(v);
        auto& pv = key<VerticalKeys::PV>(v);

        pvPresent.set(pv.has());
    }
};


//-----------------------------------------------------------------------------

}  // namespace multio::datamod


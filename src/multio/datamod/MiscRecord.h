/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @file MiscRecord.h
/// @brief Shared encoding/parametrization record used by the encoder and mars2mars.
///
/// This record contains miscellaneous keys that control GRIB encoding behavior.
/// Keys are typically set once from the global Parametrization at the start of a run.

#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <vector>

#include "multio/util/record/Entry.h"


namespace multio::datamod {

using multio::util::record::optionalEntry;

struct MiscRecord {

    // --- GRIB encoding ---
    std::optional<std::int64_t> tablesVersion;
    std::optional<std::int64_t> localTablesVersion;
    std::optional<std::int64_t> generatingProcessIdentifier;
    std::optional<std::int64_t> subCentre;
    std::optional<std::int64_t> typeOfProcessedData;

    // --- Data representation ---
    std::optional<bool> bitmapPresent;
    std::optional<double> missingValue;
    std::optional<std::int64_t> bitsPerValue;
    std::optional<double> laplacianOperator;

    // --- Time model ---
    std::optional<std::int64_t> initialStep;
    std::optional<std::int64_t> timeIncrementInSeconds;
    std::optional<std::int64_t> lengthOfTimeWindow;
    std::optional<std::int64_t> lengthOfTimeWindowInSeconds;

    // --- Ensemble ---
    std::optional<std::int64_t> perturbationNumber;
    std::optional<std::int64_t> numberOfForecastsInEnsemble;
    std::optional<std::int64_t> typeOfEnsembleForecast;

    // --- Satellite ---
    std::optional<std::int64_t> satelliteSeries;
    std::optional<std::int64_t> scaleFactorOfCentralWaveNumber;
    std::optional<std::int64_t> scaledValueOfCentralWaveNumber;

    // --- Wave ---
    std::optional<std::int64_t> scaleFactorOfWaveDirections;
    std::optional<std::int64_t> scaleFactorOfWaveFrequencies;
    std::optional<std::vector<double>> waveDirections;
    std::optional<std::vector<double>> waveFrequencies;

    // --- PV array ---
    std::optional<std::vector<double>> pv;


    static constexpr auto fields_ = std::make_tuple(
        // GRIB encoding
        optionalEntry("misc-tablesVersion", &MiscRecord::tablesVersion),
        optionalEntry("misc-localTablesVersion", &MiscRecord::localTablesVersion),
        optionalEntry("misc-generatingProcessIdentifier", &MiscRecord::generatingProcessIdentifier),
        optionalEntry("misc-subCentre", &MiscRecord::subCentre),
        optionalEntry("misc-typeOfProcessedData", &MiscRecord::typeOfProcessedData),
        // Data representation
        optionalEntry("misc-bitmapPresent", &MiscRecord::bitmapPresent),
        optionalEntry("misc-missingValue", &MiscRecord::missingValue),
        optionalEntry("misc-bitsPerValue", &MiscRecord::bitsPerValue),
        optionalEntry("misc-laplacianOperator", &MiscRecord::laplacianOperator),
        // Time model
        optionalEntry("misc-initialStep", &MiscRecord::initialStep),
        optionalEntry("misc-timeIncrementInSeconds", &MiscRecord::timeIncrementInSeconds),
        optionalEntry("misc-lengthOfTimeWindow", &MiscRecord::lengthOfTimeWindow),
        optionalEntry("misc-lengthOfTimeWindowInSeconds", &MiscRecord::lengthOfTimeWindowInSeconds),
        // Ensemble
        optionalEntry("misc-perturbationNumber", &MiscRecord::perturbationNumber),
        optionalEntry("misc-numberOfForecastsInEnsemble", &MiscRecord::numberOfForecastsInEnsemble),
        optionalEntry("misc-typeOfEnsembleForecast", &MiscRecord::typeOfEnsembleForecast),
        // Satellite
        optionalEntry("misc-satelliteSeries", &MiscRecord::satelliteSeries),
        optionalEntry("misc-scaleFactorOfCentralWaveNumber", &MiscRecord::scaleFactorOfCentralWaveNumber),
        optionalEntry("misc-scaledValueOfCentralWaveNumber", &MiscRecord::scaledValueOfCentralWaveNumber),
        // Wave
        optionalEntry("misc-scaleFactorOfWaveDirections", &MiscRecord::scaleFactorOfWaveDirections),
        optionalEntry("misc-scaleFactorOfWaveFrequencies", &MiscRecord::scaleFactorOfWaveFrequencies),
        optionalEntry("misc-waveDirections", &MiscRecord::waveDirections),
        optionalEntry("misc-waveFrequencies", &MiscRecord::waveFrequencies),
        // PV array
        optionalEntry("misc-pv", &MiscRecord::pv));
};


}  // namespace multio::datamod

/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @file MarsRecord.h
/// @brief Shared flat MARS record used across actions.
///
/// This single struct contains all MARS archival keys as optional fields.
/// Actions that need MARS metadata can either use this struct directly or
/// define their own action-specific metadata struct with a subset of these keys.

#pragma once

#include <cstdint>
#include <optional>
#include <string>

#include "multio/datamod/types/LevType.h"
#include "multio/datamod/types/Param.h"
#include "multio/datamod/types/StatType.h"
#include "multio/util/record/Entry.h"


namespace multio::datamod {

using multio::util::record::optionalEntry;

struct MarsRecord {

    // --- Identification ---
    std::optional<std::string> origin;
    std::optional<std::string> cls;  // "class" in MARS language
    std::optional<std::string> stream;
    std::optional<std::string> type;
    std::optional<std::string> expver;
    std::optional<std::string> model;

    // --- Field ---
    std::optional<Param> param;
    std::optional<LevType> levtype;
    std::optional<std::int64_t> levelist;

    // --- Time ---
    std::optional<std::int64_t> date;
    std::optional<std::int64_t> time;
    std::optional<std::int64_t> step;
    std::optional<std::string> timespan;
    std::optional<StatType> stattype;
    std::optional<std::int64_t> anoffset;
    std::optional<std::int64_t> hdate;
    std::optional<std::int64_t> refdate;
    std::optional<std::int64_t> fcmonth;
    std::optional<std::int64_t> fcperiod;

    // --- Ensemble ---
    std::optional<std::int64_t> number;

    // --- Satellite ---
    std::optional<std::int64_t> ident;
    std::optional<std::int64_t> instrument;
    std::optional<std::int64_t> channel;

    // --- Chemical / Aerosol ---
    std::optional<std::string> chem;
    std::optional<std::string> wavelength;

    // --- Wave Spectra ---
    std::optional<std::int64_t> direction;
    std::optional<std::int64_t> frequency;

    // --- Seasonal ---
    std::optional<std::int64_t> method;
    std::optional<std::int64_t> system;

    // --- Sensitivity ---
    std::optional<std::int64_t> iteration;
    std::optional<std::int64_t> diagnostic;

    // --- DestinE ---
    std::optional<std::string> dataset;
    std::optional<std::string> resolution;
    std::optional<std::string> activity;
    std::optional<std::string> experiment;
    std::optional<std::int64_t> generation;
    std::optional<std::int64_t> realization;

    // --- Encoding hints ---
    std::optional<std::string> grid;
    std::optional<std::int64_t> truncation;
    std::optional<std::string> packing;


    static constexpr auto fields_ = std::make_tuple(
        // Identification
        optionalEntry("origin", &MarsRecord::origin), optionalEntry("class", &MarsRecord::cls),
        optionalEntry("stream", &MarsRecord::stream), optionalEntry("type", &MarsRecord::type),
        optionalEntry("expver", &MarsRecord::expver), optionalEntry("model", &MarsRecord::model),
        // Field
        optionalEntry("param", &MarsRecord::param), optionalEntry("levtype", &MarsRecord::levtype),
        optionalEntry("levelist", &MarsRecord::levelist),
        // Time
        optionalEntry("date", &MarsRecord::date), optionalEntry("time", &MarsRecord::time),
        optionalEntry("step", &MarsRecord::step), optionalEntry("timespan", &MarsRecord::timespan),
        optionalEntry("stattype", &MarsRecord::stattype), optionalEntry("anoffset", &MarsRecord::anoffset),
        optionalEntry("hdate", &MarsRecord::hdate), optionalEntry("refdate", &MarsRecord::refdate),
        optionalEntry("fcmonth", &MarsRecord::fcmonth), optionalEntry("fcperiod", &MarsRecord::fcperiod),
        // Ensemble
        optionalEntry("number", &MarsRecord::number),
        // Satellite
        optionalEntry("ident", &MarsRecord::ident), optionalEntry("instrument", &MarsRecord::instrument),
        optionalEntry("channel", &MarsRecord::channel),
        // Chemical / Aerosol
        optionalEntry("chem", &MarsRecord::chem), optionalEntry("wavelength", &MarsRecord::wavelength),
        // Wave Spectra
        optionalEntry("direction", &MarsRecord::direction), optionalEntry("frequency", &MarsRecord::frequency),
        // Seasonal
        optionalEntry("method", &MarsRecord::method), optionalEntry("system", &MarsRecord::system),
        // Sensitivity
        optionalEntry("iteration", &MarsRecord::iteration), optionalEntry("diagnostic", &MarsRecord::diagnostic),
        // DestinE
        optionalEntry("dataset", &MarsRecord::dataset), optionalEntry("resolution", &MarsRecord::resolution),
        optionalEntry("activity", &MarsRecord::activity), optionalEntry("experiment", &MarsRecord::experiment),
        optionalEntry("generation", &MarsRecord::generation), optionalEntry("realization", &MarsRecord::realization),
        // Encoding hints
        optionalEntry("grid", &MarsRecord::grid), optionalEntry("truncation", &MarsRecord::truncation),
        optionalEntry("packing", &MarsRecord::packing));
};


}  // namespace multio::datamod

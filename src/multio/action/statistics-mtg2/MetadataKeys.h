/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/// @file MetadataKeys.h
/// @brief Per-action metadata records for the statistics-mtg2 action.
///
/// Defines FlushMetadataKeys (for flush messages) and FieldMetadataKeys (for field messages),
/// using the new datamod framework (fields_ tuple + readMetadata/writeMetadata).

#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <string_view>

#include "multio/datamod/Parser.h"
#include "multio/datamod/types/LevType.h"
#include "multio/datamod/types/Param.h"


namespace multio::action::statistics_mtg2 {

namespace dm = multio::datamod;

//----------------------------- FlushKind Enum ------------------------------//

enum class FlushKind : std::size_t
{
    Default,
    FirstStep,
    StepAndRestart,
    LastStep,
    EndOfSimulation,
    CloseConnection
};

//------------------------ Flush Metadata Keys Record -----------------------//

struct FlushMetadataKeys {
    std::optional<std::int64_t> date;
    std::optional<std::int64_t> time;
    std::optional<std::int64_t> step;  // hours
    FlushKind flushKind = FlushKind::Default;
    std::optional<std::string> restartDateTime;
    std::optional<std::int64_t> serverRank;

    static constexpr auto fields_ = std::make_tuple(
        dm::optionalEntry("date", &FlushMetadataKeys::date), dm::optionalEntry("time", &FlushMetadataKeys::time),
        dm::optionalEntry("step", &FlushMetadataKeys::step),
        dm::optionalEntry("flushKind", &FlushMetadataKeys::flushKind),
        dm::optionalEntry("restartDateTime", &FlushMetadataKeys::restartDateTime),
        dm::optionalEntry("serverRank", &FlushMetadataKeys::serverRank));
};

//----------------------- Field Metadata Keys Record ------------------------//

struct FieldMetadataKeys {
    std::int64_t date = 0;
    std::int64_t time = 0;
    std::int64_t step = 0;                 // hours
    std::optional<std::int64_t> timespan;  // hours
    dm::Param param;
    std::optional<std::string> stream;
    dm::LevType levtype = dm::LevType::SFC;
    std::optional<std::int64_t> levelist;
    std::optional<std::string> grid;
    std::optional<std::int64_t> truncation;
    std::optional<std::int64_t> timeIncrementInSeconds;
    std::optional<bool> bitmapPresent;
    std::optional<double> missingValue;

    static constexpr auto fields_ = std::make_tuple(
        dm::requiredEntry("date", &FieldMetadataKeys::date), dm::requiredEntry("time", &FieldMetadataKeys::time),
        dm::requiredEntry("step", &FieldMetadataKeys::step),
        dm::optionalEntry("timespan", &FieldMetadataKeys::timespan),
        dm::requiredEntry("param", &FieldMetadataKeys::param), dm::optionalEntry("stream", &FieldMetadataKeys::stream),
        dm::requiredEntry("levtype", &FieldMetadataKeys::levtype),
        dm::optionalEntry("levelist", &FieldMetadataKeys::levelist),
        dm::optionalEntry("grid", &FieldMetadataKeys::grid),
        dm::optionalEntry("truncation", &FieldMetadataKeys::truncation),
        dm::optionalEntry("misc-timeIncrementInSeconds", &FieldMetadataKeys::timeIncrementInSeconds),
        dm::optionalEntry("misc-bitmapPresent", &FieldMetadataKeys::bitmapPresent),
        dm::optionalEntry("misc-missingValue", &FieldMetadataKeys::missingValue));
};

//---------------------------------------------------------------------------//

}  // namespace multio::action::statistics_mtg2


//-------------- FlushKind parseEntry/dumpEntry declarations ----------------//
// These must be in FlushKind's own namespace so that ADL can find them
// during two-phase lookup from the datamod::detail template functions.

namespace multio::action::statistics_mtg2 {

bool parseEntry(FlushKind& value, std::string_view key, const multio::message::Metadata& md);
void dumpEntry(FlushKind value, std::string_view key, multio::message::Metadata& md);
void dumpConfigEntry(FlushKind value, const std::string& key, eckit::LocalConfiguration& conf);

}  // namespace multio::action::statistics_mtg2

/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "eckit/config/LocalConfiguration.h"
#include "eckit/message/Message.h"

namespace multio::grib2MarsMisc {

using ValueSet = std::unordered_set<std::string>;
using FieldValueMap = std::unordered_map<std::string, ValueSet>;

enum class TimeSpanEqualToZeroHandling : std::size_t
{
    LogAndIgnore,
    Ignore,
    Copy,
};

enum class Discipline192Handling : std::size_t
{
    LogAndIgnore,
    Ignore,
    TryToHandle,
    Copy,
};

enum class OnErrorHandling : std::size_t
{
    Abort,
    LogAndSkip,
    Skip,
    TryToHandle,
    Copy,
};

enum class InvalidInputMessageHandling : std::size_t
{
    TryToHandle,
    Skip,
};

struct Grib2MarsMiscOptions {
    std::optional<FieldValueMap> exclude;
    std::optional<FieldValueMap> filter;
    std::optional<FieldValueMap> except;

    bool copyGrib2Messages = true;
    bool useWmoUnits = false;
    bool controlForecast = false;
    bool convertWaveStreamToOper = false;

    long ncycle = 0;
    long defaultEnsembleSize = 0;

    std::string packingOverride;
    std::string modelOverride;
    std::string expverOverride;

    InvalidInputMessageHandling invalidInputMessage = InvalidInputMessageHandling::TryToHandle;
    OnErrorHandling onError = OnErrorHandling::LogAndSkip;
    Discipline192Handling discipline192 = Discipline192Handling::LogAndIgnore;
    TimeSpanEqualToZeroHandling timespanNonPositive = TimeSpanEqualToZeroHandling::LogAndIgnore;
};

enum class MessageDisposition
{
    Encode,
    CopyGrib2Verbatim,
    CopyExceptMatched,
    CopyInvalidMessage,
    CopyDiscipline192,
    CopyTimespanNonPositive,
    SkipExcluded,
    SkipFilteredOut,
    SkipInvalidMessage,
    SkipDiscipline192,
    SkipTimespanNonPositive,
    FailToExtract,
    FailToEncode,
    FailToArchive,
    ComplexExclusion,
};

enum class ExtractionOutcomeCode : std::uint8_t
{
    ReadyToEncode = 0,
    ProcessedAndArchived,
    CopyRequiredGrib2Verbatim,
    CopyRequiredExceptMatched,
    CopyRequiredInvalidMessage,
    CopyRequiredDiscipline192,
    CopyRequiredTimespanNonPositive,
    SkipRequiredExcluded,
    SkipRequiredFilteredOut,
    SkipRequiredInvalidMessage,
    SkipRequiredDiscipline192,
    SkipRequiredTimespanNonPositive,
    ExtractFailedReadHandleNotMemory,
    ExtractFailedMessageClassification,
    ExtractFailedExceptMatchedGrib1,
    ExtractFailedMapGrib1ToGrib2,
    ExtractFailedEmptyValues,
    ExtractFailedOptionOverrides,
    ExtractFailedMappings,
    ExtractFailedMarsDefaults,
    ExtractFailedMarsValidation,
    ExtractFailedMiscDefaults,
    ExtractFailedMiscValidation,
    ExtractFailedSpectralComplexOverflowProtection,
    ExtractFailedFileRead,
    EncodeFailedMars2Grib,
    ArchiveFailedSinkWrite,
    ExtractFailedUnknownException,
    ExtractFailedComplexExclusion,
};

struct ExtractionOutcome {
    MessageDisposition disposition{MessageDisposition::FailToExtract};
    ExtractionOutcomeCode code{ExtractionOutcomeCode::ExtractFailedUnknownException};
    std::string reason;
    std::string detail;

    bool shouldProceedToEncode() const { return disposition == MessageDisposition::Encode; }
};

struct ExtractedMsg {
    eckit::LocalConfiguration mars;
    eckit::LocalConfiguration misc;
    std::vector<double> values;
};

struct Grib2MarsMiscResult {
    ExtractedMsg extractedMessage;
    ExtractionOutcome extractionOutcome;
};

Grib2MarsMiscOptions makeGrib2MarsMiscOptions(const eckit::LocalConfiguration& options);

Grib2MarsMiscResult grib2MarsMisc(const eckit::message::Message& msg, const Grib2MarsMiscOptions& options);

}  // namespace multio::grib2MarsMisc

/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#include "multio/tools/utils/distGrib1ToGrib2Logging.h"

#include <fstream>
#include <sstream>
#include <stdexcept>
#include <string>
#include <chrono>
#include <ctime>
#include <iomanip>
#include <sstream>

namespace multio::distGrib1ToGrib2 {

namespace {

using grib2MarsMisc::ExtractionOutcomeCode;

constexpr std::size_t outcomeIndex(ExtractionOutcomeCode code) {
    return static_cast<std::size_t>(code);
}

std::size_t outcomeCount(const FileOutcome& o, ExtractionOutcomeCode code) {
    return o.outcomeCounters[outcomeIndex(code)];
}

std::size_t successfulMessages(const FileOutcome& o) {
    return outcomeCount(o, ExtractionOutcomeCode::ProcessedAndArchived);
}

std::size_t extractionFailures(const FileOutcome& o) {
    std::size_t total = 0;
    for (std::size_t i = outcomeIndex(ExtractionOutcomeCode::ExtractFailedReadHandleNotMemory);
         i <= outcomeIndex(ExtractionOutcomeCode::ExtractFailedUnknownException); ++i) {
        total += o.outcomeCounters[i];
    }
    return total;
}

std::size_t encodeFailures(const FileOutcome& o) {
    return outcomeCount(o, ExtractionOutcomeCode::EncodeFailedMars2Grib);
}

std::size_t archiveFailures(const FileOutcome& o) {
    return outcomeCount(o, ExtractionOutcomeCode::ArchiveFailedSinkWrite);
}

std::size_t copyOrSkipRequired(const FileOutcome& o) {
    std::size_t total = 0;
    for (std::size_t i = outcomeIndex(ExtractionOutcomeCode::CopyRequiredGrib2Verbatim);
         i <= outcomeIndex(ExtractionOutcomeCode::SkipRequiredTimespanNonPositive); ++i) {
        total += o.outcomeCounters[i];
    }
    return total;
}

std::string quoteForLog(const std::string& s) {
    std::string out;
    out.reserve(s.size() + 2);
    out.push_back('"');
    for (char c : s) {
        if (c == '"') {
            out += "\\\"";
        }
        else {
            out.push_back(c);
        }
    }
    out.push_back('"');
    return out;
}

std::string formatNonZeroCounters(const FileOutcome& o) {
    std::ostringstream out;
    bool first = true;
    for (std::size_t i = 0; i < o.outcomeCounters.size(); ++i) {
        if (o.outcomeCounters[i] == 0) {
            continue;
        }
        if (!first) {
            out << ' ';
        }
        first = false;
        const auto code = static_cast<ExtractionOutcomeCode>(i);
        out << toString(code) << '=' << o.outcomeCounters[i];
    }
    if (first) {
        out << "none";
    }
    return out.str();
}

}  // namespace

const char* toString(FileStatus status) {
    switch (status) {
        case FileStatus::Success:
            return "Success";
        case FileStatus::FailedExtract:
            return "FailedExtract";
        case FileStatus::FailedEncode:
            return "FailedEncode";
        case FileStatus::FailedArchive:
            return "FailedArchive";
        case FileStatus::FailedMixed:
            return "FailedMixed";
        case FileStatus::Partial:
            return "Partial";
        case FileStatus::Unknown:
            return "Unknown";
    }
    return "Unknown";
}

const char* toString(ExtractionOutcomeCode code) {
    switch (code) {
        case ExtractionOutcomeCode::ReadyToEncode:
            return "ReadyToEncode";
        case ExtractionOutcomeCode::ProcessedAndArchived:
            return "ProcessedAndArchived";
        case ExtractionOutcomeCode::CopyRequiredGrib2Verbatim:
            return "CopyRequiredGrib2Verbatim";
        case ExtractionOutcomeCode::CopyRequiredExceptMatched:
            return "CopyRequiredExceptMatched";
        case ExtractionOutcomeCode::CopyRequiredInvalidMessage:
            return "CopyRequiredInvalidMessage";
        case ExtractionOutcomeCode::CopyRequiredDiscipline192:
            return "CopyRequiredDiscipline192";
        case ExtractionOutcomeCode::CopyRequiredTimespanNonPositive:
            return "CopyRequiredTimespanNonPositive";
        case ExtractionOutcomeCode::SkipRequiredExcluded:
            return "SkipRequiredExcluded";
        case ExtractionOutcomeCode::SkipRequiredFilteredOut:
            return "SkipRequiredFilteredOut";
        case ExtractionOutcomeCode::SkipRequiredInvalidMessage:
            return "SkipRequiredInvalidMessage";
        case ExtractionOutcomeCode::SkipRequiredDiscipline192:
            return "SkipRequiredDiscipline192";
        case ExtractionOutcomeCode::SkipRequiredTimespanNonPositive:
            return "SkipRequiredTimespanNonPositive";
        case ExtractionOutcomeCode::ExtractFailedReadHandleNotMemory:
            return "ExtractFailedReadHandleNotMemory";
        case ExtractionOutcomeCode::ExtractFailedMessageClassification:
            return "ExtractFailedMessageClassification";
        case ExtractionOutcomeCode::ExtractFailedExceptMatchedGrib1:
            return "ExtractFailedExceptMatchedGrib1";
        case ExtractionOutcomeCode::ExtractFailedMapGrib1ToGrib2:
            return "ExtractFailedMapGrib1ToGrib2";
        case ExtractionOutcomeCode::ExtractFailedEmptyValues:
            return "ExtractFailedEmptyValues";
        case ExtractionOutcomeCode::ExtractFailedOptionOverrides:
            return "ExtractFailedOptionOverrides";
        case ExtractionOutcomeCode::ExtractFailedMappings:
            return "ExtractFailedMappings";
        case ExtractionOutcomeCode::ExtractFailedMarsDefaults:
            return "ExtractFailedMarsDefaults";
        case ExtractionOutcomeCode::ExtractFailedMarsValidation:
            return "ExtractFailedMarsValidation";
        case ExtractionOutcomeCode::ExtractFailedMiscDefaults:
            return "ExtractFailedMiscDefaults";
        case ExtractionOutcomeCode::ExtractFailedMiscValidation:
            return "ExtractFailedMiscValidation";
        case ExtractionOutcomeCode::ExtractFailedSpectralComplexOverflowProtection:
            return "ExtractFailedSpectralComplexOverflowProtection";
        case ExtractionOutcomeCode::ExtractFailedFileRead:
            return "ExtractFailedFileRead";
        case ExtractionOutcomeCode::EncodeFailedMars2Grib:
            return "EncodeFailedMars2Grib";
        case ExtractionOutcomeCode::ArchiveFailedSinkWrite:
            return "ArchiveFailedSinkWrite";
        case ExtractionOutcomeCode::ExtractFailedUnknownException:
            return "ExtractFailedUnknownException";
    }
    return "UnknownExtractionOutcomeCode";
}

FileStatus deriveFileStatus(const FileOutcome& o) {
    const auto nSuccess = successfulMessages(o);
    const auto nExtractFail = extractionFailures(o);
    const auto nEncodeFail = encodeFailures(o);
    const auto nArchiveFail = archiveFailures(o);
    const auto nClassifiedErrors = copyOrSkipRequired(o);

    if (nExtractFail > 0 && nSuccess == 0 && nEncodeFail == 0 && nArchiveFail == 0 && nClassifiedErrors == 0) {
        return FileStatus::FailedExtract;
    }
    if (nEncodeFail == 0 && nArchiveFail == 0 && nExtractFail == 0 && nClassifiedErrors == 0) {
        return FileStatus::Success;
    }
    if (nSuccess > 0 || nClassifiedErrors > 0) {
        return FileStatus::Partial;
    }
    if (nEncodeFail > 0 && nArchiveFail > 0) {
        return FileStatus::FailedMixed;
    }
    if (nEncodeFail > 0) {
        return FileStatus::FailedEncode;
    }
    if (nArchiveFail > 0) {
        return FileStatus::FailedArchive;
    }
    return FileStatus::Unknown;
}

std::string formatOutcomeLine(const FileOutcome& o) {
    std::ostringstream out;
    out << '[' << toString(deriveFileStatus(o)) << "] " << quoteForLog(o.filename) << ", " << o.nMessages
        << ", " << formatNonZeroCounters(o) << '\n';
    return out.str();
}

std::string formatRankProgressLine(const FileOutcome& o, int rank) {
    std::ostringstream out;
    out << "rank " << rank << " processed " << o.filename << " status=" << toString(deriveFileStatus(o))
        << " NMessages=" << o.nMessages << " Counters=" << formatNonZeroCounters(o);
    return out.str();
}

std::string serializeOutcomesLog(const std::vector<FileOutcome>& outcomes) {
    std::ostringstream out;
    for (const auto& outcome : outcomes) {
        out << formatOutcomeLine(outcome);
    }
    return out.str();
}

void writeGlobalOutcomeLog(const std::string& payload, const std::string& outputFile) {
    std::ofstream out(outputFile);
    if (!out) {
        throw std::runtime_error("cannot open output file: " + outputFile);
    }

    out << "# [Status] fileName, NMessages, NonZeroOutcomeCounters\n";
    out << payload;
}

std::string timestampString() {
    using clock = std::chrono::system_clock;

    const auto now = clock::now();
    const std::time_t t = clock::to_time_t(now);

    std::tm tm{};

    localtime_r(&t, &tm);

    std::ostringstream out;
    out << "[" << std::put_time(&tm, "%Y-%m-%d %H:%M:%S") << "]: ";

    return out.str();
}

}  // namespace multio::distGrib1ToGrib2

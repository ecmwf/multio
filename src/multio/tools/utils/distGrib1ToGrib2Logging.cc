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

#include <chrono>
#include <ctime>
#include <fstream>
#include <iomanip>
#include <sstream>
#include <stdexcept>
#include <string>

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

std::size_t requestedSkipOrCopyRequired(const FileOutcome& o) {
    return copyOrSkipRequired(o);
}

bool hasRealExtractFailure(const FileOutcome& o) {
    return extractionFailures(o) > 0;
}

bool hasRealEncodeFailure(const FileOutcome& o) {
    return encodeFailures(o) > 0;
}

bool hasRealArchiveFailure(const FileOutcome& o) {
    return archiveFailures(o) > 0;
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
            return "SUCCESS";
        case FileStatus::Partial:
            return "PARTIAL";
        case FileStatus::ExtractFail:
            return "EXTRACTFAIL";
        case FileStatus::EncodeFail:
            return "ENCODEFAIL";
        case FileStatus::ArchiveFail:
            return "ARCHIVEFAIL";
        case FileStatus::Fail:
            return "FAIL";
        case FileStatus::Unknown:
            return "FAIL";
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
    const auto nSkipLike = requestedSkipOrCopyRequired(o);
    const bool extractFail = hasRealExtractFailure(o);
    const bool encodeFail = hasRealEncodeFailure(o);
    const bool archiveFail = hasRealArchiveFailure(o);
    const int nFailureFamilies
        = static_cast<int>(extractFail) + static_cast<int>(encodeFail) + static_cast<int>(archiveFail);

    if (nSuccess == o.nMessages) {
        return FileStatus::Success;
    }
    if (nFailureFamilies == 0 && nSkipLike > 0 && (nSuccess + nSkipLike == o.nMessages)) {
        return FileStatus::Partial;
    }

    if (nFailureFamilies > 1) {
        return FileStatus::Fail;
    }
    if (extractFail) {
        return FileStatus::ExtractFail;
    }
    if (encodeFail) {
        return FileStatus::EncodeFail;
    }
    if (archiveFail) {
        return FileStatus::ArchiveFail;
    }
    return FileStatus::Fail;
}

std::string formatOutcomeLine(const FileOutcome& o) {
    std::ostringstream out;
    out << '[' << toString(deriveFileStatus(o)) << "] " << quoteForLog(o.filename) << ", nMessages=" << o.nMessages
        << ", " << formatNonZeroCounters(o) << '\n';
    return out.str();
}

std::string formatRankProgressLine(const FileOutcome& o, int rank) {
    std::ostringstream out;
    out << "rank " << rank << " processed " << o.filename << " status=" << toString(deriveFileStatus(o))
        << " NMessages=" << o.nMessages << " Counters=" << formatNonZeroCounters(o);
    return out.str();
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

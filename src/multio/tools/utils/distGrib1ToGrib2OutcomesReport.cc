/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#include "multio/tools/utils/distGrib1ToGrib2OutcomesReport.h"

#include <array>
#include <fstream>
#include <optional>
#include <sstream>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <utility>

#include "eckit/filesystem/PathName.h"

namespace multio::distGrib1ToGrib2 {

namespace {

using grib2MarsMisc::ExtractionOutcomeCode;

constexpr std::size_t outcomeIndex(ExtractionOutcomeCode code) {
    return static_cast<std::size_t>(code);
}

struct ParsedFileIdentity {
    std::string klass;
    std::string expver;
    std::string stream;
    std::string type;
    std::string levtype;
};

struct SummaryByLevtypeKey {
    std::string klass;
    std::string stream;
    std::string type;
    std::string levtype;

    bool operator==(const SummaryByLevtypeKey& other) const {
        return klass == other.klass && stream == other.stream && type == other.type && levtype == other.levtype;
    }
};

struct SummaryByTypeKey {
    std::string klass;
    std::string stream;
    std::string type;

    bool operator==(const SummaryByTypeKey& other) const {
        return klass == other.klass && stream == other.stream && type == other.type;
    }
};

struct SummaryByLevtypeKeyHash {
    std::size_t operator()(const SummaryByLevtypeKey& key) const {
        const auto h1 = std::hash<std::string>{}(key.klass);
        const auto h2 = std::hash<std::string>{}(key.stream);
        const auto h3 = std::hash<std::string>{}(key.type);
        const auto h4 = std::hash<std::string>{}(key.levtype);
        return (((h1 * 1315423911u) ^ h2) * 1315423911u ^ h3) * 1315423911u ^ h4;
    }
};

struct SummaryByTypeKeyHash {
    std::size_t operator()(const SummaryByTypeKey& key) const {
        const auto h1 = std::hash<std::string>{}(key.klass);
        const auto h2 = std::hash<std::string>{}(key.stream);
        const auto h3 = std::hash<std::string>{}(key.type);
        return ((h1 * 1315423911u) ^ h2) * 1315423911u ^ h3;
    }
};

struct OutcomeAggregate {
    std::size_t nFiles = 0;
    std::size_t nMessages = 0;
    std::array<std::size_t, static_cast<std::size_t>(ExtractionOutcomeCode::ExtractFailedUnknownException) + 1>
        outcomeCounters{};
};

std::vector<std::string> split(const std::string& str, char delim) {
    std::vector<std::string> parts;
    std::stringstream ss(str);
    std::string item;
    while (std::getline(ss, item, delim)) {
        parts.push_back(std::move(item));
    }
    return parts;
}

std::string joinCounters(
    const std::array<std::size_t, static_cast<std::size_t>(ExtractionOutcomeCode::ExtractFailedUnknownException) + 1>&
        counters) {
    std::ostringstream out;
    bool first = true;
    for (std::size_t i = 0; i < counters.size(); ++i) {
        if (counters[i] == 0) {
            continue;
        }
        if (!first) {
            out << ' ';
        }
        first = false;
        out << toString(static_cast<ExtractionOutcomeCode>(i)) << '=' << counters[i];
    }
    if (first) {
        out << "none";
    }
    return out.str();
}

std::string jsonEscape(const std::string& s) {
    std::ostringstream out;
    for (char c : s) {
        switch (c) {
            case '\\':
                out << "\\\\";
                break;
            case '"':
                out << "\\\"";
                break;
            case '\b':
                out << "\\b";
                break;
            case '\f':
                out << "\\f";
                break;
            case '\n':
                out << "\\n";
                break;
            case '\r':
                out << "\\r";
                break;
            case '\t':
                out << "\\t";
                break;
            default:
                out << c;
                break;
        }
    }
    return out.str();
}

std::optional<ParsedFileIdentity> parseFileIdentity(const std::string& filename) {
    eckit::PathName path(filename);
    const std::string parent = path.dirName().baseName();
    const std::string base = path.baseName();

    const auto parentFields = split(parent, ':');
    if (parentFields.size() < 3) {
        return std::nullopt;
    }

    const auto prefixPos = base.find('.');
    const std::string basePrefix = (prefixPos == std::string::npos) ? base : base.substr(0, prefixPos);
    const auto baseFields = split(basePrefix, ':');
    if (baseFields.size() < 2) {
        return std::nullopt;
    }

    return ParsedFileIdentity{parentFields[0], parentFields[1], parentFields[2], baseFields[0], baseFields[1]};
}

void accumulate(OutcomeAggregate& aggregate, const FileOutcome& outcome) {
    ++aggregate.nFiles;
    aggregate.nMessages += outcome.nMessages;
    for (std::size_t i = 0; i < aggregate.outcomeCounters.size(); ++i) {
        aggregate.outcomeCounters[i] += outcome.outcomeCounters[i];
    }
}

bool hasAny(const FileOutcome& outcome, std::initializer_list<ExtractionOutcomeCode> codes) {
    for (const auto code : codes) {
        if (outcome.outcomeCounters[outcomeIndex(code)] > 0) {
            return true;
        }
    }
    return false;
}

bool hasCopyRequired(const FileOutcome& outcome) {
    return hasAny(outcome,
                  {ExtractionOutcomeCode::CopyRequiredGrib2Verbatim, ExtractionOutcomeCode::CopyRequiredExceptMatched,
                   ExtractionOutcomeCode::CopyRequiredInvalidMessage, ExtractionOutcomeCode::CopyRequiredDiscipline192,
                   ExtractionOutcomeCode::CopyRequiredTimespanNonPositive});
}

bool hasRealExtractFailure(const FileOutcome& outcome) {
    return hasAny(outcome,
                  {ExtractionOutcomeCode::ExtractFailedReadHandleNotMemory,
                   ExtractionOutcomeCode::ExtractFailedMessageClassification,
                   ExtractionOutcomeCode::ExtractFailedExceptMatchedGrib1,
                   ExtractionOutcomeCode::ExtractFailedMapGrib1ToGrib2, ExtractionOutcomeCode::ExtractFailedEmptyValues,
                   ExtractionOutcomeCode::ExtractFailedOptionOverrides, ExtractionOutcomeCode::ExtractFailedMappings,
                   ExtractionOutcomeCode::ExtractFailedMarsDefaults, ExtractionOutcomeCode::ExtractFailedMarsValidation,
                   ExtractionOutcomeCode::ExtractFailedMiscDefaults, ExtractionOutcomeCode::ExtractFailedMiscValidation,
                   ExtractionOutcomeCode::ExtractFailedSpectralComplexOverflowProtection,
                   ExtractionOutcomeCode::ExtractFailedFileRead, ExtractionOutcomeCode::ExtractFailedUnknownException});
}

bool hasEncodeFailure(const FileOutcome& outcome) {
    return outcome.outcomeCounters[outcomeIndex(ExtractionOutcomeCode::EncodeFailedMars2Grib)] > 0;
}

bool hasArchiveFailure(const FileOutcome& outcome) {
    return outcome.outcomeCounters[outcomeIndex(ExtractionOutcomeCode::ArchiveFailedSinkWrite)] > 0;
}

bool hasInvalidSkip(const FileOutcome& outcome) {
    return outcome.outcomeCounters[outcomeIndex(ExtractionOutcomeCode::SkipRequiredInvalidMessage)] > 0;
}

std::size_t skipLikeCount(const FileOutcome& outcome) {
    std::size_t total = 0;
    for (std::size_t i = outcomeIndex(ExtractionOutcomeCode::CopyRequiredGrib2Verbatim);
         i <= outcomeIndex(ExtractionOutcomeCode::SkipRequiredTimespanNonPositive); ++i) {
        total += outcome.outcomeCounters[i];
    }
    return total;
}

bool isFullSuccess(const FileOutcome& outcome) {
    return outcome.outcomeCounters[outcomeIndex(ExtractionOutcomeCode::ProcessedAndArchived)] == outcome.nMessages
        && !hasCopyRequired(outcome) && !hasRealExtractFailure(outcome) && !hasEncodeFailure(outcome)
        && !hasArchiveFailure(outcome) && !hasInvalidSkip(outcome);
}

bool isSkipSuccess(const FileOutcome& outcome) {
    return skipLikeCount(outcome) > 0
        && outcome.outcomeCounters[outcomeIndex(ExtractionOutcomeCode::ProcessedAndArchived)] + skipLikeCount(outcome)
               == outcome.nMessages
        && !hasRealExtractFailure(outcome) && !hasEncodeFailure(outcome) && !hasArchiveFailure(outcome);
}

void writeTextFile(const std::string& path, const std::string& payload) {
    std::ofstream out(path);
    if (!out) {
        throw std::runtime_error("cannot open output file: " + path);
    }
    out << payload;
}

std::string formatSummaryByLevtypeLine(const SummaryByLevtypeKey& key, const OutcomeAggregate& aggregate) {
    std::ostringstream out;
    out << key.klass << ',' << key.stream << ',' << key.type << ',' << key.levtype << ',' << aggregate.nFiles << ','
        << aggregate.nMessages << ',' << joinCounters(aggregate.outcomeCounters) << '\n';
    return out.str();
}

std::string formatSummaryByTypeLine(const SummaryByTypeKey& key, const OutcomeAggregate& aggregate) {
    std::ostringstream out;
    out << key.klass << ',' << key.stream << ',' << key.type << ',' << aggregate.nFiles << ',' << aggregate.nMessages
        << ',' << joinCounters(aggregate.outcomeCounters) << '\n';
    return out.str();
}

void emitJsonStringField(std::ostringstream& out, const std::string& key, const std::string& value,
                         bool withComma = true) {
    out << "    \"" << key << "\": \"" << jsonEscape(value) << "\"";
    if (withComma) {
        out << ',';
    }
    out << '\n';
}

void emitJsonNullableField(std::ostringstream& out, const std::string& key, const std::optional<std::string>& value,
                           bool withComma = true) {
    out << "    \"" << key << "\": ";
    if (value) {
        out << "\"" << jsonEscape(*value) << "\"";
    }
    else {
        out << "null";
    }
    if (withComma) {
        out << ',';
    }
    out << '\n';
}

std::string buildSummaryJson(const std::vector<FileOutcome>& outcomes) {
    std::ostringstream out;
    out << "{\n  \"files\": [\n";

    for (std::size_t idx = 0; idx < outcomes.size(); ++idx) {
        const auto& outcome = outcomes[idx];
        const auto identity = parseFileIdentity(outcome.filename);

        out << "  {\n";
        emitJsonStringField(out, "status", toString(deriveFileStatus(outcome)));
        emitJsonStringField(out, "filename", outcome.filename);
        emitJsonNullableField(out, "class", identity ? std::optional<std::string>{identity->klass} : std::nullopt);
        emitJsonNullableField(out, "expver", identity ? std::optional<std::string>{identity->expver} : std::nullopt);
        emitJsonNullableField(out, "stream", identity ? std::optional<std::string>{identity->stream} : std::nullopt);
        emitJsonNullableField(out, "type", identity ? std::optional<std::string>{identity->type} : std::nullopt);
        emitJsonNullableField(out, "levtype", identity ? std::optional<std::string>{identity->levtype} : std::nullopt);
        out << "    \"nMessages\": " << outcome.nMessages << ",\n";
        out << "    \"counters\": {\n";
        for (std::size_t i = 0; i < outcome.outcomeCounters.size(); ++i) {
            out << "      \"" << toString(static_cast<ExtractionOutcomeCode>(i))
                << "\": " << outcome.outcomeCounters[i];
            if (i + 1 != outcome.outcomeCounters.size()) {
                out << ',';
            }
            out << '\n';
        }
        out << "    }\n";
        out << "  }";
        if (idx + 1 != outcomes.size()) {
            out << ',';
        }
        out << '\n';
    }

    out << "  ]\n}\n";
    return out.str();
}

std::string buildSummaryDocumentation() {
    return R"MD(# Summary Files

The distributed tool writes:

- top-level summary files:
  - `Summary.log`: human-readable, one line per processed input file
  - `Summary.json`: machine-readable JSON representation of the same per-file summary
- an `output/` directory containing the produced GRIB files, typically one file per rank
- a `logging/` directory containing report views and status-partitioned file lists

## Summary.log format

Each line has the form:

```text
[STATUS] "filename", nMessages=N, CounterA=X CounterB=Y ...
```

- `STATUS` is the overall file status
- `filename` is the input file path
- `nMessages` is the number of messages read from that input file
- the remaining fields are the non-zero outcome counters for that file

## Status meanings

- `SUCCESS`: all messages in the file were extracted, encoded, and archived successfully
- `PARTIAL`: there were no genuine failures, but at least one message was skipped by policy or classified as copy-required; all other messages were extracted, encoded, and archived successfully
- `EXTRACTFAIL`: the file contains genuine failures and all of them are extraction failures
- `ENCODEFAIL`: the file contains genuine failures and all of them are encoding failures
- `ARCHIVEFAIL`: the file contains genuine failures and all of them are archiving failures
- `FAIL`: the file contains a mix of different genuine failure families

## Notes

- In distributed summaries, `CopyRequired*` outcomes are treated as skip-like outcomes
- `Summary.json` includes the same status plus all counters, and also extracts `class`, `expver`, `stream`, `type`, and `levtype` when they can be inferred from the input path
- The `logging/` directory contains one filename-only list per final status:
  - `success_list.txt`
  - `partial_list.txt`
  - `extractfail_list.txt`
  - `encodefail_list.txt`
  - `archivefail_list.txt`
  - `fail_list.txt`
- Each input file appears in exactly one of those six list files
)MD";
}

}  // namespace

std::string serializeFileOutcomes(const std::vector<FileOutcome>& outcomes) {
    std::ostringstream out;
    for (const auto& outcome : outcomes) {
        out << outcome.filename << '\t' << outcome.nMessages;
        for (const auto count : outcome.outcomeCounters) {
            out << '\t' << count;
        }
        out << '\n';
    }
    return out.str();
}

std::vector<FileOutcome> deserializeFileOutcomes(const std::string& payload) {
    std::vector<FileOutcome> outcomes;
    std::istringstream in(payload);
    std::string line;
    while (std::getline(in, line)) {
        if (line.empty()) {
            continue;
        }

        auto fields = split(line, '\t');
        if (fields.size() != 2 + static_cast<std::size_t>(ExtractionOutcomeCode::ExtractFailedUnknownException) + 1) {
            throw std::runtime_error("invalid serialized FileOutcome record");
        }

        FileOutcome outcome;
        outcome.filename = std::move(fields[0]);
        outcome.nMessages = static_cast<std::size_t>(std::stoull(fields[1]));
        for (std::size_t i = 0; i < outcome.outcomeCounters.size(); ++i) {
            outcome.outcomeCounters[i] = static_cast<std::size_t>(std::stoull(fields[i + 2]));
        }
        outcomes.push_back(std::move(outcome));
    }
    return outcomes;
}

DistGrib1ToGrib2ReportPaths makeReportPaths(const std::string& outputDirectory) {
    const std::string loggingSubdir = outputDirectory + "/logging";
    return DistGrib1ToGrib2ReportPaths{outputDirectory + "/Summary.log",
                                       outputDirectory + "/Summary.json",
                                       outputDirectory + "/README",
                                       loggingSubdir,
                                       loggingSubdir + "/SummaryByClassStreamTypeLevtype.log",
                                       loggingSubdir + "/SummaryByClassStreamType.log",
                                       loggingSubdir + "/success_list.txt",
                                       loggingSubdir + "/partial_list.txt",
                                       loggingSubdir + "/extractfail_list.txt",
                                       loggingSubdir + "/encodefail_list.txt",
                                       loggingSubdir + "/archivefail_list.txt",
                                       loggingSubdir + "/fail_list.txt"};
}

void writeOutcomeReports(const std::vector<FileOutcome>& outcomes, const DistGrib1ToGrib2ReportPaths& paths) {
    std::ostringstream perFile;
    perFile << "# [Status] fileName, nMessages=<N>, NonZeroOutcomeCounters\n";

    std::unordered_map<SummaryByLevtypeKey, OutcomeAggregate, SummaryByLevtypeKeyHash> summaryByLevtype;
    std::unordered_map<SummaryByTypeKey, OutcomeAggregate, SummaryByTypeKeyHash> summaryByType;

    std::ostringstream successList;
    std::ostringstream partialList;
    std::ostringstream extractFailList;
    std::ostringstream encodeFailList;
    std::ostringstream archiveFailList;
    std::ostringstream failList;

    for (const auto& outcome : outcomes) {
        perFile << formatOutcomeLine(outcome);
        const auto status = deriveFileStatus(outcome);

        if (auto identity = parseFileIdentity(outcome.filename)) {
            accumulate(summaryByLevtype[SummaryByLevtypeKey{identity->klass, identity->stream, identity->type,
                                                            identity->levtype}],
                       outcome);
            accumulate(summaryByType[SummaryByTypeKey{identity->klass, identity->stream, identity->type}], outcome);
        }

        switch (status) {
            case FileStatus::Success:
                successList << outcome.filename << '\n';
                break;
            case FileStatus::Partial:
                partialList << outcome.filename << '\n';
                break;
            case FileStatus::ExtractFail:
                extractFailList << outcome.filename << '\n';
                break;
            case FileStatus::EncodeFail:
                encodeFailList << outcome.filename << '\n';
                break;
            case FileStatus::ArchiveFail:
                archiveFailList << outcome.filename << '\n';
                break;
            case FileStatus::Fail:
            case FileStatus::Unknown:
                failList << outcome.filename << '\n';
                break;
        }
    }

    std::ostringstream byLevtype;
    byLevtype << "# class,stream,type,levtype,NFiles,NMessages,NonZeroOutcomeCounters\n";
    for (const auto& entry : summaryByLevtype) {
        byLevtype << formatSummaryByLevtypeLine(entry.first, entry.second);
    }

    std::ostringstream byType;
    byType << "# class,stream,type,NFiles,NMessages,NonZeroOutcomeCounters\n";
    for (const auto& entry : summaryByType) {
        byType << formatSummaryByTypeLine(entry.first, entry.second);
    }

    writeTextFile(paths.summaryLog, perFile.str());
    writeTextFile(paths.summaryJson, buildSummaryJson(outcomes));
    writeTextFile(paths.readme, buildSummaryDocumentation());
    writeTextFile(paths.byClassStreamTypeLevtypeLog, byLevtype.str());
    writeTextFile(paths.byClassStreamTypeLog, byType.str());
    writeTextFile(paths.successList, successList.str());
    writeTextFile(paths.partialList, partialList.str());
    writeTextFile(paths.extractFailList, extractFailList.str());
    writeTextFile(paths.encodeFailList, encodeFailList.str());
    writeTextFile(paths.archiveFailList, archiveFailList.str());
    writeTextFile(paths.failList, failList.str());
}

}  // namespace multio::distGrib1ToGrib2

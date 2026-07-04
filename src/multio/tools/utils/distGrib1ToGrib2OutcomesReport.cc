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

std::string joinCounters(const std::array<std::size_t, static_cast<std::size_t>(ExtractionOutcomeCode::ExtractFailedUnknownException) + 1>& counters) {
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

bool hasRequestedSkips(const FileOutcome& outcome) {
    return hasAny(outcome, {ExtractionOutcomeCode::SkipRequiredExcluded,
                            ExtractionOutcomeCode::SkipRequiredFilteredOut,
                            ExtractionOutcomeCode::SkipRequiredDiscipline192,
                            ExtractionOutcomeCode::SkipRequiredTimespanNonPositive});
}

bool hasCopyRequired(const FileOutcome& outcome) {
    return hasAny(outcome, {ExtractionOutcomeCode::CopyRequiredGrib2Verbatim,
                            ExtractionOutcomeCode::CopyRequiredExceptMatched,
                            ExtractionOutcomeCode::CopyRequiredInvalidMessage,
                            ExtractionOutcomeCode::CopyRequiredDiscipline192,
                            ExtractionOutcomeCode::CopyRequiredTimespanNonPositive});
}

bool hasRealExtractFailure(const FileOutcome& outcome) {
    return hasAny(outcome, {ExtractionOutcomeCode::ExtractFailedReadHandleNotMemory,
                            ExtractionOutcomeCode::ExtractFailedMessageClassification,
                            ExtractionOutcomeCode::ExtractFailedExceptMatchedGrib1,
                            ExtractionOutcomeCode::ExtractFailedMapGrib1ToGrib2,
                            ExtractionOutcomeCode::ExtractFailedEmptyValues,
                            ExtractionOutcomeCode::ExtractFailedOptionOverrides,
                            ExtractionOutcomeCode::ExtractFailedMappings,
                            ExtractionOutcomeCode::ExtractFailedMarsDefaults,
                            ExtractionOutcomeCode::ExtractFailedMarsValidation,
                            ExtractionOutcomeCode::ExtractFailedMiscDefaults,
                            ExtractionOutcomeCode::ExtractFailedMiscValidation,
                            ExtractionOutcomeCode::ExtractFailedSpectralComplexOverflowProtection,
                            ExtractionOutcomeCode::ExtractFailedFileRead,
                            ExtractionOutcomeCode::ExtractFailedUnknownException});
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

std::size_t requestedSkipCount(const FileOutcome& outcome) {
    return outcome.outcomeCounters[outcomeIndex(ExtractionOutcomeCode::SkipRequiredExcluded)]
           + outcome.outcomeCounters[outcomeIndex(ExtractionOutcomeCode::SkipRequiredFilteredOut)]
           + outcome.outcomeCounters[outcomeIndex(ExtractionOutcomeCode::SkipRequiredDiscipline192)]
           + outcome.outcomeCounters[outcomeIndex(ExtractionOutcomeCode::SkipRequiredTimespanNonPositive)];
}

bool isFullSuccess(const FileOutcome& outcome) {
    return outcome.outcomeCounters[outcomeIndex(ExtractionOutcomeCode::ProcessedAndArchived)] == outcome.nMessages
           && !hasCopyRequired(outcome) && !hasRealExtractFailure(outcome)
           && !hasEncodeFailure(outcome) && !hasArchiveFailure(outcome) && !hasInvalidSkip(outcome);
}

bool isSkipSuccess(const FileOutcome& outcome) {
    return outcome.outcomeCounters[outcomeIndex(ExtractionOutcomeCode::ProcessedAndArchived)] + requestedSkipCount(outcome)
                   == outcome.nMessages
           && !hasCopyRequired(outcome) && !hasRealExtractFailure(outcome) && !hasEncodeFailure(outcome)
           && !hasArchiveFailure(outcome) && !hasInvalidSkip(outcome);
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

DistGrib1ToGrib2ReportPaths makeReportPaths(const std::string& outputPrefix) {
    return DistGrib1ToGrib2ReportPaths{outputPrefix + "_GlobalOutcome.log",
                                       outputPrefix + "_SummaryByClassStreamTypeLevtype.log",
                                       outputPrefix + "_SummaryByClassStreamType.log",
                                       outputPrefix + "_FullSuccess.list",
                                       outputPrefix + "_SkipSuccess.list",
                                       outputPrefix + "_EncodingFailures.list",
                                       outputPrefix + "_ArchiveFailures.list",
                                       outputPrefix + "_ExtractFailures.list"};
}

void writeOutcomeReports(const std::vector<FileOutcome>& outcomes, const DistGrib1ToGrib2ReportPaths& paths) {
    std::ostringstream perFile;
    perFile << "# [Status] fileName, NMessages, NonZeroOutcomeCounters\n";

    std::unordered_map<SummaryByLevtypeKey, OutcomeAggregate, SummaryByLevtypeKeyHash> summaryByLevtype;
    std::unordered_map<SummaryByTypeKey, OutcomeAggregate, SummaryByTypeKeyHash> summaryByType;

    std::ostringstream fullSuccess;
    std::ostringstream skipSuccess;
    std::ostringstream encodeFailures;
    std::ostringstream archiveFailures;
    std::ostringstream extractFailures;

    for (const auto& outcome : outcomes) {
        perFile << formatOutcomeLine(outcome);

        if (auto identity = parseFileIdentity(outcome.filename)) {
            accumulate(summaryByLevtype[SummaryByLevtypeKey{identity->klass, identity->stream, identity->type, identity->levtype}],
                       outcome);
            accumulate(summaryByType[SummaryByTypeKey{identity->klass, identity->stream, identity->type}], outcome);
        }

        if (isFullSuccess(outcome)) {
            fullSuccess << outcome.filename << '\n';
        }
        if (isSkipSuccess(outcome)) {
            skipSuccess << outcome.filename << '\n';
        }
        if (hasEncodeFailure(outcome)) {
            encodeFailures << outcome.filename << '\n';
        }
        if (hasArchiveFailure(outcome)) {
            archiveFailures << outcome.filename << '\n';
        }
        if (hasRealExtractFailure(outcome)) {
            extractFailures << outcome.filename << '\n';
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

    writeTextFile(paths.perFileLog, perFile.str());
    writeTextFile(paths.byClassStreamTypeLevtypeLog, byLevtype.str());
    writeTextFile(paths.byClassStreamTypeLog, byType.str());
    writeTextFile(paths.fullSuccessList, fullSuccess.str());
    writeTextFile(paths.skipSuccessList, skipSuccess.str());
    writeTextFile(paths.encodeFailureList, encodeFailures.str());
    writeTextFile(paths.archiveFailureList, archiveFailures.str());
    writeTextFile(paths.extractFailureList, extractFailures.str());
}

}  // namespace multio::distGrib1ToGrib2

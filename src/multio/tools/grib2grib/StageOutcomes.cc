/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#include "multio/tools/grib2grib/StageOutcomes.h"

#include "eckit/exception/Exceptions.h"

#include <sstream>
#include <string>
#include <utility>
#include <vector>

namespace multio::distGrib1ToGrib2::grib2grib {

namespace {

template <typename Counters>
std::size_t countNonSuccess(const Counters& counters) {
    std::size_t total = 0;
    for (std::size_t i = 1; i < counters.counts.size(); ++i) {
        total += counters.counts[i];
    }
    return total;
}

template <typename Counters>
std::size_t countSuccess(const Counters& counters) {
    return counters.counts[0];
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

std::vector<std::string> split(const std::string& str, char delim) {
    std::vector<std::string> parts;
    std::istringstream in(str);
    std::string item;
    while (std::getline(in, item, delim)) {
        parts.push_back(std::move(item));
    }
    return parts;
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

template <typename Enum, typename Counters>
std::string countersToJson(const Counters& counters) {
    std::ostringstream out;
    out << '{';
    for (std::size_t i = 0; i < counters.counts.size(); ++i) {
        if (i != 0) {
            out << ',';
        }
        out << '"' << toString(static_cast<Enum>(i)) << "\":" << counters.counts[i];
    }
    out << '}';
    return out.str();
}

template <typename Enum, typename Counters>
std::string formatCounters(const char* label, const Counters& counters) {
    std::ostringstream out;
    out << label << '{';

    bool first = true;
    for (std::size_t i = 0; i < counters.counts.size(); ++i) {
        if (counters.counts[i] == 0) {
            continue;
        }
        if (!first) {
            out << ' ';
        }
        first = false;
        out << toString(static_cast<Enum>(i)) << '=' << counters.counts[i];
    }

    if (first) {
        out << "none";
    }
    out << '}';
    return out.str();
}

template <typename Counters>
void appendCounts(std::ostringstream& out, const Counters& counters) {
    for (const auto count : counters.counts) {
        out << '\t' << count;
    }
}

template <typename Counters>
void parseCounts(const std::vector<std::string>& fields, std::size_t& pos, Counters& counters) {
    for (std::size_t i = 0; i < counters.counts.size(); ++i, ++pos) {
        counters.counts[i] = static_cast<std::size_t>(std::stoull(fields[pos]));
    }
}

constexpr std::size_t serializedFieldCount() {
    return 7 + OpenFileCounters::size() + ReadMessageCounters::size() + GribBasedFilterCounters::size()
         + GribToMarsCounters::size() + MarsOverridesCounters::size() + MarsToMarsCounters::size()
         + MarsBasedFilterCounters::size() + MarsToGribCounters::size() + PostEncodeValidationCounters::size()
         + Grib2Fdb5Counters::size() + FileFlushCounters::size();
}

}  // namespace

const char* toString(ProcessingStage stage) {
    switch (stage) {
        case ProcessingStage::OpenFile:
            return "OpenFile";
        case ProcessingStage::ReadMessage:
            return "ReadMessage";
        case ProcessingStage::GribBasedFilter:
            return "GribBasedFilter";
        case ProcessingStage::GribToMars:
            return "GribToMars";
        case ProcessingStage::MarsToMars:
            return "MarsToMars";
        case ProcessingStage::MarsOverrides:
            return "MarsOverrides";
        case ProcessingStage::MarsBasedFilter:
            return "MarsBasedFilter";
        case ProcessingStage::MarsToGrib:
            return "MarsToGrib";
        case ProcessingStage::PostEncodeValidation:
            return "PostEncodeValidation";
        case ProcessingStage::Grib2Fdb5:
            return "Grib2Fdb5";
        case ProcessingStage::FileFlush:
            return "FileFlush";
    }
    return "UnknownProcessingStage";
}

const char* toString(FileSummary summary) {
    switch (summary) {
        case FileSummary::Success:
            return "SUCCESS";
        case FileSummary::Partial:
            return "PARTIAL";
        case FileSummary::Fail:
            return "FAIL";
    }
    return "FAIL";
}

const char* toString(OpenFileCode code) {
    switch (code) {
        case OpenFileCode::Valid:
            return "Valid";
        case OpenFileCode::OpenFailed:
            return "OpenFailed";
        case OpenFileCode::UnknownFailure:
            return "UnknownFailure";
    }
    return "UnknownOpenFileCode";
}

const char* toString(ReadMessageCode code) {
    switch (code) {
        case ReadMessageCode::Valid:
            return "Valid";
        case ReadMessageCode::ReadFailed:
            return "ReadFailed";
        case ReadMessageCode::UnknownFailure:
            return "UnknownFailure";
    }
    return "UnknownReadMessageCode";
}

const char* toString(GribBasedFilterCode code) {
    switch (code) {
        case GribBasedFilterCode::Accepted:
            return "Accepted";
        case GribBasedFilterCode::RejectedDiscipline192:
            return "RejectedDiscipline192";
        case GribBasedFilterCode::RejectedGrib1ByEditionPolicy:
            return "RejectedGrib1ByEditionPolicy";
        case GribBasedFilterCode::RejectedGrib2ByEditionPolicy:
            return "RejectedGrib2ByEditionPolicy";
        case GribBasedFilterCode::RejectedInvalidInputMessage:
            return "RejectedInvalidInputMessage";
        case GribBasedFilterCode::FailedGribBasedFilter:
            return "FailedGribBasedFilter";
    }
    return "UnknownGribBasedFilterCode";
}

const char* toString(GribToMarsCode code) {
    switch (code) {
        case GribToMarsCode::Valid:
            return "Valid";
        case GribToMarsCode::MapGribToMarsFailed:
            return "MapGribToMarsFailed";
        case GribToMarsCode::ValuesExtractionFailed:
            return "ValuesExtractionFailed";
        case GribToMarsCode::UnknownFailure:
            return "UnknownFailure";
    }
    return "UnknownGribToMarsCode";
}

const char* toString(MarsOverridesCode code) {
    switch (code) {
        case MarsOverridesCode::Valid:
            return "Valid";
        case MarsOverridesCode::OptionOverridesFailed:
            return "OptionOverridesFailed";
        case MarsOverridesCode::UnknownFailure:
            return "UnknownFailure";
    }
    return "UnknownMarsOverridesCode";
}

const char* toString(MarsToMarsCode code) {
    switch (code) {
        case MarsToMarsCode::Valid:
            return "Valid";
        case MarsToMarsCode::MappingsFailed:
            return "MappingsFailed";
        case MarsToMarsCode::MergeMiscFailed:
            return "MergeMiscFailed";
        case MarsToMarsCode::MarsDefaultsFailed:
            return "MarsDefaultsFailed";
        case MarsToMarsCode::MarsValidationFailed:
            return "MarsValidationFailed";
        case MarsToMarsCode::MiscDefaultsFailed:
            return "MiscDefaultsFailed";
        case MarsToMarsCode::MiscValidationFailed:
            return "MiscValidationFailed";
        case MarsToMarsCode::UnknownFailure:
            return "UnknownFailure";
    }
    return "UnknownMarsToMarsCode";
}

const char* toString(MarsBasedFilterCode code) {
    switch (code) {
        case MarsBasedFilterCode::Accepted:
            return "Accepted";
        case MarsBasedFilterCode::Rejected:
            return "Rejected";
    }
    return "UnknownMarsBasedFilterCode";
}

const char* toString(MarsToGribCode code) {
    switch (code) {
        case MarsToGribCode::Valid:
            return "Valid";
        case MarsToGribCode::EncodeFailed:
            return "EncodeFailed";
        case MarsToGribCode::TestCaseGenerationFailed:
            return "TestCaseGenerationFailed";
        case MarsToGribCode::TestCaseWriteFailed:
            return "TestCaseWriteFailed";
        case MarsToGribCode::UnknownFailure:
            return "UnknownFailure";
    }
    return "UnknownMarsToGribCode";
}

const char* toString(PostEncodeValidationCode code) {
    switch (code) {
        case PostEncodeValidationCode::Valid:
            return "Valid";
        case PostEncodeValidationCode::InvalidEncodedMessage:
            return "InvalidEncodedMessage";
    }
    return "UnknownPostEncodeValidationCode";
}

const char* toString(Grib2Fdb5Code code) {
    switch (code) {
        case Grib2Fdb5Code::Valid:
            return "Valid";
        case Grib2Fdb5Code::ArchiveFailed:
            return "ArchiveFailed";
        case Grib2Fdb5Code::UnknownFailure:
            return "UnknownFailure";
    }
    return "UnknownGrib2Fdb5Code";
}

const char* toString(FileFlushCode code) {
    switch (code) {
        case FileFlushCode::Valid:
            return "Valid";
        case FileFlushCode::FileFlushFailed:
            return "FileFlushFailed";
        case FileFlushCode::UnknownFailure:
            return "UnknownFailure";
    }
    return "UnknownFileFlushCode";
}

std::string toJson(const OpenFileCounters& counters) {
    return countersToJson<OpenFileCode>(counters);
}

std::string toJson(const ReadMessageCounters& counters) {
    return countersToJson<ReadMessageCode>(counters);
}

std::string toJson(const GribBasedFilterCounters& counters) {
    return countersToJson<GribBasedFilterCode>(counters);
}

std::string toJson(const GribToMarsCounters& counters) {
    return countersToJson<GribToMarsCode>(counters);
}

std::string toJson(const MarsOverridesCounters& counters) {
    return countersToJson<MarsOverridesCode>(counters);
}

std::string toJson(const MarsToMarsCounters& counters) {
    return countersToJson<MarsToMarsCode>(counters);
}

std::string toJson(const MarsBasedFilterCounters& counters) {
    return countersToJson<MarsBasedFilterCode>(counters);
}

std::string toJson(const MarsToGribCounters& counters) {
    return countersToJson<MarsToGribCode>(counters);
}

std::string toJson(const PostEncodeValidationCounters& counters) {
    return countersToJson<PostEncodeValidationCode>(counters);
}

std::string toJson(const Grib2Fdb5Counters& counters) {
    return countersToJson<Grib2Fdb5Code>(counters);
}

std::string toJson(const FileFlushCounters& counters) {
    return countersToJson<FileFlushCode>(counters);
}

void FileStageOutcomes::add(const FileStageOutcomes& other) {
    nMessages += other.nMessages;
    nFailedMarsToGribTestCaseGenerations += other.nFailedMarsToGribTestCaseGenerations;
    nFailedMarsToGribTestCaseWrites += other.nFailedMarsToGribTestCaseWrites;
    nCloseFailures += other.nCloseFailures;
    nGenericProcessOneMessageFailures += other.nGenericProcessOneMessageFailures;
    nGenericProcessUnitOfWorkFailures += other.nGenericProcessUnitOfWorkFailures;

    openFile.add(other.openFile);
    readMessage.add(other.readMessage);
    gribBasedFilter.add(other.gribBasedFilter);
    gribToMars.add(other.gribToMars);
    marsToMars.add(other.marsToMars);
    marsOverrides.add(other.marsOverrides);
    marsBasedFilter.add(other.marsBasedFilter);
    marsToGrib.add(other.marsToGrib);
    postEncodeValidation.add(other.postEncodeValidation);
    grib2Fdb5.add(other.grib2Fdb5);
    fileFlush.add(other.fileFlush);
}

FileSummary deriveSummary(const FileStageOutcomes& outcomes) {
    const std::size_t failures = countNonSuccess(outcomes.openFile) + countNonSuccess(outcomes.readMessage)
                               + countNonSuccess(outcomes.gribToMars) + countNonSuccess(outcomes.marsOverrides)
                               + countNonSuccess(outcomes.marsToMars) + countNonSuccess(outcomes.marsToGrib)
                               + countNonSuccess(outcomes.postEncodeValidation) + countNonSuccess(outcomes.grib2Fdb5)
                               + countNonSuccess(outcomes.fileFlush);

    if (failures > 0) {
        return FileSummary::Fail;
    }

    if (outcomes.openFile.get(OpenFileCode::Valid) != 1 || outcomes.fileFlush.get(FileFlushCode::Valid) != 1) {
        return FileSummary::Fail;
    }

    const std::size_t intentionalRejects
        = countNonSuccess(outcomes.gribBasedFilter) + countNonSuccess(outcomes.marsBasedFilter);
    const std::size_t converted = countSuccess(outcomes.grib2Fdb5);

    if (converted + intentionalRejects != outcomes.nMessages) {
        return FileSummary::Fail;
    }

    if (intentionalRejects > 0) {
        return FileSummary::Partial;
    }

    return (converted == outcomes.nMessages) ? FileSummary::Success : FileSummary::Fail;
}

void accumulate(OutcomeAggregate& aggregate, const FileStageOutcomes& outcomes) {
    ++aggregate.nFiles;
    aggregate.nMessages += outcomes.nMessages;
    aggregate.nFailedMarsToGribTestCaseGenerations += outcomes.nFailedMarsToGribTestCaseGenerations;
    aggregate.nFailedMarsToGribTestCaseWrites += outcomes.nFailedMarsToGribTestCaseWrites;
    aggregate.nCloseFailures += outcomes.nCloseFailures;
    aggregate.nGenericProcessOneMessageFailures += outcomes.nGenericProcessOneMessageFailures;
    aggregate.nGenericProcessUnitOfWorkFailures += outcomes.nGenericProcessUnitOfWorkFailures;

    aggregate.openFile.add(outcomes.openFile);
    aggregate.readMessage.add(outcomes.readMessage);
    aggregate.gribBasedFilter.add(outcomes.gribBasedFilter);
    aggregate.gribToMars.add(outcomes.gribToMars);
    aggregate.marsOverrides.add(outcomes.marsOverrides);
    aggregate.marsToMars.add(outcomes.marsToMars);
    aggregate.marsBasedFilter.add(outcomes.marsBasedFilter);
    aggregate.marsToGrib.add(outcomes.marsToGrib);
    aggregate.postEncodeValidation.add(outcomes.postEncodeValidation);
    aggregate.grib2Fdb5.add(outcomes.grib2Fdb5);
    aggregate.fileFlush.add(outcomes.fileFlush);
}

std::string formatOutcomeLine(const FileStageOutcomes& outcomes) {
    std::ostringstream out;
    out << '[' << toString(deriveSummary(outcomes)) << "] " << quoteForLog(outcomes.filename)
        << ", nMessages=" << outcomes.nMessages << ", "
        << "nFailedMarsToGribTestCaseGenerations=" << outcomes.nFailedMarsToGribTestCaseGenerations << ", "
        << "nFailedMarsToGribTestCaseWrites=" << outcomes.nFailedMarsToGribTestCaseWrites << ", "
        << "nCloseFailures=" << outcomes.nCloseFailures << ", "
        << "nGenericProcessOneMessageFailures=" << outcomes.nGenericProcessOneMessageFailures << ", "
        << "nGenericProcessUnitOfWorkFailures=" << outcomes.nGenericProcessUnitOfWorkFailures << ", "
        << formatCounters<OpenFileCode>("OpenFile", outcomes.openFile) << ' '
        << formatCounters<ReadMessageCode>("ReadMessage", outcomes.readMessage) << ' '
        << formatCounters<GribBasedFilterCode>("GribBasedFilter", outcomes.gribBasedFilter) << ' '
        << formatCounters<GribToMarsCode>("GribToMars", outcomes.gribToMars) << ' '
        << formatCounters<MarsOverridesCode>("MarsOverrides", outcomes.marsOverrides) << ' '
        << formatCounters<MarsToMarsCode>("MarsToMars", outcomes.marsToMars) << ' '
        << formatCounters<MarsBasedFilterCode>("MarsBasedFilter", outcomes.marsBasedFilter) << ' '
        << formatCounters<MarsToGribCode>("MarsToGrib", outcomes.marsToGrib) << ' '
        << formatCounters<PostEncodeValidationCode>("PostEncodeValidation", outcomes.postEncodeValidation) << ' '
        << formatCounters<Grib2Fdb5Code>("Grib2Fdb5", outcomes.grib2Fdb5) << ' '
        << formatCounters<FileFlushCode>("FileFlush", outcomes.fileFlush) << '\n';
    return out.str();
}

std::string formatAggregateLine(const OutcomeAggregate& aggregate) {
    std::ostringstream out;
    out << "nFiles=" << aggregate.nFiles << ", nMessages=" << aggregate.nMessages
        << ", nFailedMarsToGribTestCaseGenerations=" << aggregate.nFailedMarsToGribTestCaseGenerations
        << ", nFailedMarsToGribTestCaseWrites=" << aggregate.nFailedMarsToGribTestCaseWrites
        << ", nCloseFailures=" << aggregate.nCloseFailures
        << ", nGenericProcessOneMessageFailures=" << aggregate.nGenericProcessOneMessageFailures
        << ", nGenericProcessUnitOfWorkFailures=" << aggregate.nGenericProcessUnitOfWorkFailures << ", "
        << formatCounters<OpenFileCode>("OpenFile", aggregate.openFile) << ' '
        << formatCounters<ReadMessageCode>("ReadMessage", aggregate.readMessage) << ' '
        << formatCounters<GribBasedFilterCode>("GribBasedFilter", aggregate.gribBasedFilter) << ' '
        << formatCounters<GribToMarsCode>("GribToMars", aggregate.gribToMars) << ' '
        << formatCounters<MarsOverridesCode>("MarsOverrides", aggregate.marsOverrides) << ' '
        << formatCounters<MarsToMarsCode>("MarsToMars", aggregate.marsToMars) << ' '
        << formatCounters<MarsBasedFilterCode>("MarsBasedFilter", aggregate.marsBasedFilter) << ' '
        << formatCounters<MarsToGribCode>("MarsToGrib", aggregate.marsToGrib) << ' '
        << formatCounters<PostEncodeValidationCode>("PostEncodeValidation", aggregate.postEncodeValidation) << ' '
        << formatCounters<Grib2Fdb5Code>("Grib2Fdb5", aggregate.grib2Fdb5) << ' '
        << formatCounters<FileFlushCode>("FileFlush", aggregate.fileFlush);
    return out.str();
}

std::string toJson(const FileStageOutcomes& outcomes) {
    std::ostringstream out;
    out << "{\n"
        << "  \"summary\": \"" << toString(deriveSummary(outcomes)) << "\",\n"
        << "  \"filename\": \"" << jsonEscape(outcomes.filename) << "\",\n"
        << "  \"nMessages\": " << outcomes.nMessages << ",\n"
        << "  \"nFailedMarsToGribTestCaseGenerations\": " << outcomes.nFailedMarsToGribTestCaseGenerations << ",\n"
        << "  \"nFailedMarsToGribTestCaseWrites\": " << outcomes.nFailedMarsToGribTestCaseWrites << ",\n"
        << "  \"nCloseFailures\": " << outcomes.nCloseFailures << ",\n"
        << "  \"nGenericProcessOneMessageFailures\": " << outcomes.nGenericProcessOneMessageFailures << ",\n"
        << "  \"nGenericProcessUnitOfWorkFailures\": " << outcomes.nGenericProcessUnitOfWorkFailures << ",\n"
        << "  \"openFile\": " << toJson(outcomes.openFile) << ",\n"
        << "  \"readMessage\": " << toJson(outcomes.readMessage) << ",\n"
        << "  \"gribBasedFilter\": " << toJson(outcomes.gribBasedFilter) << ",\n"
        << "  \"gribToMars\": " << toJson(outcomes.gribToMars) << ",\n"
        << "  \"marsOverrides\": " << toJson(outcomes.marsOverrides) << ",\n"
        << "  \"marsToMars\": " << toJson(outcomes.marsToMars) << ",\n"
        << "  \"marsBasedFilter\": " << toJson(outcomes.marsBasedFilter) << ",\n"
        << "  \"marsToGrib\": " << toJson(outcomes.marsToGrib) << ",\n"
        << "  \"postEncodeValidation\": " << toJson(outcomes.postEncodeValidation) << ",\n"
        << "  \"grib2Fdb5\": " << toJson(outcomes.grib2Fdb5) << ",\n"
        << "  \"fileFlush\": " << toJson(outcomes.fileFlush) << "\n"
        << '}';
    return out.str();
}

std::string toJson(const std::vector<FileStageOutcomes>& outcomes) {
    std::ostringstream out;
    out << "{\n  \"files\": [\n";
    for (std::size_t i = 0; i < outcomes.size(); ++i) {
        out << toJson(outcomes[i]);
        if (i + 1 != outcomes.size()) {
            out << ',';
        }
        out << '\n';
    }
    out << "  ]\n}";
    return out.str();
}

std::string toJson(const OutcomeAggregate& aggregate) {
    std::ostringstream out;
    out << "{\n"
        << "  \"nFiles\": " << aggregate.nFiles << ",\n"
        << "  \"nMessages\": " << aggregate.nMessages << ",\n"
        << "  \"nFailedMarsToGribTestCaseGenerations\": " << aggregate.nFailedMarsToGribTestCaseGenerations << ",\n"
        << "  \"nFailedMarsToGribTestCaseWrites\": " << aggregate.nFailedMarsToGribTestCaseWrites << ",\n"
        << "  \"nCloseFailures\": " << aggregate.nCloseFailures << ",\n"
        << "  \"nGenericProcessOneMessageFailures\": " << aggregate.nGenericProcessOneMessageFailures << ",\n"
        << "  \"nGenericProcessUnitOfWorkFailures\": " << aggregate.nGenericProcessUnitOfWorkFailures << ",\n"
        << "  \"openFile\": " << toJson(aggregate.openFile) << ",\n"
        << "  \"readMessage\": " << toJson(aggregate.readMessage) << ",\n"
        << "  \"gribBasedFilter\": " << toJson(aggregate.gribBasedFilter) << ",\n"
        << "  \"gribToMars\": " << toJson(aggregate.gribToMars) << ",\n"
        << "  \"marsOverrides\": " << toJson(aggregate.marsOverrides) << ",\n"
        << "  \"marsToMars\": " << toJson(aggregate.marsToMars) << ",\n"
        << "  \"marsBasedFilter\": " << toJson(aggregate.marsBasedFilter) << ",\n"
        << "  \"marsToGrib\": " << toJson(aggregate.marsToGrib) << ",\n"
        << "  \"postEncodeValidation\": " << toJson(aggregate.postEncodeValidation) << ",\n"
        << "  \"grib2Fdb5\": " << toJson(aggregate.grib2Fdb5) << ",\n"
        << "  \"fileFlush\": " << toJson(aggregate.fileFlush) << "\n"
        << '}';
    return out.str();
}

std::string serializeFileStageOutcomes(const std::vector<FileStageOutcomes>& outcomes) {
    std::ostringstream out;
    for (const auto& outcome : outcomes) {
        out << outcome.filename << '\t' << outcome.nMessages << '\t' << outcome.nFailedMarsToGribTestCaseGenerations
            << '\t' << outcome.nFailedMarsToGribTestCaseWrites << '\t' << outcome.nCloseFailures << '\t'
            << outcome.nGenericProcessOneMessageFailures << '\t' << outcome.nGenericProcessUnitOfWorkFailures;
        appendCounts(out, outcome.openFile);
        appendCounts(out, outcome.readMessage);
        appendCounts(out, outcome.gribBasedFilter);
        appendCounts(out, outcome.gribToMars);
        appendCounts(out, outcome.marsOverrides);
        appendCounts(out, outcome.marsToMars);
        appendCounts(out, outcome.marsBasedFilter);
        appendCounts(out, outcome.marsToGrib);
        appendCounts(out, outcome.postEncodeValidation);
        appendCounts(out, outcome.grib2Fdb5);
        appendCounts(out, outcome.fileFlush);
        out << '\n';
    }
    return out.str();
}

std::vector<FileStageOutcomes> deserializeFileStageOutcomes(const std::string& payload) {
    std::vector<FileStageOutcomes> outcomes;
    std::istringstream in(payload);
    std::string line;
    while (std::getline(in, line)) {
        if (line.empty()) {
            continue;
        }

        auto fields = split(line, '\t');
        if (fields.size() != serializedFieldCount()) {
            throw eckit::BadValue("invalid serialized FileStageOutcomes record", Here());
        }

        FileStageOutcomes outcome;
        outcome.filename = std::move(fields[0]);
        outcome.nMessages = static_cast<std::size_t>(std::stoull(fields[1]));
        outcome.nFailedMarsToGribTestCaseGenerations = static_cast<std::size_t>(std::stoull(fields[2]));
        outcome.nFailedMarsToGribTestCaseWrites = static_cast<std::size_t>(std::stoull(fields[3]));
        outcome.nCloseFailures = static_cast<std::size_t>(std::stoull(fields[4]));
        outcome.nGenericProcessOneMessageFailures = static_cast<std::size_t>(std::stoull(fields[5]));
        outcome.nGenericProcessUnitOfWorkFailures = static_cast<std::size_t>(std::stoull(fields[6]));

        std::size_t pos = 7;
        parseCounts(fields, pos, outcome.openFile);
        parseCounts(fields, pos, outcome.readMessage);
        parseCounts(fields, pos, outcome.gribBasedFilter);
        parseCounts(fields, pos, outcome.gribToMars);
        parseCounts(fields, pos, outcome.marsOverrides);
        parseCounts(fields, pos, outcome.marsToMars);
        parseCounts(fields, pos, outcome.marsBasedFilter);
        parseCounts(fields, pos, outcome.marsToGrib);
        parseCounts(fields, pos, outcome.postEncodeValidation);
        parseCounts(fields, pos, outcome.grib2Fdb5);
        parseCounts(fields, pos, outcome.fileFlush);

        outcomes.push_back(std::move(outcome));
    }
    return outcomes;
}

}  // namespace multio::distGrib1ToGrib2::grib2grib

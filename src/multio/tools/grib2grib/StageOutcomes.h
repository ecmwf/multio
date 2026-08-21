/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/// @file
/// @brief Stage-level outcome model for the new isolated `grib2grib` pipeline.
///
/// This header defines the complete accounting vocabulary for the new pipeline:
/// - the ordered stages
/// - the per-stage outcome enums
/// - the per-stage counter containers
/// - the per-file and aggregated outcome records
/// - the public helper APIs for rendering, serialisation and summary derivation

#pragma once

#include <array>
#include <cstddef>
#include <cstdint>
#include <string>
#include <vector>

namespace multio::distGrib1ToGrib2::grib2grib {

/// @brief Ordered processing stages of the new GRIB-to-GRIB conversion pipeline.
///
/// The stage order is part of the processing contract and is documented here so
/// all later modules can share the same vocabulary.
enum class ProcessingStage : std::uint8_t
{
    OpenFile = 0,
    ReadMessage,
    GribBasedFilter,
    GribToMars,
    MarsToMars,
    MarsOverrides,
    MarsBasedFilter,
    MarsToGrib,
    PostEncodeValidation,
    Grib2Fdb5,
    FileFlush,
};

/// @brief Final per-file summary derived from stage counters.
enum class FileSummary : std::uint8_t
{
    Success = 0,
    Partial,
    Fail,
};

/// @brief File-level outcome codes for the `OpenFile` stage.
enum class OpenFileCode : std::uint8_t
{
    Valid = 0,
    OpenFailed,
    UnknownFailure,
};

/// @brief Message-level outcome codes for the `ReadMessage` stage.
enum class ReadMessageCode : std::uint8_t
{
    Valid = 0,
    ReadFailed,
    UnknownFailure,
};

/// @brief Message-level outcome codes for the `GribBasedFilter` stage.
///
/// This stage contains intentional early rejections and one generic technical
/// failure bucket for unexpected classification failures.
enum class GribBasedFilterCode : std::uint8_t
{
    Accepted = 0,
    RejectedDiscipline192,
    RejectedGrib1ByEditionPolicy,
    RejectedGrib2ByEditionPolicy,
    RejectedInvalidInputMessage,
    FailedGribBasedFilter,
};

/// @brief Message-level outcome codes for the `GribToMars` stage.
enum class GribToMarsCode : std::uint8_t
{
    Valid = 0,
    MapGribToMarsFailed,
    ValuesExtractionFailed,
    UnknownFailure,
};

/// @brief Message-level outcome codes for the `MarsToMars` stage.
enum class MarsToMarsCode : std::uint8_t
{
    Valid = 0,
    MappingsFailed,
    MergeMiscFailed,
    MarsDefaultsFailed,
    MarsValidationFailed,
    MiscDefaultsFailed,
    MiscValidationFailed,
    UnknownFailure,
};

/// @brief Message-level outcome codes for the `MarsOverrides` stage.
enum class MarsOverridesCode : std::uint8_t
{
    Valid = 0,
    OptionOverridesFailed,
    UnknownFailure,
};

/// @brief Message-level outcome codes for the `MarsBasedFilter` stage.
enum class MarsBasedFilterCode : std::uint8_t
{
    Accepted = 0,
    Rejected,
};

/// @brief Message-level outcome codes for the `MarsToGrib` stage.
enum class MarsToGribCode : std::uint8_t
{
    Valid = 0,
    EncodeFailed,
    TestCaseGenerationFailed,
    TestCaseWriteFailed,
    UnknownFailure,
};

/// @brief Message-level outcome codes for the `PostEncodeValidation` stage.
enum class PostEncodeValidationCode : std::uint8_t
{
    Valid = 0,
    InvalidEncodedMessage,
};

/// @brief Message-level outcome codes for the `Grib2Fdb5` stage.
enum class Grib2Fdb5Code : std::uint8_t
{
    Valid = 0,
    ArchiveFailed,
    UnknownFailure,
};

/// @brief File-level outcome codes for the `FileFlush` stage.
enum class FileFlushCode : std::uint8_t
{
    Valid = 0,
    FileFlushFailed,
    UnknownFailure,
};

/// @brief Small reusable counter container for one stage-specific enum.
/// @tparam Code Enum type describing the stage outcomes.
/// @tparam N Number of enum values stored by the counter.
///
/// Convention: index 0 is always the success or accepted state for the stage.
template <typename Code, std::size_t N>
struct StageCounters {
    std::array<std::size_t, N> counts{};

    static constexpr std::size_t size() { return N; }

    void bump(Code code) { ++counts[indexOf(code)]; }

    std::size_t get(Code code) const { return counts[indexOf(code)]; }

    void add(const StageCounters& other) {
        for (std::size_t i = 0; i < N; ++i) {
            counts[i] += other.counts[i];
        }
    }

private:
    static constexpr std::size_t indexOf(Code code) { return static_cast<std::size_t>(code); }
};

/// @brief Counter set for the `OpenFile` stage.
struct OpenFileCounters : StageCounters<OpenFileCode, 3> {};
/// @brief Counter set for the `ReadMessage` stage.
struct ReadMessageCounters : StageCounters<ReadMessageCode, 3> {};
/// @brief Counter set for the `GribBasedFilter` stage.
struct GribBasedFilterCounters : StageCounters<GribBasedFilterCode, 6> {};
/// @brief Counter set for the `GribToMars` stage.
struct GribToMarsCounters : StageCounters<GribToMarsCode, 4> {};
/// @brief Counter set for the `MarsOverrides` stage.
struct MarsOverridesCounters : StageCounters<MarsOverridesCode, 3> {};
/// @brief Counter set for the `MarsToMars` stage.
struct MarsToMarsCounters : StageCounters<MarsToMarsCode, 8> {};
/// @brief Counter set for the `MarsBasedFilter` stage.
struct MarsBasedFilterCounters : StageCounters<MarsBasedFilterCode, 3> {};
/// @brief Counter set for the `MarsToGrib` stage.
struct MarsToGribCounters : StageCounters<MarsToGribCode, 5> {};
/// @brief Counter set for the `PostEncodeValidation` stage.
struct PostEncodeValidationCounters : StageCounters<PostEncodeValidationCode, 2> {};
/// @brief Counter set for the `Grib2Fdb5` stage.
struct Grib2Fdb5Counters : StageCounters<Grib2Fdb5Code, 3> {};
/// @brief Counter set for the `FileFlush` stage.
struct FileFlushCounters : StageCounters<FileFlushCode, 3> {};

/// @brief Complete per-file stage accounting for the new pipeline.
struct FileStageOutcomes {
    std::string filename;
    std::size_t nMessages = 0;
    std::size_t nFailedMarsToGribTestCaseGenerations = 0;
    std::size_t nFailedMarsToGribTestCaseWrites = 0;
    std::size_t nCloseFailures = 0;
    std::size_t nGenericProcessOneMessageFailures = 0;
    std::size_t nGenericProcessUnitOfWorkFailures = 0;

    OpenFileCounters openFile;
    ReadMessageCounters readMessage;
    GribBasedFilterCounters gribBasedFilter;
    GribToMarsCounters gribToMars;
    MarsToMarsCounters marsToMars;
    MarsOverridesCounters marsOverrides;
    MarsBasedFilterCounters marsBasedFilter;
    MarsToGribCounters marsToGrib;
    PostEncodeValidationCounters postEncodeValidation;
    Grib2Fdb5Counters grib2Fdb5;
    FileFlushCounters fileFlush;

    void add(const FileStageOutcomes& other);
};

/// @brief Aggregated stage accounting across multiple files.
struct OutcomeAggregate {
    std::size_t nFiles = 0;
    std::size_t nMessages = 0;
    std::size_t nFailedMarsToGribTestCaseGenerations = 0;
    std::size_t nFailedMarsToGribTestCaseWrites = 0;
    std::size_t nCloseFailures = 0;
    std::size_t nGenericProcessOneMessageFailures = 0;
    std::size_t nGenericProcessUnitOfWorkFailures = 0;

    OpenFileCounters openFile;
    ReadMessageCounters readMessage;
    GribBasedFilterCounters gribBasedFilter;
    GribToMarsCounters gribToMars;
    MarsToMarsCounters marsToMars;
    MarsOverridesCounters marsOverrides;
    MarsBasedFilterCounters marsBasedFilter;
    MarsToGribCounters marsToGrib;
    PostEncodeValidationCounters postEncodeValidation;
    Grib2Fdb5Counters grib2Fdb5;
    FileFlushCounters fileFlush;
};

/// @name String rendering helpers
/// @{
const char* toString(ProcessingStage);
const char* toString(FileSummary);
const char* toString(OpenFileCode);
const char* toString(ReadMessageCode);
const char* toString(GribBasedFilterCode);
const char* toString(GribToMarsCode);
const char* toString(MarsOverridesCode);
const char* toString(MarsToMarsCode);
const char* toString(MarsBasedFilterCode);
const char* toString(MarsToGribCode);
const char* toString(PostEncodeValidationCode);
const char* toString(Grib2Fdb5Code);
const char* toString(FileFlushCode);
/// @}

/// @name JSON rendering helpers
/// @{
std::string toJson(const OpenFileCounters&);
std::string toJson(const ReadMessageCounters&);
std::string toJson(const GribBasedFilterCounters&);
std::string toJson(const GribToMarsCounters&);
std::string toJson(const MarsOverridesCounters&);
std::string toJson(const MarsToMarsCounters&);
std::string toJson(const MarsBasedFilterCounters&);
std::string toJson(const MarsToGribCounters&);
std::string toJson(const PostEncodeValidationCounters&);
std::string toJson(const Grib2Fdb5Counters&);
std::string toJson(const FileFlushCounters&);
std::string toJson(const FileStageOutcomes&);
std::string toJson(const std::vector<FileStageOutcomes>&);
std::string toJson(const OutcomeAggregate&);
/// @}

/// @brief Derive the per-file summary from the recorded stage counters.
/// @param outcomes Per-file stage outcomes.
/// @return `Success`, `Partial`, or `Fail` according to the current summary rules.
FileSummary deriveSummary(const FileStageOutcomes&);

/// @brief Add one file report into a global aggregate.
/// @param aggregate Aggregate to update.
/// @param outcomes Per-file stage outcomes to fold into the aggregate.
void accumulate(OutcomeAggregate&, const FileStageOutcomes&);

/// @brief Build a compact one-line text representation of one file outcome.
/// @param outcomes Per-file stage outcomes.
/// @return Human-readable single-line representation.
std::string formatOutcomeLine(const FileStageOutcomes&);

/// @brief Build a compact one-line text representation of an aggregate outcome.
/// @param aggregate Aggregate stage outcomes.
/// @return Human-readable single-line representation.
std::string formatAggregateLine(const OutcomeAggregate&);

/// @brief Serialise per-file stage outcomes into a stable text payload.
/// @param outcomes Sequence of per-file outcomes.
/// @return Tab-separated payload suitable for MPI gathering.
std::string serializeFileStageOutcomes(const std::vector<FileStageOutcomes>&);

/// @brief Reconstruct per-file stage outcomes from a serialised payload.
/// @param payload Serialised text payload.
/// @return Decoded sequence of per-file outcomes.
/// @throw eckit exception If the payload shape is invalid.
std::vector<FileStageOutcomes> deserializeFileStageOutcomes(const std::string&);

}  // namespace multio::distGrib1ToGrib2::grib2grib

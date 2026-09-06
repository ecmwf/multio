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
/// @brief Local sink initialization utilities for the distributed `grib2grib` tool.

#pragma once

#include <array>
#include <cstdint>
#include <cstdio>
#include <memory>
#include <optional>
#include <string>
#include <vector>

#include "eckit/config/LocalConfiguration.h"

#include "multio/sink/DataSink.h"
#include "multio/tools/grib2grib/StageOutcomes.h"

namespace metkit::codes {
class CodesHandle;
}

namespace multio::distGrib1ToGrib2::grib2grib {

/// @brief Default rank-local file output path for the main accepted-output sink.
std::string rankOutputPath(const std::string& outputDirectory, int rank);

/// @brief Resolve the effective main sink configuration for one rank.
///
/// If the top-level `sink` block is absent, a file sink is synthesized. For a
/// file sink with no explicit `path`, the rank-local default path is injected.
eckit::LocalConfiguration sinkConfigurationForRank(const eckit::LocalConfiguration& options,
                                                   const std::string& outputDirectory, int rank);

/// @brief Build the main accepted-output sink for one rank, or return `nullptr` when disabled.
std::unique_ptr<multio::sink::DataSink> buildSink(const eckit::LocalConfiguration& options,
                                                   const std::string& outputDirectory, int rank);

/// @brief Append-only file sink for rank-local mars2grib testcases.
class TestCaseFileSink {
public:
    TestCaseFileSink(const std::string& directory, std::int64_t mpiRank);
    ~TestCaseFileSink();

    TestCaseFileSink(const TestCaseFileSink&) = delete;
    TestCaseFileSink& operator=(const TestCaseFileSink&) = delete;
    TestCaseFileSink(TestCaseFileSink&&) = delete;
    TestCaseFileSink& operator=(TestCaseFileSink&&) = delete;

    /// @brief Append one testcase record.
    /// @throw eckit::WriteError On a short write.
    void write(const std::string& testCase);

    void flush();

private:
    std::FILE* file_ = nullptr;
};

/// @brief Rank-local sinks used by the distributed `grib2grib` pipeline.
///
/// Owns:
/// - the main accepted-output sink;
/// - optional per-stage debug sinks, configured under `debug-sinks` using the
///   exact same grammar as the top-level `sink`;
/// - the optional testcase text sink used by `MarsToGrib`.
///
/// Debug sinks are best-effort observational side outputs. Missing stage entries
/// are treated as no-ops. Write failures are caught internally and never change
/// the main pipeline classification result.
class Grib2GribSinks {
public:
    Grib2GribSinks(const eckit::LocalConfiguration& options, const std::string& outputDirectory, int rank,
                   bool generateTestcases, const std::optional<std::string>& testcasesDirectory);
    ~Grib2GribSinks();

    /// @brief Main encoded-GRIB2 output sink, or `nullptr` when disabled.
    multio::sink::DataSink* mainDataSink();

    /// @brief Testcase file sink, or `nullptr` when testcase generation is disabled.
    TestCaseFileSink* testCaseSink();

    /// @brief Best-effort side sink for stage-specific rejected inputs.
    void debugStageInput(ProcessingStage stage, const metkit::codes::CodesHandle& inputHandle) noexcept;

    /// @brief Flush the main data sink(s) and the testcase sink.
    void flush();

private:
    static constexpr std::size_t processingStageCount = static_cast<std::size_t>(ProcessingStage::FileFlush) + 1;

    std::vector<std::unique_ptr<multio::sink::DataSink>> sinks_;
    std::array<std::unique_ptr<multio::sink::DataSink>, processingStageCount> debugSinks_{};
    std::unique_ptr<TestCaseFileSink> testCaseSink_;
};

}  // namespace multio::distGrib1ToGrib2::grib2grib

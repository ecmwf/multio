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

#include <cstdint>
#include <cstdio>
#include <memory>
#include <optional>
#include <string>
#include <vector>

#include "eckit/config/LocalConfiguration.h"

namespace multio::sink {
class DataSink;
}

namespace multio::distGrib1ToGrib2::grib2grib {

std::string rankOutputPath(const std::string& outputDirectory, int rank);

eckit::LocalConfiguration sinkConfigurationForRank(const eckit::LocalConfiguration& options,
                                                   const std::string& outputDirectory, int rank);

std::unique_ptr<multio::sink::DataSink> buildSink(const eckit::LocalConfiguration& options,
                                                  const std::string& outputDirectory, int rank);

/// @brief Append-only text file sink for mars2grib testcases.
///
/// RAII wrapper around a raw file handle, replacing the manual `std::FILE*`
/// lifecycle that previously lived in the `MarsToGrib` stage context.
class TestCaseFileSink {
public:
    TestCaseFileSink(const std::string& directory, std::int64_t mpiRank);
    ~TestCaseFileSink();

    TestCaseFileSink(const TestCaseFileSink&) = delete;
    TestCaseFileSink& operator=(const TestCaseFileSink&) = delete;
    TestCaseFileSink(TestCaseFileSink&&) = delete;
    TestCaseFileSink& operator=(TestCaseFileSink&&) = delete;

    /// @brief Append one testcase record to the file.
    /// @throw eckit::WriteError On a short write.
    void write(const std::string& testCase);

    void flush();

private:
    std::FILE* file_ = nullptr;
};

/// @brief Rank-local sinks used by the distributed `grib2grib` pipeline.
///
/// Owns the encoded-GRIB2 data sink(s) and, when testcase generation is
/// enabled, the testcase file sink. Initialised once per rank alongside the
/// data sink and passed down the whole processing chain.
class Grib2GribSinks {
public:
    Grib2GribSinks(const eckit::LocalConfiguration& options, const std::string& outputDirectory, int rank,
                   bool generateTestcases, const std::optional<std::string>& testcasesDirectory);
    ~Grib2GribSinks();

    /// @brief Main encoded-GRIB2 output sink (internally `sinks_[0]`).
    multio::sink::DataSink& mainDataSink();

    /// @brief Testcase file sink, or `nullptr` when testcase generation is disabled.
    TestCaseFileSink* testCaseSink();

    /// @brief Flush the main data sink(s) and the testcase sink.
    void flush();

private:
    std::vector<std::unique_ptr<multio::sink::DataSink>> sinks_;
    std::unique_ptr<TestCaseFileSink> testCaseSink_;
};

}  // namespace multio::distGrib1ToGrib2::grib2grib

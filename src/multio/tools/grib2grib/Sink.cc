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

#include "multio/tools/grib2grib/Sink.h"

#include <iostream>

#include "eckit/exception/Exceptions.h"
#include "eckit/filesystem/PathName.h"

#include "multio/config/ComponentConfiguration.h"
#include "multio/config/MultioConfiguration.h"
#include "multio/sink/DataSink.h"
#include "multio/tools/grib2grib/Utils.h"

namespace multio::distGrib1ToGrib2::grib2grib {

namespace {

std::string testCaseFilePath(const std::string& directory, std::int64_t mpiRank) {
    eckit::PathName dir{directory};
    return (dir / ("mars2grib-testcases." + std::to_string(mpiRank) + ".json")).asString();
}

}  // namespace

std::string rankOutputPath(const std::string& outputDirectory, int rank) {
    return outputDirectory + "/output/rank" + std::to_string(rank) + ".grib2";
}

eckit::LocalConfiguration sinkConfigurationForRank(const eckit::LocalConfiguration& options,
                                                   const std::string& outputDirectory, int rank) {
    eckit::LocalConfiguration sinkConf;
    if (options.has("sink")) {
        sinkConf = options.getSubConfiguration("sink");
    }
    else {
        sinkConf.set("type", std::string{"file"});
    }

    if (!sinkConf.has("type")) {
        sinkConf.set("type", std::string{"file"});
    }
    if (sinkConf.getString("type") == "file" && !sinkConf.has("path")) {
        sinkConf.set("path", rankOutputPath(outputDirectory, rank));
    }

    return sinkConf;
}

std::unique_ptr<multio::sink::DataSink> buildSink(const eckit::LocalConfiguration& options,
                                                  const std::string& outputDirectory, int rank) {
    const auto sinkConf = sinkConfigurationForRank(options, outputDirectory, rank);
    config::MultioConfiguration multioConf(eckit::LocalConfiguration{}, config::LocalPeerTag::Client);
    config::ComponentConfiguration componentConf(sinkConf, multioConf);
    if (sinkConf.getString("type") == "file" && sinkConf.has("path")) {
        eckit::PathName{sinkConf.getString("path")}.dirName().mkdir();
    }
    std::cerr << timestampString() << "rank " << rank << " building sink of type: " << sinkConf.getString("type")
              << std::endl;
    return sink::DataSinkFactory::instance().build(sinkConf.getString("type"), componentConf);
}

TestCaseFileSink::TestCaseFileSink(const std::string& directory, std::int64_t mpiRank) {
    const auto filePath = testCaseFilePath(directory, mpiRank);
    file_ = std::fopen(filePath.c_str(), "a");
    if (file_ == nullptr) {
        throw eckit::CantOpenFile(filePath, Here());
    }
}

TestCaseFileSink::~TestCaseFileSink() {
    if (file_ != nullptr) {
        std::fclose(file_);
        file_ = nullptr;
    }
}

void TestCaseFileSink::write(const std::string& testCase) {
    const auto written = std::fwrite(testCase.data(), 1, testCase.size(), file_);
    if (written != testCase.size()) {
        throw eckit::WriteError("Failed to append MarsToGrib testcase output", Here());
    }
}

void TestCaseFileSink::flush() {
    if (file_ != nullptr) {
        std::fflush(file_);
    }
}

Grib2GribSinks::Grib2GribSinks(const eckit::LocalConfiguration& options, const std::string& outputDirectory, int rank,
                               bool generateTestcases, const std::optional<std::string>& testcasesDirectory) {
    sinks_.push_back(buildSink(options, outputDirectory, rank));

    if (generateTestcases) {
        if (!testcasesDirectory) {
            throw eckit::BadValue("mars-to-grib option 'testcases-dir' is required when testcases are enabled", Here());
        }
        testCaseSink_ = std::make_unique<TestCaseFileSink>(*testcasesDirectory, rank);
    }
}

Grib2GribSinks::~Grib2GribSinks() = default;

multio::sink::DataSink& Grib2GribSinks::mainDataSink() {
    return *sinks_[0];
}

TestCaseFileSink* Grib2GribSinks::testCaseSink() {
    return testCaseSink_.get();
}

void Grib2GribSinks::flush() {
    for (const auto& sink : sinks_) {
        sink->flush();
    }
    if (testCaseSink_ != nullptr) {
        testCaseSink_->flush();
    }
}

}  // namespace multio::distGrib1ToGrib2::grib2grib

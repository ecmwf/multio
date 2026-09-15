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
///
/// This file owns all rank-local sink construction for the new pipeline:
/// - the main accepted-output sink built from top-level `sink`
/// - one optional debug sink built from top-level `debug-sink`
/// - the optional append-only testcase text sink used by `MarsToGrib`
///
/// Debug sinks deliberately reuse the exact same sink grammar as the main sink.
/// Diagnostic messages are relabelled by stage before they are written.

#include "multio/tools/grib2grib/Sink.h"

#include <iostream>
#include <utility>

#include "eckit/exception/Exceptions.h"
#include "eckit/filesystem/PathName.h"
#include "eckit/runtime/Main.h"

#include "metkit/codes/api/CodesAPI.h"

#include "multio/config/ComponentConfiguration.h"
#include "multio/config/MultioConfiguration.h"
#include "multio/sink/DataSink.h"
#include "multio/tools/grib2grib/CodesHandleToEckitMessage.h"
#include "multio/tools/grib2grib/Utils.h"

namespace multio::distGrib1ToGrib2::grib2grib {

namespace {

std::string debugRankOutputPath(const std::string& outputDirectory, int rank) {
    return outputDirectory + "/debug/rank" + std::to_string(rank) + ".grib";
}

// Diagnostic expver uses 3SRR: S is the one-based message stage and RR is
// 01-49 for rejection, 51-98 for known failure, or 99 for unknown failure.
const char* debugExpver(ProcessingStage stage, std::uint8_t outcome) {
    switch (stage) {
        case ProcessingStage::GribBasedFilter:
            switch (static_cast<GribBasedFilterCode>(outcome)) {
                case GribBasedFilterCode::RejectedDiscipline192:
                    return "3101";
                case GribBasedFilterCode::RejectedGrib1ByEditionPolicy:
                    return "3102";
                case GribBasedFilterCode::RejectedGrib2ByEditionPolicy:
                    return "3103";
                case GribBasedFilterCode::RejectedInvalidInputMessage:
                    return "3104";
                case GribBasedFilterCode::FailedGribBasedFilter:
                    return "3151";
                default:
                    return nullptr;
            }
        case ProcessingStage::GribToMars:
            switch (static_cast<GribToMarsCode>(outcome)) {
                case GribToMarsCode::MapGribToMarsFailed:
                    return "3251";
                case GribToMarsCode::ValuesExtractionFailed:
                    return "3252";
                case GribToMarsCode::UnknownFailure:
                    return "3299";
                default:
                    return nullptr;
            }
        case ProcessingStage::MarsToMars:
            switch (static_cast<MarsToMarsCode>(outcome)) {
                case MarsToMarsCode::MappingsFailed:
                    return "3351";
                case MarsToMarsCode::MergeMiscFailed:
                    return "3352";
                case MarsToMarsCode::MarsDefaultsFailed:
                    return "3353";
                case MarsToMarsCode::MarsValidationFailed:
                    return "3354";
                case MarsToMarsCode::MiscDefaultsFailed:
                    return "3355";
                case MarsToMarsCode::MiscValidationFailed:
                    return "3356";
                case MarsToMarsCode::UnknownFailure:
                    return "3399";
                default:
                    return nullptr;
            }
        case ProcessingStage::MarsOverrides:
            switch (static_cast<MarsOverridesCode>(outcome)) {
                case MarsOverridesCode::OptionOverridesFailed:
                    return "3451";
                case MarsOverridesCode::UnknownFailure:
                    return "3499";
                default:
                    return nullptr;
            }
        case ProcessingStage::MarsBasedFilter:
            return static_cast<MarsBasedFilterCode>(outcome) == MarsBasedFilterCode::Rejected ? "3501" : nullptr;
        case ProcessingStage::MarsToGrib:
            switch (static_cast<MarsToGribCode>(outcome)) {
                case MarsToGribCode::EncodeFailed:
                    return "3651";
                case MarsToGribCode::TestCaseGenerationFailed:
                    return "3652";
                case MarsToGribCode::TestCaseWriteFailed:
                    return "3653";
                case MarsToGribCode::UnknownFailure:
                    return "3699";
                default:
                    return nullptr;
            }
        case ProcessingStage::PostEncodeValidation:
            return static_cast<PostEncodeValidationCode>(outcome) == PostEncodeValidationCode::InvalidEncodedMessage
                       ? "3751"
                       : nullptr;
        case ProcessingStage::Grib2Fdb5:
            switch (static_cast<Grib2Fdb5Code>(outcome)) {
                case Grib2Fdb5Code::ArchiveFailed:
                    return "3851";
                case Grib2Fdb5Code::UnknownFailure:
                    return "3899";
                default:
                    return nullptr;
            }
        default:
            return nullptr;
    }
}

void stripDebugData(metkit::codes::CodesHandle&) {
    // Reserved for metadata-only diagnostic messages.
}

eckit::LocalConfiguration sinkConfigurationWithDefaults(eckit::LocalConfiguration sinkConf,
                                                        const std::string& defaultPath) {
    if (!sinkConf.has("type")) {
        sinkConf.set("type", std::string{"file"});
    }

    if (sinkConf.getString("type") == "file" && !sinkConf.has("path")) {
        sinkConf.set("path", defaultPath);
    }

    return sinkConf;
}

std::unique_ptr<multio::sink::DataSink> buildSinkFromConfiguration(eckit::LocalConfiguration sinkConf, int rank) {
    config::MultioConfiguration multioConf(eckit::LocalConfiguration{}, config::LocalPeerTag::Client);
    config::ComponentConfiguration componentConf(sinkConf, multioConf);
    if (sinkConf.getString("type") == "file" && sinkConf.has("path")) {
        eckit::PathName{sinkConf.getString("path")}.dirName().mkdir();
    }
    std::cerr << timestampString() << "rank " << rank << " building sink of type: " << sinkConf.getString("type")
              << std::endl;
    return sink::DataSinkFactory::instance().build(sinkConf.getString("type"), componentConf);
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

    return sinkConfigurationWithDefaults(std::move(sinkConf), rankOutputPath(outputDirectory, rank));
}

std::unique_ptr<multio::sink::DataSink> buildSink(const eckit::LocalConfiguration& options,
                                                  const std::string& outputDirectory, int rank) {
    if (options.has("sink")) {
        const auto sinkConf = options.getSubConfiguration("sink");
        if (sinkConf.has("enabled") && !sinkConf.getBool("enabled")) {
            return nullptr;
        }
    }
    return buildSinkFromConfiguration(sinkConfigurationForRank(options, outputDirectory, rank), rank);
}

TestCaseFileSink::TestCaseFileSink(const std::string& directory, std::int64_t mpiRank) {
    const eckit::PathName outputDirectory = eckit::PathName{directory} / eckit::Main::hostname();
    outputDirectory.mkdir();
    const eckit::PathName filePath = outputDirectory / ("file." + std::to_string(mpiRank) + ".jsonl");
    file_ = std::fopen(filePath.asString().c_str(), "a");
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
    if (auto sink = buildSink(options, outputDirectory, rank)) {
        sinks_.push_back(std::move(sink));
    }

    if (options.has("debug-sink")) {
        if (!options.isSubConfiguration("debug-sink")) {
            throw eckit::BadValue("debug-sink must be a configuration section", Here());
        }

        eckit::LocalConfiguration debugSinkConf = options.getSubConfiguration("debug-sink");
        stripDebugData_ = debugSinkConf.has("strip-data") ? debugSinkConf.getBool("strip-data") : false;
        if (!debugSinkConf.has("enabled") || debugSinkConf.getBool("enabled")) {
            try {
                debugSinkConf = sinkConfigurationWithDefaults(std::move(debugSinkConf),
                                                              debugRankOutputPath(outputDirectory, rank));
                debugSink_ = buildSinkFromConfiguration(std::move(debugSinkConf), rank);
            }
            catch (...) {
                // Debug output must never prevent the conversion from starting.
            }
        }
    }

    if (generateTestcases) {
        if (!testcasesDirectory) {
            throw eckit::BadValue("mars-to-grib option 'testcases-dir' is required when testcases are enabled", Here());
        }
        testCaseSink_ = std::make_unique<TestCaseFileSink>(*testcasesDirectory, rank);
    }
}

Grib2GribSinks::~Grib2GribSinks() = default;

multio::sink::DataSink* Grib2GribSinks::mainDataSink() {
    return sinks_.empty() ? nullptr : sinks_[0].get();
}

TestCaseFileSink* Grib2GribSinks::testCaseSink() {
    return testCaseSink_.get();
}

void Grib2GribSinks::debugStageInputCode(ProcessingStage stage, std::uint8_t outcome,
                                         const metkit::codes::CodesHandle& inputHandle) noexcept {
    const char* expver = debugExpver(stage, outcome);
    if (expver == nullptr) {
        return;
    }

    writeDebugInput(inputHandle, expver);
}

void Grib2GribSinks::debugSuccessfulInput(const metkit::codes::CodesHandle& inputHandle) noexcept {
    writeDebugInput(inputHandle, "2251");
}

void Grib2GribSinks::writeDebugInput(const metkit::codes::CodesHandle& inputHandle,
                                     const std::string& expver) noexcept {
    if (!debugSink_) {
        return;
    }

    try {
        auto diagnosticInput = inputHandle.clone();
        diagnosticInput->set("expver", expver);
        if (stripDebugData_) {
            stripDebugData(*diagnosticInput);
        }
        debugSink_->write(to_eckit_message(*diagnosticInput));
    }
    catch (...) {
        // Debug output must never affect processing or classification.
    }
}

void Grib2GribSinks::flush() {
    for (const auto& sink : sinks_) {
        sink->flush();
    }
    if (debugSink_) {
        try {
            debugSink_->flush();
        }
        catch (...) {
            // Debug output must never affect processing or classification.
        }
    }
    if (testCaseSink_ != nullptr) {
        testCaseSink_->flush();
    }
}

}  // namespace multio::distGrib1ToGrib2::grib2grib

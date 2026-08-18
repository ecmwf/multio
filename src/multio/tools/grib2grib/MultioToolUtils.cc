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
/// @brief Tool-level orchestration helpers for the distributed `grib2grib` tool.

#include "multio/tools/grib2grib/MultioToolUtils.h"

#include <fstream>
#include <iomanip>
#include <iostream>

#include "eckit/exception/Exceptions.h"

#include "multio/tools/grib2grib/MpiUtils.h"
#include "multio/tools/grib2grib/OptionsUtils.h"
#include "multio/tools/grib2grib/ProcessRankOwnedUnitsOfWork.h"
#include "multio/tools/grib2grib/Sink.h"
#include "multio/tools/grib2grib/Summary.h"

namespace multio::grib2grib::utils {

namespace {

std::vector<std::string> readFileList(const std::string& fileListPath) {
    std::ifstream in(fileListPath);
    if (!in) {
        throw eckit::CantOpenFile(fileListPath, Here());
    }

    std::vector<std::string> filenames;
    std::string line;
    while (std::getline(in, line)) {
        if (!line.empty() && line.back() == '\r') {
            line.pop_back();
        }
        if (!line.empty()) {
            filenames.push_back(line);
        }
    }

    if (!in.eof()) {
        throw eckit::ReadError("error while reading file list: " + fileListPath, Here());
    }

    return filenames;
}

}  // namespace

eckit::LocalConfiguration loadAndBroadcastOptionsAsConfiguration(const std::string& optionsFile,
                                                                 const eckit::mpi::Comm& comm) {
    return multio::distGrib1ToGrib2::grib2grib::loadAndBroadcastOptionsAsConfiguration(optionsFile, comm);
}

GlobalContext buildGlobalContext(const eckit::LocalConfiguration& rawOptions) {
    return multio::distGrib1ToGrib2::grib2grib::parseGlobalContext(rawOptions);
}

std::unique_ptr<Grib2GribSinks> buildRankLocalWriter(const eckit::LocalConfiguration& rawOptions,
                                                     const GlobalContext& context, const std::string& outputDirectory,
                                                     const eckit::mpi::Comm& comm) {
    return std::make_unique<Grib2GribSinks>(rawOptions, outputDirectory, static_cast<int>(comm.rank()),
                                            context.marsToGrib.generateTestcases, context.marsToGrib.testcasesDir);
}

std::vector<WorkUnit> distributeWork(const std::string& fileList, long averageWorkUnitsPerRank,
                                     const eckit::mpi::Comm& comm) {
    std::vector<WorkBucket> rootBuckets;
    if (comm.rank() == 0) {
        rootBuckets = multio::distGrib1ToGrib2::grib2grib::createBuckets(
            readFileList(fileList), comm.size(), static_cast<std::size_t>(averageWorkUnitsPerRank));
    }

    const auto rankBucket = multio::distGrib1ToGrib2::grib2grib::distributeRankOwnedBucket(
        comm.rank() == 0 ? &rootBuckets : nullptr, comm);
    return rankBucket.workUnits;
}

std::vector<FileStageOutcomes> processWorkUnits(const std::vector<WorkUnit>& workUnits, const GlobalContext& context,
                                                Grib2GribSinks& writer) {
    return multio::distGrib1ToGrib2::grib2grib::processRankOwnedUnitsOfWork(workUnits, context, writer);
}

std::vector<FileStageOutcomes> gatherWorkUnitOutcome(const std::vector<FileStageOutcomes>& localOutcomes,
                                                     const eckit::mpi::Comm& comm) {
    return multio::distGrib1ToGrib2::grib2grib::gatherOutcomes(localOutcomes, comm);
}

std::vector<FileStageOutcomes> summarizeWorkUnitOutcomePerFile(
    const std::vector<FileStageOutcomes>& workUnitOutcomeGlobal) {
    return multio::distGrib1ToGrib2::grib2grib::createPerFileOutcomes(workUnitOutcomeGlobal);
}

SummaryType createSummary(const std::vector<FileStageOutcomes>& workUnitOutcomePerFile) {
    return workUnitOutcomePerFile;
}

AggregateSummary buildAggregateSummary(const SummaryType& summary) {
    return multio::distGrib1ToGrib2::grib2grib::summarizeByFileSummary(summary);
}

void writeSummary(const SummaryType& summary, const std::string& outputDirectory) {
    const std::string summaryLogPath = outputDirectory + "/Summary.log";
    const std::string summaryJsonPath = outputDirectory + "/Summary.json";

    std::ofstream summaryLog(summaryLogPath);
    if (!summaryLog) {
        throw eckit::CantOpenFile(summaryLogPath, Here());
    }
    for (const auto& outcome : summary) {
        summaryLog << formatOutcomeLine(outcome);
    }
    if (!summaryLog.good()) {
        throw eckit::WriteError("error while writing summary file: " + summaryLogPath, Here());
    }

    std::ofstream summaryJson(summaryJsonPath);
    if (!summaryJson) {
        throw eckit::CantOpenFile(summaryJsonPath, Here());
    }
    summaryJson << toJson(summary);
    if (!summaryJson.good()) {
        throw eckit::WriteError("error while writing summary file: " + summaryJsonPath, Here());
    }
}

void printAggregateSummary(const AggregateSummary& summary) {
    const std::size_t totalFiles = summary.success.nFiles + summary.partial.nFiles + summary.fail.nFiles;

    const auto printBucket = [totalFiles](const char* label, const multio::grib2grib::utils::AggregateSummaryBucket& bucket) {
        const double percent = totalFiles == 0 ? 0.0 : 100.0 * static_cast<double>(bucket.nFiles) / totalFiles;
        std::cout << label << ',' << bucket.nFiles << ',' << bucket.nMessages << ',' << std::fixed
                  << std::setprecision(2) << percent << std::endl;
    };

    printBucket("SUCCESS", summary.success);
    printBucket("PARTIAL", summary.partial);
    printBucket("FAIL", summary.fail);
}

}  // namespace multio::grib2grib::utils

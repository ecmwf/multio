/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#include <algorithm>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <string>
#include <vector>


#include "metkit/codes/api/CodesAPI.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/filesystem/PathName.h"
#include "eckit/log/Log.h"
#include "eckit/option/CmdArgs.h"
#include "eckit/option/SimpleOption.h"


#include <eccodes.h>

#include "multio/tools/MultioTool.h"
#include "multio/tools/grib2grib/UnitOfWork.h"
#include "multio/tools/grib2grib/Utils.h"
#include "multio/tools/grib2grib/WorkUnitLoadBalancer.h"

namespace multio::grib2grib {

namespace detail {

using WorkBucket = multio::distGrib1ToGrib2::grib2grib::WorkBucket;
using WorkUnit = multio::distGrib1ToGrib2::grib2grib::WorkUnit;
using multio::distGrib1ToGrib2::grib2grib::createBuckets;
using multio::distGrib1ToGrib2::grib2grib::timestampString;

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

std::uint64_t workUnitSize(const WorkUnit& workUnit) {
    return static_cast<std::uint64_t>(workUnit.endOffset - workUnit.startOffset);
}

void writeWorkUnitsCsv(const std::vector<WorkBucket>& buckets, const std::string& outputDirectory) {
    const std::string path = outputDirectory + "/work-units.csv";
    std::ofstream out(path);
    if (!out) {
        throw eckit::CantOpenFile(path, Here());
    }

    out << "MPI_rank,filename,offsetStart,offsetEnd,size\n";
    for (std::size_t rank = 0; rank < buckets.size(); ++rank) {
        for (const auto& workUnit : buckets[rank].workUnits) {
            out << rank << ',' << workUnit.filename << ',' << workUnit.startOffset << ',' << workUnit.endOffset << ','
                << workUnitSize(workUnit) << '\n';
        }
    }
}

void writeDistributionStats(const std::vector<WorkBucket>& buckets, const std::string& outputDirectory,
                            std::size_t nFiles, std::size_t averageWorkUnitsPerRank) {
    const std::string path = outputDirectory + "/distribution-stats.csv";
    std::ofstream out(path);
    if (!out) {
        throw eckit::CantOpenFile(path, Here());
    }

    std::uint64_t totalWeight = 0;
    std::uint64_t totalWorkUnits = 0;
    std::uint64_t minWeight = buckets.empty() ? 0 : buckets.front().totalWeightBytes;
    std::uint64_t maxWeight = buckets.empty() ? 0 : buckets.front().totalWeightBytes;

    for (const auto& bucket : buckets) {
        totalWeight += bucket.totalWeightBytes;
        totalWorkUnits += bucket.workUnits.size();
        minWeight = std::min(minWeight, bucket.totalWeightBytes);
        maxWeight = std::max(maxWeight, bucket.totalWeightBytes);
    }

    const double idealWeight = buckets.empty() ? 0.0 : static_cast<double>(totalWeight) / static_cast<double>(buckets.size());
    const std::uint64_t maxMinusMin = maxWeight - minWeight;
    const double imbalancePercent = idealWeight > 0.0 ? 100.0 * static_cast<double>(maxMinusMin) / idealWeight : 0.0;

    out << "metric,value\n";
    out << "n_workers," << buckets.size() << '\n';
    out << "n_files," << nFiles << '\n';
    out << "n_work_units," << totalWorkUnits << '\n';
    out << "total_weight_bytes," << totalWeight << '\n';
    out << "average_work_units_per_rank," << averageWorkUnitsPerRank << '\n';
    out << "ideal_bucket_weight_bytes," << idealWeight << '\n';
    out << "min_bucket_weight_bytes," << minWeight << '\n';
    out << "max_bucket_weight_bytes," << maxWeight << '\n';
    out << "max_minus_min_bytes," << maxMinusMin << '\n';
    out << "imbalance_percent," << imbalancePercent << '\n';
}

void print(long rank, long workUnitIndex, const metkit::codes::CodesHandle& handle) {
    const long paramId = handle.getLong("paramId");
    const long channel = handle.getLong("channel");
    const long offset = handle.getLong("offset");
    const long count = handle.getLong("count");
    const long totalLength = handle.getLong("totalLength");
    const long isMessageValid = handle.getLong("isMessageValid");

    std::cout << std::setw(12) << rank << std::setw(12) << workUnitIndex << std::setw(12) << paramId
              << std::setw(12) << channel << std::setw(12) << offset << std::setw(12) << count
              << std::setw(12) << totalLength << std::setw(12) << isMessageValid << std::endl;
}

void scanWorkUnitMessages(long rank, long workUnitIndex, const WorkUnit& workUnit) {
    std::cout << "Scanning work unit messages for rank " << rank << ", work unit index " << workUnitIndex
              << ", file '" << workUnit.filename << "' in [" << workUnit.startOffset << ", " << workUnit.endOffset
              << ")" << std::endl;
    multio::distGrib1ToGrib2::grib2grib::UnitOfWork unitOfWork{workUnit};
    unitOfWork.open();
    while (unitOfWork.newMessageAvailable()) {
        const auto message = unitOfWork.nextMessage();
        if (!message) {
            break;
        }
        print(rank, workUnitIndex, *message);
    }
    unitOfWork.close();
}

void scanBucketMessages(const std::vector<WorkBucket>& buckets) {
    long workUnitIndex = 0;
    for (std::size_t rank = 0; rank < buckets.size(); ++rank) {
        const auto& bucket = buckets[rank];
        for (const auto& workUnit : bucket.workUnits) {
            scanWorkUnitMessages(static_cast<long>(rank), workUnitIndex, workUnit);
            ++workUnitIndex;
        }
    }
}

}  // namespace detail

class Grib2GribDataDistributionTest final : public multio::MultioTool {
public:
    Grib2GribDataDistributionTest(int argc, char** argv) : multio::MultioTool(argc, argv) {
        options_.push_back(new eckit::option::SimpleOption<std::string>("file-list", "Path to file list"));
        options_.push_back(
            new eckit::option::SimpleOption<std::string>("output-directory", "Path to output directory"));
        options_.push_back(new eckit::option::SimpleOption<long>("n-workers", "Number of synthetic worker buckets"));
        options_.push_back(new eckit::option::SimpleOption<long>("average-work-units-per-rank",
                                                                 "Average number of work units per synthetic rank"));
        options_.push_back(new eckit::option::SimpleOption<bool>("scan-work-unit-messages",
                                                                 "Scan all generated work units and iterate messages"));
    }

private:
    void usage(const std::string& tool) const override {
        eckit::Log::info() << "\nUsage: " << tool
                           << " --file-list <file-list.txt> --output-directory <path> --n-workers <N>"
                              " --average-work-units-per-rank <N> [--scan-work-unit-messages]\n";
    }

    void init(const eckit::option::CmdArgs& args) override {
        args.get("file-list", fileList_);
        args.get("output-directory", outputDirectory_);
        args.get("n-workers", nWorkers_);
        args.get("average-work-units-per-rank", averageWorkUnitsPerRank_);
        args.get("scan-work-unit-messages", scanWorkUnitMessages_);

        if (fileList_.empty()) {
            throw eckit::UserError("Missing required option --file-list", Here());
        }
        if (outputDirectory_.empty()) {
            throw eckit::UserError("Missing required option --output-directory", Here());
        }
        if (nWorkers_ <= 0) {
            throw eckit::UserError("--n-workers must be > 0", Here());
        }
        if (averageWorkUnitsPerRank_ <= 0) {
            throw eckit::UserError("--average-work-units-per-rank must be > 0", Here());
        }
    }

    void execute(const eckit::option::CmdArgs&) override {
        const auto filenames = detail::readFileList(fileList_);
        eckit::PathName{outputDirectory_}.mkdir();

        std::cerr << detail::timestampString() << "computing synthetic data distribution for " << filenames.size()
                  << " files" << std::endl;

        const auto buckets = detail::createBuckets(filenames, static_cast<std::size_t>(nWorkers_),
                                                   static_cast<std::size_t>(averageWorkUnitsPerRank_));

        if (scanWorkUnitMessages_) {
            detail::scanBucketMessages(buckets);
        }

        detail::writeWorkUnitsCsv(buckets, outputDirectory_);
        detail::writeDistributionStats(buckets, outputDirectory_, filenames.size(),
                                       static_cast<std::size_t>(averageWorkUnitsPerRank_));

        std::cerr << detail::timestampString() << "distribution test outputs written to '" << outputDirectory_ << "'"
                  << std::endl;
    }

    void finish(const eckit::option::CmdArgs&) override {}

    int numberOfPositionalArguments() const override { return 0; }
    int minimumPositionalArguments() const override { return 0; }

private:
    std::string fileList_;
    std::string outputDirectory_;
    long nWorkers_ = 0;
    long averageWorkUnitsPerRank_ = 0;
    bool scanWorkUnitMessages_ = false;
};

}  // namespace multio::grib2grib

int main(int argc, char** argv) {
    multio::grib2grib::Grib2GribDataDistributionTest tool(argc, argv);
    return tool.start();
}

/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#include <string>
#include "eckit/exception/Exceptions.h"
#include "eckit/log/Log.h"
#include "eckit/mpi/Comm.h"
#include "eckit/option/CmdArgs.h"
#include "eckit/option/SimpleOption.h"

#include "multio/tools/grib2grib/MultioToolUtils.h"
#include "multio/tools/MultioTool.h"

namespace multio::grib2grib {

class DistributedGribToGrib final : public multio::MultioTool {
public:
    DistributedGribToGrib(int argc, char** argv) : multio::MultioTool(argc, argv) {
        options_.push_back(
            new eckit::option::SimpleOption<std::string>("options-file", "Path to YAML options file"));
        options_.push_back(new eckit::option::SimpleOption<std::string>("file-list", "Path to file list"));
        options_.push_back(
            new eckit::option::SimpleOption<std::string>("output-directory", "Path to output directory"));
        options_.push_back(new eckit::option::SimpleOption<long>("average-work-units-per-rank",
                                                                 "Average number of work units per MPI rank, default=15"));
    }

private:
    void usage(const std::string& tool) const override {
        eckit::Log::info() << "\nUsage: " << tool
                           << " --options-file <options.yaml> --file-list <file-list.txt> --output-directory <path>"
                              " [--average-work-units-per-rank <N>]\n";
    }

    void init(const eckit::option::CmdArgs& args) override {
        args.get("options-file", optionsFile_);
        args.get("file-list", fileList_);
        args.get("output-directory", outputDirectory_);
        args.get("average-work-units-per-rank", averageWorkUnitsPerRank_);

        if (optionsFile_.empty()) {
            throw eckit::UserError("Missing required option --options-file", Here());
        }
        if (fileList_.empty()) {
            throw eckit::UserError("Missing required option --file-list", Here());
        }
        if (outputDirectory_.empty()) {
            throw eckit::UserError("Missing required option --output-directory", Here());
        }
        if (averageWorkUnitsPerRank_ <= 0) {
            throw eckit::UserError("--average-work-units-per-rank must be > 0", Here());
        }
    }

    void execute(const eckit::option::CmdArgs&) override {
        auto& comm = eckit::mpi::comm();
        const auto rank = comm.rank();

        // Gloal initialisation of the MPI environment and loading of options
        const auto rawOptions = utils::loadAndBroadcastOptionsAsConfiguration(optionsFile_, comm);
        const auto workUnits = utils::distributeWork(fileList_, averageWorkUnitsPerRank_, comm);

        // Local initialisation of Processing environment and processing of rank-owned work units
        auto writer = utils::buildRankLocalWriter(rawOptions, outputDirectory_, comm);
        const auto context = utils::buildGlobalContext(rawOptions);

        // Process rank-owned work units and gather outcomes from all ranks
        const auto workUnitsOutcomePerTask = utils::processWorkUnits(workUnits, context, *writer);

        //  Gather and summarize outcomes from all ranks, and write summary on rank 0
        const auto workUnitOutcomeGlobal = utils::gatherWorkUnitOutcome(workUnitsOutcomePerTask, comm);

        // Write summary on rank 0
        if (rank == 0) {
            const auto workUnitOutcomePerFile = utils::summarizeWorkUnitOutcomePerFile(workUnitOutcomeGlobal);
            const auto summary = utils::createSummary(workUnitOutcomePerFile);
            utils::writeSummary(summary);
        }
    }

    void finish(const eckit::option::CmdArgs&) override {}

    int numberOfPositionalArguments() const override { return 0; }
    int minimumPositionalArguments() const override { return 0; }

private:
    std::string optionsFile_;
    std::string fileList_;
    std::string outputDirectory_;
    long averageWorkUnitsPerRank_ = 15;
};

}  // namespace multio::grib2grib

int main(int argc, char** argv) {
    multio::grib2grib::DistributedGribToGrib tool(argc, argv);
    return tool.start();
}

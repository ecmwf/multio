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
/// @brief Throwaway isolation harness for `Grib2GribSinks`.
///
/// Reads GRIB messages from an input file and pushes them through a
/// `Grib2GribSinks` constructed from the same options file used by the
/// distributed tool, exercising the data sink, the testcase sink and flush.

#include <string>

#include "eckit/exception/Exceptions.h"
#include "eckit/log/Log.h"
#include "eckit/option/CmdArgs.h"
#include "eckit/option/SimpleOption.h"

#include "metkit/codes/api/CodesAPI.h"

#include "multio/sink/DataSink.h"
#include "multio/tools/MultioTool.h"
#include "multio/tools/grib2grib/CodesHandleToEckitMessage.h"
#include "multio/tools/grib2grib/OptionsUtils.h"
#include "multio/tools/grib2grib/Sink.h"
#include "multio/tools/grib2grib/UnitOfWork.h"

namespace multio::grib2grib {

class Grib2GribSinkTest final : public multio::MultioTool {
public:
    Grib2GribSinkTest(int argc, char** argv) : multio::MultioTool(argc, argv) {
        options_.push_back(new eckit::option::SimpleOption<std::string>("options-file", "Path to YAML options file"));
        options_.push_back(new eckit::option::SimpleOption<std::string>("input-file", "Path to input GRIB file"));
        options_.push_back(
            new eckit::option::SimpleOption<std::string>("output-directory", "Path to output directory"));
        options_.push_back(new eckit::option::SimpleOption<long>("rank", "Rank used for output filenames (default 0)"));
    }

private:
    void usage(const std::string& tool) const override {
        eckit::Log::info() << "\nUsage: " << tool
                           << " --options-file <options.yaml> --input-file <input.grib>"
                              " --output-directory <dir> [--rank <n>]\n";
    }

    void init(const eckit::option::CmdArgs& args) override {
        args.get("options-file", optionsFile_);
        args.get("input-file", inputFile_);
        args.get("output-directory", outputDirectory_);
        args.get("rank", rank_);

        if (optionsFile_.empty()) {
            throw eckit::UserError("Missing required option --options-file", Here());
        }
        if (inputFile_.empty()) {
            throw eckit::UserError("Missing required option --input-file", Here());
        }
        if (outputDirectory_.empty()) {
            throw eckit::UserError("Missing required option --output-directory", Here());
        }
    }

    void execute(const eckit::option::CmdArgs&) override {
        namespace g2g = multio::distGrib1ToGrib2::grib2grib;

        const auto rawOptions = g2g::parseOptionsYaml(g2g::readOptionsFileAsString(optionsFile_));

        g2g::Grib2GribSinks sinks{rawOptions, outputDirectory_, static_cast<int>(rank_)};

        g2g::UnitOfWork unitOfWork{g2g::WorkUnit{inputFile_, 0, g2g::fileSizeBytes(inputFile_)}};
        unitOfWork.open();
        while (unitOfWork.newMessageAvailable()) {
            const auto message = unitOfWork.nextMessage();
            if (!message) {
                break;
            }

            sinks.mainDataSink().write(g2g::to_eckit_message(*message));

            if (sinks.testCaseSink() != nullptr) {
                sinks.testCaseSink()->write("grib2grib-sink-test synthetic testcase line\n");
            }
        }
        unitOfWork.close();

        sinks.flush();
    }

    void finish(const eckit::option::CmdArgs&) override {}

    int numberOfPositionalArguments() const override { return 0; }
    int minimumPositionalArguments() const override { return 0; }

private:
    std::string optionsFile_;
    std::string inputFile_;
    std::string outputDirectory_;
    long rank_ = 0;
};

}  // namespace multio::grib2grib

int main(int argc, char** argv) {
    multio::grib2grib::Grib2GribSinkTest tool(argc, argv);
    return tool.start();
}

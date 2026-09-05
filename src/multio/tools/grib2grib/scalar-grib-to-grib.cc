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
#include "eckit/option/CmdArgs.h"
#include "eckit/option/SimpleOption.h"

#include "metkit/codes/api/CodesAPI.h"

#include "multio/tools/MultioTool.h"
#include "multio/tools/grib2grib/GlobalContext.h"
#include "multio/tools/grib2grib/OptionsUtils.h"
#include "multio/tools/grib2grib/ProcessOneMessage.h"
#include "multio/tools/grib2grib/Sink.h"
#include "multio/tools/grib2grib/StageOutcomes.h"
#include "multio/tools/grib2grib/UnitOfWork.h"

namespace multio::grib2grib {

class ScalarGribToGrib final : public multio::MultioTool {
public:
    ScalarGribToGrib(int argc, char** argv) : multio::MultioTool(argc, argv) {
        options_.push_back(new eckit::option::SimpleOption<std::string>("options-file", "Path to YAML options file"));
        options_.push_back(new eckit::option::SimpleOption<std::string>("input-file", "Path to input GRIB file"));
        options_.push_back(
            new eckit::option::SimpleOption<std::string>("output-directory", "Path to output directory"));
    }

private:
    void usage(const std::string& tool) const override {
        eckit::Log::info() << "\nUsage: " << tool
                           << " --options-file <options.yaml> --input-file <input.grib>"
                              " --output-directory <path>\n";
    }

    void init(const eckit::option::CmdArgs& args) override {
        args.get("options-file", optionsFile_);
        args.get("input-file", inputFile_);
        args.get("output-directory", outputDirectory_);

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
        g2g::validateGlobalContext(rawOptions);
        auto context = g2g::parseGlobalContext(rawOptions);
        g2g::Grib2GribSinks writer{rawOptions, outputDirectory_, 0, context.marsToGrib.generateTestcases,
                                   context.marsToGrib.testcasesDir};
        g2g::FileStageOutcomes outcomes;
        outcomes.filename = inputFile_;

        g2g::UnitOfWork input{g2g::WorkUnit{inputFile_, 0, g2g::fileSizeBytes(inputFile_)}, context.reader.mode};
        input.open();
        outcomes.openFile.bump(g2g::OpenFileCode::Valid);

        while (input.newMessageAvailable()) {
            auto message = input.nextMessage();
            if (!message) {
                break;
            }

            outcomes.readMessage.bump(g2g::ReadMessageCode::Valid);
            g2g::processOneMessage(*message, context, writer, outcomes);
        }

        writer.flush();
        outcomes.fileFlush.bump(g2g::FileFlushCode::Valid);
        if (!input.close()) {
            ++outcomes.nCloseFailures;
        }
        g2g::freeGlobalContext(context);
    }

    void finish(const eckit::option::CmdArgs&) override {}

    int numberOfPositionalArguments() const override { return 0; }
    int minimumPositionalArguments() const override { return 0; }

private:
    std::string optionsFile_;
    std::string inputFile_;
    std::string outputDirectory_;
};

}  // namespace multio::grib2grib

int main(int argc, char** argv) {
    multio::grib2grib::ScalarGribToGrib tool(argc, argv);
    return tool.start();
}

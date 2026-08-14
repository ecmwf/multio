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

#include "multio/tools/MultioTool.h"
#include "multio/tools/grib2grib/GlobalContext.h"
#include "multio/tools/grib2grib/OptionsUtils.h"

namespace multio::grib2grib {

class Grib2GribOptionsParser final : public multio::MultioTool {
public:
    Grib2GribOptionsParser(int argc, char** argv) : multio::MultioTool(argc, argv) {
        options_.push_back(
            new eckit::option::SimpleOption<std::string>("options-file", "Path to YAML options file"));
    }

private:
    void usage(const std::string& tool) const override {
        eckit::Log::info() << "\nUsage: " << tool << " --options-file <options.yaml>\n";
    }

    void init(const eckit::option::CmdArgs& args) override {
        args.get("options-file", optionsFile_);

        if (optionsFile_.empty()) {
            throw eckit::UserError("Missing required option --options-file", Here());
        }
    }

    void execute(const eckit::option::CmdArgs&) override {
        const auto payload = multio::distGrib1ToGrib2::grib2grib::readOptionsFileAsString(optionsFile_);
        const auto rawOptions = multio::distGrib1ToGrib2::grib2grib::parseOptionsYaml(payload);

        multio::distGrib1ToGrib2::grib2grib::validateGlobalContext(rawOptions);
        auto context = multio::distGrib1ToGrib2::grib2grib::parseGlobalContext(rawOptions);
        multio::distGrib1ToGrib2::grib2grib::freeGlobalContext(context);
    }

    void finish(const eckit::option::CmdArgs&) override {}

    int numberOfPositionalArguments() const override { return 0; }
    int minimumPositionalArguments() const override { return 0; }

private:
    std::string optionsFile_;
};

}  // namespace multio::grib2grib

int main(int argc, char** argv) {
    multio::grib2grib::Grib2GribOptionsParser tool(argc, argv);
    return tool.start();
}

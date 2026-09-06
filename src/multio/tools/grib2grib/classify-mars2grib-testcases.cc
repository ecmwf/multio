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
#include <sstream>
#include <string>
#include <vector>

#include "eckit/config/YAMLConfiguration.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/filesystem/PathName.h"
#include "eckit/log/Log.h"
#include "eckit/option/CmdArgs.h"
#include "eckit/option/SimpleOption.h"
#include "eckit/utils/MD5.h"

#include "metkit/mars2grib/api/Mars2GribClassify.h"

#include "multio/tools/MultioTool.h"

namespace multio::grib2grib {

namespace {

std::string classificationHash(const std::string& classification) {
    eckit::MD5 md5;
    md5.add(classification);
    return md5.digest();
}

bool isWithin(const eckit::PathName& path, const eckit::PathName& directory) {
    const std::string normalizedPath = path.fullName().asString() + "/";
    const std::string normalizedDirectory = directory.fullName().asString() + "/";
    return normalizedPath.compare(0, normalizedDirectory.size(), normalizedDirectory) == 0;
}

void validateDirectories(const eckit::PathName& inputDirectory, const eckit::PathName& outputDirectory) {
    if (!inputDirectory.exists() || !inputDirectory.isDir()) {
        throw eckit::UserError("Input directory does not exist or is not a directory: " + inputDirectory.asString(),
                               Here());
    }
    if (isWithin(outputDirectory, inputDirectory) || isWithin(inputDirectory, outputDirectory)) {
        throw eckit::UserError("Input and output directories must not overlap", Here());
    }
    if (outputDirectory.exists()) {
        std::vector<eckit::PathName> files;
        std::vector<eckit::PathName> directories;
        if (!outputDirectory.isDir()) {
            throw eckit::UserError("Output path is not a directory: " + outputDirectory.asString(), Here());
        }
        outputDirectory.children(files, directories);
        if (!files.empty() || !directories.empty()) {
            throw eckit::UserError("Output directory must be empty or nonexistent: " + outputDirectory.asString(),
                                   Here());
        }
    }
}

std::vector<eckit::PathName> inputFiles(const eckit::PathName& inputDirectory) {
    std::vector<eckit::PathName> files;
    std::vector<eckit::PathName> directories;
    inputDirectory.childrenRecursive(files, directories);
    std::sort(files.begin(), files.end());
    return files;
}

void writeOnce(const eckit::PathName& path, const std::string& contents) {
    if (path.exists()) {
        return;
    }
    std::ofstream output(path.asString(), std::ios::binary);
    if (!output) {
        throw eckit::CantOpenFile(path, Here());
    }
    output << contents;
    if (!output) {
        throw eckit::WriteError("Failed to write " + path.asString(), Here());
    }
}

bool filter(const eckit::LocalConfiguration&, const eckit::LocalConfiguration&, const std::string&,
            const metkit::mars2grib::ProductTimeSpecResult&) {
    return true;
}

void classifyAndWrite(const std::string& line, const eckit::PathName& outputDirectory, bool splitByMarsKeys) {
    std::istringstream input(line);
    eckit::YAMLConfiguration yaml(input);
    const eckit::LocalConfiguration testCase(yaml);

    for (const char* key : {"mars", "misc", "opt", "out"}) {
        if (!testCase.has(key) || !testCase.isSubConfiguration(key)) {
            throw eckit::BadValue(std::string("Testcase requires subconfiguration '") + key + "'", Here());
        }
    }

    const auto mars = testCase.getSubConfiguration("mars");
    const auto misc = testCase.getSubConfiguration("misc");
    const auto options = testCase.getSubConfiguration("opt");
    metkit::mars2grib::Mars2GribClassify classifier(options);
    const auto activeConcepts = classifier.computeActiveConcepts(mars, misc);
    const auto productTimeSpec = classifier.computeProductTimeSpec(mars, misc);

    if (!filter(mars, misc, activeConcepts, productTimeSpec)) {
        return;
    }

    eckit::PathName activeDirectory = outputDirectory / classificationHash(activeConcepts);
    activeDirectory.mkdir();
    writeOnce(activeDirectory / "ActiveConcepts.json", activeConcepts);

    eckit::PathName destination = activeDirectory / classificationHash(productTimeSpec.classification);
    destination.mkdir();
    writeOnce(destination / "productTimeSpec.json", productTimeSpec.json);

    if (splitByMarsKeys) {
        destination /= mars.getString("class");
        destination /= mars.getString("stream");
        destination /= mars.getString("type");
        destination.mkdir();
    }

    const eckit::PathName outputPath = destination / "testcases.jsonl";
    std::ofstream output(outputPath.asString(), std::ios::binary | std::ios::app);
    if (!output) {
        throw eckit::CantOpenFile(outputPath, Here());
    }
    output << line << '\n';
    if (!output) {
        throw eckit::WriteError("Failed to append " + outputPath.asString(), Here());
    }
}

}  // namespace

class ClassifyMars2GribTestcases final : public multio::MultioTool {
public:
    ClassifyMars2GribTestcases(int argc, char** argv) : multio::MultioTool(argc, argv) {
        options_.push_back(
            new eckit::option::SimpleOption<std::string>("input-directory", "Input testcase directory"));
        options_.push_back(
            new eckit::option::SimpleOption<std::string>("output-directory", "Classified output directory"));
        options_.push_back(new eckit::option::SimpleOption<bool>(
            "split-by-mars-keys", "Add class/stream/type directories below classification hashes"));
        options_.push_back(
            new eckit::option::SimpleOption<bool>("skip-errors", "Report invalid records and continue"));
    }

private:
    void usage(const std::string& tool) const override {
        eckit::Log::info() << "\nUsage: " << tool
                           << " --input-directory <path> --output-directory <path>"
                              " [--split-by-mars-keys=true] [--skip-errors=true]\n";
    }

    void init(const eckit::option::CmdArgs& args) override {
        args.get("input-directory", inputDirectory_);
        args.get("output-directory", outputDirectory_);
        args.get("split-by-mars-keys", splitByMarsKeys_);
        args.get("skip-errors", skipErrors_);

        if (inputDirectory_.empty()) {
            throw eckit::UserError("Missing required option --input-directory", Here());
        }
        if (outputDirectory_.empty()) {
            throw eckit::UserError("Missing required option --output-directory", Here());
        }
    }

    void execute(const eckit::option::CmdArgs&) override {
        const eckit::PathName inputDirectory(inputDirectory_);
        const eckit::PathName outputDirectory(outputDirectory_);
        validateDirectories(inputDirectory, outputDirectory);
        outputDirectory.mkdir();

        for (const auto& file : inputFiles(inputDirectory)) {
            std::ifstream input(file.asString());
            if (!input) {
                throw eckit::CantOpenFile(file, Here());
            }

            std::string line;
            std::size_t lineNumber = 0;
            while (std::getline(input, line)) {
                ++lineNumber;
                if (line.empty()) {
                    continue;
                }
                try {
                    classifyAndWrite(line, outputDirectory, splitByMarsKeys_);
                }
                catch (const eckit::Exception& error) {
                    if (!skipErrors_) {
                        throw eckit::UserError(file.asString() + ":" + std::to_string(lineNumber) + ": "
                                                   + error.what(),
                                               Here());
                    }
                    eckit::Log::error() << file.asString() << ":" << lineNumber << ": " << error.what() << std::endl;
                }
            }
            if (!input.eof()) {
                throw eckit::ReadError("Failed to read " + file.asString(), Here());
            }
        }
    }

    void finish(const eckit::option::CmdArgs&) override {}

    int numberOfPositionalArguments() const override { return 0; }
    int minimumPositionalArguments() const override { return 0; }

    std::string inputDirectory_;
    std::string outputDirectory_;
    bool splitByMarsKeys_ = false;
    bool skipErrors_ = false;
};

}  // namespace multio::grib2grib

int main(int argc, char** argv) {
    multio::grib2grib::ClassifyMars2GribTestcases tool(argc, argv);
    return tool.start();
}

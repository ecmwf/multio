#include <fstream>
#include <iomanip>
#include <iostream>

#include <algorithm>
#include <array>
#include <fstream>
#include <functional>
#include <iostream>
#include <iterator>
#include <regex>
#include <sstream>
#include <string>
#include <vector>


#include "eckit/exception/Exceptions.h"
#include "eckit/filesystem/PathName.h"
#include "eckit/log/Log.h"
#include "eckit/option/CmdArgs.h"
#include "eckit/option/SimpleOption.h"
#include "multio/tools/MultioTool.h"

#include "FesomInterpolationWeights.h"
#include "InterpolateFesom.h"

namespace multio::action::interpolate_fesom {


class FesomCacheValidator final : public multio::MultioTool {
public:  // methods
    FesomCacheValidator(int argc, char** argv);

private:
    void usage(const std::string& tool) const override {
        eckit::Log::info() << std::endl << "Usage: " << tool << " [options]" << std::endl;
        eckit::Log::info()
            << "EXAMPLE: " << std::endl
            << "fesom-cache-validator  --inputPath=. --outputPath=.  --outputFile=file.csv --inputFile=file.atlas "
            << std::endl
            << std::endl;
    }

    void init(const eckit::option::CmdArgs& args) override;

    void finish(const eckit::option::CmdArgs&) override;

    void execute(const eckit::option::CmdArgs& args) override;

    int numberOfPositionalArguments() const override { return 0; }
    int minimumPositionalArguments() const override { return 0; }

    std::string inputPath_;
    std::string outputPath_;
    std::string inputFile_;
    std::string outputFile_;
    std::string mode_;
};


FesomCacheValidator::FesomCacheValidator(int argc, char** argv) :
    multio::MultioTool{argc, argv},
    inputPath_{"."},
    outputPath_{"."},
    inputFile_{"fesomCache.atlas"},
    outputFile_{"fesomTriplets.csv"},
    mode_{"raw"} {

    options_.push_back(new eckit::option::SimpleOption<std::string>(
        "outputFile", "Name of the output file Default(\"fesomTriplets.csv\")"));
    options_.push_back(new eckit::option::SimpleOption<std::string>(
        "inputPath", "Path of the input files with the triplets. Default( \".\" )"));
    options_.push_back(new eckit::option::SimpleOption<std::string>(
        "outputPath", "Path of the output files with the triplets. Default( \".\" )"));
    options_.push_back(new eckit::option::SimpleOption<std::string>(
        "inputFile", "Name of the input file. Default( \"fesomCache.atlas\" )"));
    options_.push_back(new eckit::option::SimpleOption<std::string>(
        "sortOption",
        "Type of sort to be applied to the triplets [\"columMajor\"|\"rowMajor\"|\"raw\"]. Default(\"raw\")"));

    return;
}


void FesomCacheValidator::init(const eckit::option::CmdArgs& args) {

    args.get("inputPath", inputPath_);
    args.get("outputPath", outputPath_);
    args.get("inputFile", inputFile_);
    args.get("outputFile", outputFile_);
    args.get("sortOption", mode_);

    eckit::PathName inputPath_tmp{inputPath_};
    ASSERT(inputPath_tmp.exists());
    eckit::PathName inputFile_tmp{inputPath_ + "/" + inputFile_};
    ASSERT(inputFile_tmp.exists());
    eckit::PathName outputPath_tmp{outputPath_};
    outputPath_tmp.mkdir();
    ASSERT(mode_ == "raw" || mode_ == "columMajor" || mode_ == "rowMajor");
}

void FesomCacheValidator::execute(const eckit::option::CmdArgs& args) {

    const std::string iFname{inputPath_ + "/" + inputFile_};
    const std::string oFname{outputPath_ + "/" + outputFile_};
    Fesom2HEALPix<double> cache(iFname);


    if (mode_ == "columMajor") {
        // Get triplets
        std::vector<Tri> triplets;
        cache.getTriplets(triplets);
        // Open the file for writing
        std::ofstream file(oFname);
        // Check if the file is opened successfully
        if (!file.is_open()) {
            std::ostringstream os;
            os << " - Unable to open output csv file: " << iFname << std::endl;
            throw eckit::SeriousBug(os.str(), Here());
        }
        // Sort triplets
        std::sort(triplets.begin(), triplets.end(),
                  [](Tri a, Tri b) { return a.reverse_idx(1000000000) < b.reverse_idx(1000000000); });
        // Write triplets
        for (const auto& t : triplets) {
            file << std::setw(10) << t.h() << "," << std::setw(10) << t.f() << "," << std::setw(15) << std::fixed
                 << std::setprecision(8) << t.v() << std::endl;
        }
        // Close the file
        file.close();
    }

    if (mode_ == "rowMajor") {
        // Get triplets
        std::vector<Tri> triplets;
        cache.getTriplets(triplets);
        // Open the file for writing
        std::ofstream file(oFname);
        // Check if the file is opened successfully
        if (!file.is_open()) {
            std::ostringstream os;
            os << " - Unable to open output csv file: " << iFname << std::endl;
            throw eckit::SeriousBug(os.str(), Here());
        }
        // Sort triplets
        std::sort(triplets.begin(), triplets.end(), [](Tri a, Tri b) { return a.idx(1000000000) < b.idx(1000000000); });
        // Write triplets
        for (const auto& t : triplets) {
            file << std::setw(10) << t.h() << "," << std::setw(10) << t.f() << "," << std::setw(15) << std::fixed
                 << std::setprecision(8) << t.v() << std::endl;
        }
        // Close the file
        file.close();
    }

    if (mode_ == "raw") {
        cache.dumpCOO(oFname);
    }
};


void FesomCacheValidator::finish(const eckit::option::CmdArgs&) {}

}  // namespace multio::action::interpolate_fesom


int main(int argc, char** argv) {
    multio::action::interpolate_fesom::FesomCacheValidator tool(argc, argv);
    return tool.start();
}

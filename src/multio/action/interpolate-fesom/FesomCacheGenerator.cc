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

namespace multio::action::interpolateFESOM {

namespace {

void parseInputFileName(const std::string& fname, std::string& fesomName, std::string& domain, size_t& NSide,
                        size_t& level, orderingConvention_e& orderingConvention) {
    static const std::regex fnameGrammar("([^_]+)_([^_]+)_NSIDE([1-9][0-9]*)_([0-9][0-9]*)_([a-zA-Z]*).csv");
    std::smatch matchFName;
    if (std::regex_match(fname, matchFName, fnameGrammar)) {
        fesomName = matchFName[1].str();
        domain = matchFName[2].str();
        NSide = static_cast<size_t>(std::stoi(matchFName[3].str()));
        level = static_cast<size_t>(std::stoi(matchFName[4].str()));
        orderingConvention = orderingConvention_string2enum(matchFName[5].str());
    }
    else {
        throw eckit::SeriousBug("Unable to parse filename: " + fname, Here());
    }
    return;
}


std::string creteMeshFileName(const std::string& path, const std::string& fname, std::string& fesomName) {
    static const std::regex fnameGrammar("([^_]+)_(.*)");
    std::smatch matchFName;
    if (std::regex_match(fname, matchFName, fnameGrammar)) {
        fesomName = matchFName[1].str();
    }
    else {
        throw eckit::SeriousBug("Unable to parse filename: " + fname, Here());
    }
    std::string tmp{path + "/" + fname};
    eckit::PathName file{tmp};
    if (!file.exists()) {
        throw eckit::SeriousBug("Unable to open file: " + tmp, Here());
    }
    return tmp;
}

}  // namespace


class FesomCacheGenerator final : public multio::MultioTool {
public:  // methods
    FesomCacheGenerator(int argc, char** argv);

private:
    void usage(const std::string& tool) const override {
        eckit::Log::info() << std::endl << "Usage: " << tool << " [options]" << std::endl;
        eckit::Log::info()
            << "EXAMPLE: " << std::endl
            << "fesom-cache-generator --mode=fromTriplets --inputPath=. --inputFile=CORE2_ngrid_NSIDE32_0_ring.csv "
               "--dumpTriplets=1"
            << std::endl
            << std::endl;
    }

    void init(const eckit::option::CmdArgs& args) override;

    void finish(const eckit::option::CmdArgs&) override;

    void execute(const eckit::option::CmdArgs& args) override;

    void loadTriplets();

    int numberOfPositionalArguments() const override { return 0; }
    int minimumPositionalArguments() const override { return 0; }

    std::string inputPath_;
    std::string outputPath_;
    std::string outputPrecision_;
    std::string inputFile_;
    std::string workingMode_;
    bool dumpTriplets_;

    std::string fesomName_;
    std::string domain_;
    size_t NSide_;
    size_t level_;
    orderingConvention_e orderingConvention_;

    void loadTriplets(std::vector<Tri>& triplets) const;
};


FesomCacheGenerator::FesomCacheGenerator(int argc, char** argv) :
    multio::MultioTool{argc, argv},
    inputPath_{"."},
    outputPath_{"."},
    inputFile_{"CORE2_ngrid_NSIDE32_0_ring.csv"},
    workingMode_{"fromTriplets"},
    dumpTriplets_{false},
    fesomName_{"CORE2"},
    domain_{"ngrid"},
    NSide_{0},
    level_{0},
    orderingConvention_{orderingConvention_e::RING} {

    options_.push_back(new eckit::option::SimpleOption<std::string>(
        "mode", "WorkingMode [fromTriplets]. Default( \"fromTriplets\" )"));
    options_.push_back(new eckit::option::SimpleOption<std::string>(
        "inputPath", "Path of the input files with the triplets. Default( \".\" )"));
    options_.push_back(new eckit::option::SimpleOption<std::string>(
        "outputPath", "Path of the output files with the triplets. Default( \".\" )"));
    options_.push_back(new eckit::option::SimpleOption<std::string>(
        "inputFile", "Name of the input file. Default( \"CORE2_ngrid_NSIDE32_0_ring.csv\" )"));
    options_.push_back(new eckit::option::SimpleOption<bool>("dumpTriplets", "Dump all the triplets to screen"));

    return;
}


void FesomCacheGenerator::loadTriplets(std::vector<Tri>& triplets) const {
    std::ifstream file(inputPath_ + "/" + inputFile_);
    if (!file.good()) {
        throw eckit::SeriousBug("Unable to read the input file", Here());
    }

    std::string line;
    triplets.resize(0);
    while (std::getline(file, line)) {
        int32_t iHEALPix;
        int32_t iFESOM;
        double weight;
        static const std::regex lineGrammar(
            "([0-9][0-9]*)\\s+([0-9][0-9]*)\\s*([+]?([0-9]*[.])?[0-9]+([eE][-+][0-9]+)?)");
        std::smatch matchLine;
        if (std::regex_match(line, matchLine, lineGrammar)) {
            iHEALPix = std::stoi(matchLine[1].str());
            iFESOM = std::stoi(matchLine[2].str());
            weight = std::stod(matchLine[3].str());
        }
        else {
            throw eckit::SeriousBug("Unable to parse line: " + line, Here());
        }
        triplets.emplace_back(iHEALPix, iFESOM, weight);
    }
    file.close();
}


void FesomCacheGenerator::init(const eckit::option::CmdArgs& args) {

    args.get("mode", workingMode_);
    ASSERT(workingMode_ == "fromTriplets");

    if (workingMode_ == "fromTriplets") {
        args.get("inputPath", inputPath_);
        args.get("outputPath", outputPath_);
        args.get("inputFile", inputFile_);
        args.get("dumpTriplets", dumpTriplets_);

        eckit::PathName inputPath_tmp{inputPath_};
        ASSERT(inputPath_tmp.exists());
        eckit::PathName inputFile_tmp{inputPath_ + "/" + inputFile_};
        ASSERT(inputFile_tmp.exists());
        eckit::PathName outputPath_tmp{outputPath_};
        outputPath_tmp.mkdir();
        parseInputFileName(inputFile_, fesomName_, domain_, NSide_, level_, orderingConvention_);
    }
}

void FesomCacheGenerator::execute(const eckit::option::CmdArgs& args) {

    size_t level;
    size_t nnz;
    size_t nRows;
    size_t nCols;
    size_t nOutRows;
    std::vector<std::int32_t> landSeaMask;
    std::vector<std::int32_t> rowStart;
    std::vector<std::int32_t> colIdx;
    std::vector<float> valuesf;
    std::vector<double> valuesd;

    if (workingMode_ == "fromTriplets") {
        std::vector<Tri> triplets;

        loadTriplets(triplets);

        FesomInterpolationWeights weightsf(triplets);
        FesomInterpolationWeights weightsd(triplets);


        weightsf.generateCacheFromTriplets(NSide_, orderingConvention_, level_, nnz, nRows, nCols, nOutRows,
                                           landSeaMask, rowStart, colIdx, valuesf);

        weightsf.dumpCache(outputPath_, fesomName_, domain_, NSide_, orderingConvention_, level_, nnz, nRows, nCols,
                           nOutRows, landSeaMask, rowStart, colIdx, valuesf);

        weightsd.generateCacheFromTriplets(NSide_, orderingConvention_, level_, nnz, nRows, nCols, nOutRows,
                                           landSeaMask, rowStart, colIdx, valuesd);

        weightsd.dumpCache(outputPath_, fesomName_, domain_, NSide_, orderingConvention_, level_, nnz, nRows, nCols,
                           nOutRows, landSeaMask, rowStart, colIdx, valuesd);

        if (dumpTriplets_) {
            weightsf.dumpTriplets();
        }
    }
};


void FesomCacheGenerator::finish(const eckit::option::CmdArgs&) {}

}  // namespace multio::action::interpolateFESOM


int main(int argc, char** argv) {
    multio::action::interpolateFESOM::FesomCacheGenerator tool(argc, argv);
    return tool.start();
}

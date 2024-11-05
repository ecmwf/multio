#include <iostream>

#include <algorithm>
#include <array>
#include <cmath>
#include <fstream>
#include <functional>
#include <iomanip>
#include <iostream>
#include <iterator>
#include <regex>
#include <sstream>
#include <string>
#include <vector>

#include "eckit/exception/Exceptions.h"
#include "eckit/filesystem/PathName.h"
#include "eckit/linalg/Triplet.h"
#include "eckit/log/Log.h"
#include "eckit/option/CmdArgs.h"
#include "eckit/option/SimpleOption.h"
#include "mir/method/WeightMatrix.h"
#include "multio/tools/MultioTool.h"

namespace multio::action::interpolateFESOM2MIR {

namespace {


void parseInputFileName(const std::string& fname, std::string& fesomName, std::string& domain, size_t& Nrows, size_t& Ncols,
                        size_t& level) {
    static const std::regex fnameGrammar("([^_]+)_([^_]+)_([1-9][0-9]*)_([1-9][0-9]*)_([0-9][0-9]*)(?:_([a-zA-Z]*))?\\.csv");
    std::smatch matchFName;
    if (std::regex_match(fname, matchFName, fnameGrammar)) {
        fesomName = matchFName[1].str();
        domain = matchFName[2].str();
        Nrows = static_cast<size_t>(std::stoi(matchFName[3].str()));
        Ncols = static_cast<size_t>(std::stoi(matchFName[4].str()));
        level = static_cast<size_t>(std::stoi(matchFName[5].str()));
    }
    else {
        throw eckit::SeriousBug("Unable to parse filename: " + fname, Here());
    }
    return;
}

std::string fesomCacheName(const std::string& fesomName, const std::string& domain, const std::string& precision,
                           size_t Nrows, size_t Ncols,  double level) {
    std::ostringstream os;
    std::string localDomain{domain};
    localDomain.erase(std::remove_if(localDomain.begin(), localDomain.end(), ::isspace), localDomain.end());
    std::transform(localDomain.begin(), localDomain.end(), localDomain.begin(),
                   [](unsigned char c) { return std::tolower(c); });
    os << "fesom_" << fesomName << "_" << localDomain << "_to_latlong_" << std::setw(6) << std::setfill('0') << Nrows <<"_"<<Ncols
       << "_" << precision << "_" << std::setw(8) << std::setfill('0')
       << static_cast<size_t>(std::fabs(level * 1000)) << ".mat";
    return os.str();
}

}  // namespace


class Fesom2mirCacheGenerator final : public multio::MultioTool {
public:  // methods
    Fesom2mirCacheGenerator(int argc, char** argv);

private:
    void usage(const std::string& tool) const override {
        eckit::Log::info() << std::endl << "Usage: " << tool << " [options]" << std::endl;
        eckit::Log::info() << "EXAMPLE: " << std::endl
                           << "fesom-cache-generator-latlong --inputPath=. --inputFile=CORE2_ngrid_32_32_0.csv "
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
    std::string outputPrecision_;
    std::string inputFile_;


    std::string fesomName_;
    std::string domain_;
    size_t Nrow_;
    size_t Ncol_;
    size_t level_;

    void loadTriplets(std::vector<eckit::linalg::Triplet>& triplets) const;
};


Fesom2mirCacheGenerator::Fesom2mirCacheGenerator(int argc, char** argv) :
    multio::MultioTool{argc, argv},
    inputPath_{"."},
    outputPath_{"."},
    inputFile_{"CORE2_ngrid_32_32_0.csv"},
    fesomName_{"CORE2"},
    domain_{"ngrid"},
    Nrow_{0},
    Ncol_{0},
    level_{0} {

    options_.push_back(new eckit::option::SimpleOption<std::string>(
        "inputPath", "Path of the input files with the triplets. Default( \".\" )"));
    options_.push_back(new eckit::option::SimpleOption<std::string>(
        "outputPath", "Path of the output files with the triplets. Default( \".\" )"));
    options_.push_back(new eckit::option::SimpleOption<std::string>(
        "inputFile", "Name of the input file. Default( \"CORE2_ngrid_32_32_0.csv\" )"));

    return;
}


void Fesom2mirCacheGenerator::loadTriplets(std::vector<eckit::linalg::Triplet>& triplets) const {
    std::ifstream file(inputPath_ + "/" + inputFile_);
    if (!file.good()) {
        throw eckit::SeriousBug("Unable to read the input file", Here());
    }

    std::string line;
    triplets.resize(0);
    while (std::getline(file, line)) {
        int32_t iLatlong;
        int32_t iFESOM;
        double weight;
        static const std::regex lineGrammar(
            "([0-9][0-9]*)\\s+([0-9][0-9]*)\\s*([+]?([0-9]*[.])?[0-9]+([eE][-+][0-9]+)?)");
        std::smatch matchLine;
        if (std::regex_match(line, matchLine, lineGrammar)) {
            iLatlong = std::stoi(matchLine[1].str());
            iFESOM = std::stoi(matchLine[2].str());
            weight = std::stod(matchLine[3].str());
        }
        else {
            throw eckit::SeriousBug("Unable to parse line: " + line, Here());
        }
        triplets.emplace_back(iLatlong, iFESOM, weight);
    }
    file.close();
}


void Fesom2mirCacheGenerator::init(const eckit::option::CmdArgs& args) {
    args.get("inputPath", inputPath_);
    args.get("outputPath", outputPath_);
    args.get("inputFile", inputFile_);

    eckit::PathName inputPath_tmp{inputPath_};
    ASSERT(inputPath_tmp.exists());
    eckit::PathName inputFile_tmp{inputPath_ + "/" + inputFile_};
    ASSERT(inputFile_tmp.exists());
    eckit::PathName outputPath_tmp{outputPath_};
    outputPath_tmp.mkdir();
    parseInputFileName(inputFile_, fesomName_, domain_, Nrow_, Ncol_, level_);

}

void Fesom2mirCacheGenerator::execute(const eckit::option::CmdArgs& args) {
    std::vector<eckit::linalg::Triplet> triplets;
    loadTriplets(triplets);

    std::sort(begin(triplets), end(triplets), [](const auto& a, const auto& b) { return a.row() < b.row(); });

    const auto cacheFileName = fesomCacheName(fesomName_, domain_, "double", Nrow_, Ncol_, level_);

    mir::method::WeightMatrix W(Nrow_, Ncol_);
    W.setFromTriplets(triplets);
    W.save(cacheFileName);
};


void Fesom2mirCacheGenerator::finish(const eckit::option::CmdArgs&) {}

}  // namespace multio::action::interpolateFESOM2MIR


int main(int argc, char** argv) {
    multio::action::interpolateFESOM2MIR::Fesom2mirCacheGenerator tool(argc, argv);
    return tool.start();
}

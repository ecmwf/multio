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

#include "eckit/linalg/Triplet.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/filesystem/PathName.h"
#include "eckit/log/Log.h"
#include "eckit/option/CmdArgs.h"
#include "eckit/option/SimpleOption.h"
#include "multio/tools/MultioTool.h"
#include "mir/method/WeightMatrix.h"

namespace multio::action::interpolateFESOM2MIR {

namespace {

enum class orderingConvention_e : unsigned int
{
    RING,
    NESTED,
    UNKNOWN
};

orderingConvention_e orderingConvention_string2enum(const std::string& orderingConvention) {
    if (orderingConvention != "ring" && orderingConvention != "nested") {
        std::ostringstream os;
        os << " - Unexpected value for \"orderingConvention\": "
           << "\"" << orderingConvention << "\"" << std::endl;
        throw eckit::SeriousBug(os.str(), Here());
    }
    return (orderingConvention == "ring" ? orderingConvention_e::RING : orderingConvention_e::NESTED);
}


std::string orderingConvention_enum2string(orderingConvention_e orderingConvention) {
    if (orderingConvention == orderingConvention_e::UNKNOWN) {
        std::ostringstream os;
        os << " - Unexpected value for \"orderingConvention\": "
           << "\"UNKNOWN\"" << std::endl;
        throw eckit::SeriousBug(os.str(), Here());
    }
    return (orderingConvention == orderingConvention_e::RING ? "ring" : "nested");
}


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

std::string fesomCacheName(const std::string& fesomName, const std::string& domain, const std::string& precision,
                           size_t NSide, const std::string& orderingConvention, double level) {
    std::ostringstream os;
    std::string localDomain{domain};
    localDomain.erase(std::remove_if(localDomain.begin(), localDomain.end(), ::isspace), localDomain.end());
    std::transform(localDomain.begin(), localDomain.end(), localDomain.begin(),
                   [](unsigned char c) { return std::tolower(c); });
    os << "fesom_" << fesomName << "_" << localDomain << "_to_HEALPix_" << std::setw(6) << std::setfill('0') << NSide
       << "_" << precision << "_" << orderingConvention << "_" << std::setw(8)
       << std::setfill('0') << static_cast<size_t>(std::fabs(level * 1000));
    return os.str();
}

}  // namespace


class Fesom2mirCacheGenerator final : public multio::MultioTool {
public:  // methods
    Fesom2mirCacheGenerator(int argc, char** argv);

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
    size_t Nrow_;
    size_t Ncol_;
    orderingConvention_e orderingConvention_;

    void loadTriplets(std::vector<eckit::linalg::Triplet>& triplets, size_t& Nrow, size_t& Ncol ) const;
};


Fesom2mirCacheGenerator::Fesom2mirCacheGenerator(int argc, char** argv) :
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


void Fesom2mirCacheGenerator::loadTriplets(std::vector<eckit::linalg::Triplet>& triplets, size_t& Nrow, size_t& Ncol) const {
    std::ifstream file(inputPath_ + "/" + inputFile_);
    if (!file.good()) {
        throw eckit::SeriousBug("Unable to read the input file", Here());
    }

    std::string line;
    triplets.resize(0);
    Nrow = 0;
    Ncol = 0;
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
            // This code is not safe, it assumes that the indices are all used
            Nrow = std::max(Nrow_, static_cast<size_t>(iHEALPix + 1));
            Ncol = std::max(Ncol_, static_cast<size_t>(iFESOM + 1));
        }
        else {
            throw eckit::SeriousBug("Unable to parse line: " + line, Here());
        }
        triplets.emplace_back(iHEALPix, iFESOM, weight);
    }
    file.close();
}


void Fesom2mirCacheGenerator::init(const eckit::option::CmdArgs& args) {

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

void Fesom2mirCacheGenerator::execute(const eckit::option::CmdArgs& args) {

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
        std::vector<eckit::linalg::Triplet> triplets;

        loadTriplets(triplets, Nrow_, Ncol_);

        {
            mir::method::WeightMatrix W(Nrow_, Ncol_);
            W.setFromTriplets(triplets);
            W.save(fesomCacheName(fesomName_, domain_, "double",
                           NSide_, orderingConvention_enum2string(orderingConvention_), level_));
        }

    }
};


void Fesom2mirCacheGenerator::finish(const eckit::option::CmdArgs&) {}

}  // namespace multio::action::interpolateFESOM


int main(int argc, char** argv) {
    multio::action::interpolateFESOM2MIR::Fesom2mirCacheGenerator tool(argc, argv);
    return tool.start();
}

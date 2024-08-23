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

namespace multio::action::interpolateFESOM {


namespace {

std::vector<std::vector<double>> readCSV(const std::string& fieldsPath, const std::string& fieldsName) {

    eckit::PathName inputPath_tmp{fieldsPath};
    ASSERT(inputPath_tmp.exists());
    eckit::PathName inputFile_tmp{fieldsPath + "/" + fieldsName};
    ASSERT(inputFile_tmp.exists());

    std::string filename(fieldsPath + "/" + fieldsName);
    std::ifstream file(filename);
    std::vector<std::vector<double>> matrix;

    if (!file.is_open()) {
        std::ostringstream os;
        os << " - Unable to open csv file: " << std::endl;
        throw eckit::SeriousBug(os.str(), Here());
    }

    // Read the first line to get dimensions
    std::string line;
    std::getline(file, line);
    std::istringstream iss(line);
    int nRows, nCols;
    if (!(iss >> nRows >> nCols)) {
        std::ostringstream os;
        os << " - Error reading dimensions from the file " << std::endl;
        throw eckit::SeriousBug(os.str(), Here());
    }

    // Resize the matrix
    matrix.resize(nCols, std::vector<double>(nRows));

    // Read the remaining lines and fill the matrix
    for (int i = 0; i < nRows; ++i) {
        for (int j = 0; j < nCols; ++j) {
            if (!(file >> matrix[j][i])) {
                std::ostringstream os;
                os << " - Error reading data from the file " << std::endl;
                throw eckit::SeriousBug(os.str(), Here());
            }
            // Ignore the comma
            file.ignore();
        }
    }

    file.close();
    return matrix;
}


// Function to write the nested vector to a CSV file
void writeCSV(const std::vector<std::vector<double>>& matrix, const std::string& filename) {
    std::ofstream file(filename);

    if (!file.is_open()) {
        std::cerr << "Error opening the file for writing!" << std::endl;
        return;
    }

    // Write the dimensions as the first line
    // file << matrix[0].size() << " " << matrix.size() << std::endl;

    // Write the matrix data
    for (size_t i = 0; i < matrix[0].size(); ++i) {
        for (size_t j = 0; j < matrix.size(); ++j) {
            file << std::setw(15) << std::fixed << std::setprecision(8) << matrix[j][i];
            // Add comma if it's not the last element in the row
            if (j != matrix.size() - 1) {
                file << "  ";
            }
        }
        file << std::endl;
    }

    file.close();
    std::cout << "CSV file has been created successfully." << std::endl;
}

}  // namespace


class FesomCacheValidator final : public multio::MultioTool {
public:  // methods
    FesomCacheValidator(int argc, char** argv);

private:
    void usage(const std::string& tool) const override {
        eckit::Log::info() << std::endl << "Usage: " << tool << " [options]" << std::endl;
        eckit::Log::info() << "EXAMPLE: " << std::endl
                           << "fesom-cache-validator  --cachePath=. --fieldPath=. --outputPath=. "
                              "--cacheFile=file.atlas --fieldFile=inputFields.csv  --outputFile=interpolatedField.csv "
                           << std::endl
                           << "The foramt of the fields must be (every column is a different field):" << std::endl
                           << " ---------------------------------------------" << std::endl
                           << "NROWS NCOLS" << std::endl
                           << "r1c1     r1c2     r1c3     ... r1cNCOLS" << std::endl
                           << "r2c1     r2c2     r2c3     ... r2cNCOLS" << std::endl
                           << "  ...  " << std::endl
                           << "rNROWSc1 rNROWSc2 rNROWSc3 ... rNROWScNCOLS" << std::endl
                           << " ---------------------------------------------" << std::endl
                           << std::endl;
    }

    void init(const eckit::option::CmdArgs& args) override;

    void finish(const eckit::option::CmdArgs&) override;

    void execute(const eckit::option::CmdArgs& args) override;

    int numberOfPositionalArguments() const override { return 0; }
    int minimumPositionalArguments() const override { return 0; }

    std::string cachePath_;
    std::string cacheFile_;
    std::string fieldPath_;
    std::string fieldFile_;
    std::string outputPath_;
    std::string outputFile_;

    std::vector<std::vector<double>> fields_;
};


FesomCacheValidator::FesomCacheValidator(int argc, char** argv) :
    multio::MultioTool{argc, argv},
    cachePath_{"."},
    cacheFile_{"fesomCache.atlas"},
    fieldPath_{"."},
    fieldFile_{"inputFields.csv"},
    outputPath_{"."},
    outputFile_{"interpolated_fields.csv"} {

    options_.push_back(
        new eckit::option::SimpleOption<std::string>("cachePath", "Name of the cache path. Default( \"./\" )"));
    options_.push_back(new eckit::option::SimpleOption<std::string>(
        "cacheFile", "Name of the cache file. Default( \"fesomCache.atlas\" )"));
    options_.push_back(
        new eckit::option::SimpleOption<std::string>("fieldPath", "Path of the field files. Default( \".\" )"));
    options_.push_back(new eckit::option::SimpleOption<std::string>(
        "fieldFile", "Name of the field file. Default( \"inputFields.csv\" )"));


    options_.push_back(new eckit::option::SimpleOption<std::string>(
        "outputPath", "Path of the output file with the interpolated fields. Default( \".\" )"));
    options_.push_back(new eckit::option::SimpleOption<std::string>(
        "outputFile", "Name of the output file Default(\"interpolated_fields.csv\")"));

    return;
}


void FesomCacheValidator::init(const eckit::option::CmdArgs& args) {

    args.get("cachePath", cachePath_);
    args.get("fieldPath", fieldPath_);
    args.get("outputPath", outputPath_);
    args.get("cacheFile", cacheFile_);
    args.get("fieldFile", fieldFile_);
    args.get("outputFile", outputFile_);

    fields_ = readCSV(fieldPath_, fieldFile_);

    eckit::PathName cachePath_tmp{cachePath_};
    ASSERT(cachePath_tmp.exists());
    eckit::PathName cacheFile_tmp{cachePath_ + "/" + cacheFile_};
    ASSERT(cacheFile_tmp.exists());

    eckit::PathName inputPath_tmp{fieldPath_};
    ASSERT(inputPath_tmp.exists());
    eckit::PathName inputFile_tmp{fieldPath_ + "/" + fieldFile_};
    ASSERT(inputFile_tmp.exists());

    eckit::PathName outputPath_tmp{outputPath_};
    outputPath_tmp.mkdir();
}

void FesomCacheValidator::execute(const eckit::option::CmdArgs& args) {

    const std::string iFname{cachePath_ + "/" + cacheFile_};
    const std::string oFname{outputPath_ + "/" + outputFile_};
    Fesom2HEALPix<double> cache(iFname);

    std::vector<std::vector<double>> result;
    std::vector<double> tmp;
    tmp.resize(cache.nOutRows());
    double missing = -999999.0;
    result.resize(0);
    for (size_t i = 0; i < fields_.size(); ++i) {
        std::fill(tmp.begin(), tmp.end(), 0.0);
        cache.interpolate<double, double>(fields_[i].data(), tmp.data(), fields_[i].size(), cache.nOutRows(), missing);
        result.push_back(tmp);
    }

    writeCSV(result, oFname);
};


void FesomCacheValidator::finish(const eckit::option::CmdArgs&) {};

}  // namespace multio::action::interpolateFESOM


int main(int argc, char** argv) {
    multio::action::interpolateFESOM::FesomCacheValidator tool(argc, argv);
    return tool.start();
}

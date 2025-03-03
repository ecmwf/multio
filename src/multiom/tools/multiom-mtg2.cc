/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/// @author Domokos Sarmany
/// @author Simon Smart
/// @author Tiago Quintino

/// @date Oct 2019

#include <stdio.h>
#include <stdlib.h>
#include <fstream>
#include <regex>
#include <unordered_map>
#include <unordered_set>

#include "eccodes.h"
#include "eckit/config/LocalConfiguration.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/filesystem/PathName.h"
#include "eckit/io/FileHandle.h"
#include "eckit/io/MemoryHandle.h"
#include "eckit/io/PeekHandle.h"
#include "eckit/io/StdFile.h"
#include "eckit/log/Log.h"
#include "eckit/maths/Functions.h"
#include "eckit/message/Decoder.h"
#include "eckit/message/Message.h"
#include "eckit/message/Reader.h"
#include "eckit/option/CmdArgs.h"
#include "eckit/option/SimpleOption.h"
#include "metkit/codes/CodesContent.h"
#include "metkit/codes/CodesHandleDeleter.h"
#include "metkit/codes/CodesSplitter.h"
#include "multio/LibMultio.h"
#include "multio/tools/MultioTool.h"
#include "multiom/api/c/api.h"


namespace multiom {
namespace test {

namespace {

using ValueSet = std::unordered_set<std::string>;

using FieldValueMap = std::unordered_map<std::string, ValueSet>;

FieldValueMap parseFieldValueMap(std::string s, int verbosity) {
    const std::string FIELD_DELIM = ";";
    const std::string FIELDVAL_DELIM = "=";
    const std::string VALUES_DELIM = ",";
    FieldValueMap ret;

    size_t posField = 0;
    std::string fieldAndVals;
    do {
        posField = s.find(FIELD_DELIM);
        fieldAndVals = s.substr(0, posField);
        s.erase(0, posField + FIELD_DELIM.length());

        // Separate field from values by split on =
        size_t posFieldVal = fieldAndVals.find(FIELDVAL_DELIM);
        ASSERT(posFieldVal != std::string::npos);
        std::string field = fieldAndVals.substr(0, posFieldVal);
        fieldAndVals.erase(0, posFieldVal + FIELDVAL_DELIM.length());
        // fieldAndVals should contain only values now
        if (verbosity >= 1) {
            std::cout << "Parsed field " << field << std::endl;
        }

        size_t posVals = 0;
        std::string val;
        ValueSet values;
        do {
            posVals = fieldAndVals.find(VALUES_DELIM);
            val = fieldAndVals.substr(0, posVals);
            fieldAndVals.erase(0, posVals + VALUES_DELIM.length());

            if (verbosity >= 1) {
                std::cout << "   parsed value: " << val << std::endl;
            }

            values.insert(val);
        } while (posVals != std::string::npos);

        ret.emplace(std::move(field), std::move(values));
    } while (posField != std::string::npos);

    return ret;
}

bool matches(eckit::message::Message msg, const FieldValueMap& map, int verbosity = 0) {
    for (const auto& fieldVals : map) {
        std::string fieldVal = msg.getString(fieldVals.first);

        bool has = (fieldVals.second.find(fieldVal) != fieldVals.second.end());
        if (has) {
            if (verbosity >= 1) {
                std::cout << "Matched field \"" << fieldVals.first << "\" with value \"" << fieldVal << "\""
                          << std::endl;
            }
            return true;
        }
    }
    return false;
}


}  // namespace

class MultioMMtg2 final : public multio::MultioTool {
public:  // methods
    MultioMMtg2(int argc, char** argv);

private:
    void usage(const std::string& tool) const override {
        eckit::Log::info() << std::endl << "Usage: " << tool << " [options] inputFile outputFile " << std::endl;
        eckit::Log::info() << std::endl
                           << "\tinputFile:\t"
                           << "GRIB file" << std::endl
                           << "\toutputFile:\t"
                           << "output file location" << std::endl;
    }

    void init(const eckit::option::CmdArgs& args) override;

    void finish(const eckit::option::CmdArgs&) override;

    void execute(const eckit::option::CmdArgs& args) override;

    bool subtocExists() const;

    int numberOfPositionalArguments() const override { return 2; }
    int minimumPositionalArguments() const override { return 2; }


    bool copyGrib2Messages_ = true;
    std::string knowledgeRoot_ = "";
    std::string samplePath_ = "";
    std::string encodingFile_ = "";
    std::string mappingFile_ = "";
    long verbosity_ = 0;

    std::optional<FieldValueMap> excludeMap_ = {};
    std::optional<FieldValueMap> filterMap_ = {};
    std::optional<std::string> overwritePacking_ = {};
    // bool encode32_ = false;
    // std::string configPath_ = "";
    // std::string stepRange_ = "";
    // long minStep_ = -1;
    // long maxStep_ = 1000000000;
};

MultioMMtg2::MultioMMtg2(int argc, char** argv) : multio::MultioTool{argc, argv} {
    // TODO
    // - skip or copy grib2 messages (by reading edition key?)
    //   - --all (to explicitly reencode all messages, default is copy)
    // - test all AIFS output (read from fdb)
    // - module on HPC with this tool being deployed
    // x remove environmental variables (use lib info)
    // x load sample FROM_FILE
    // - pass down verbosity to fortran


    options_.push_back(new eckit::option::SimpleOption<bool>("help", "Print help"));
    options_.push_back(new eckit::option::SimpleOption<bool>(
        "all", "If specified also grib2 messages will reencoded instead of copied"));
    options_.push_back(
        new eckit::option::SimpleOption<std::string>("knowledge-root",
                                                     "Path to knowledege root dir containing grib2 sample, encoding "
                                                     "and mapping rules. Default: MULTIO_HOME/share/multiom/49r2v9"));
    options_.push_back(new eckit::option::SimpleOption<std::string>(
        "encoding-rules", "Path to encoding-rules.yaml. Default: KNOWLEDGE_ROOT/encodings/encoding-rules.yaml"));
    options_.push_back(new eckit::option::SimpleOption<std::string>(
        "mapping-rules", "Path to mapping-rules.yaml. Default: KNOWLEDGE_ROOT/mappings/mapping-rules.yaml"));
    options_.push_back(new eckit::option::SimpleOption<std::string>(
        "samples-path", "Path to grib2 samples directory. Default: KNOWLEDGE_ROOT/samples"));
    options_.push_back(new eckit::option::SimpleOption<bool>("verbose", "Sets verbosity to 1"));
    options_.push_back(new eckit::option::SimpleOption<long>("verbosity", "Verbosity level"));
    options_.push_back(new eckit::option::SimpleOption<std::string>(
        "exclude",
        "Keys and values to be excluded. Multiple values are separated by ','. Multiple key-values pairs are separated "
        "by ';'. Example --exclude paramId=130,131,133;levtype=pl,sfc"));
    options_.push_back(new eckit::option::SimpleOption<std::string>(
        "filter",
        "Keys and values to be included. Multiple values are separated by ','. Multiple key-values pairs are separated "
        "by ';'. Example --filter paramId=130,131,133;levtype=pl,sfc"));
    options_.push_back(
        new eckit::option::SimpleOption<std::string>("packing",
                                                     "Enforce a specific packing type, e.g. `ccsds`, `simple`, "
                                                     "`complex`. Note: Avoid the prefix `grid_` or `spectral_`"));
    // options_.push_back(
    //     new eckit::option::SimpleOption<bool>("decode",
    //                                           "Decode messages and pass raw data with metadata through the pipeline "
    //                                           "(with data in double precision)"));
    // options_.push_back(
    //     new eckit::option::SimpleOption<bool>("decodeSingle",
    //                                           "Decode messages and pass raw data with metadata through the pipeline "
    //                                           "(with data in single precision)"));
    // options_.push_back(
    //     new eckit::option::SimpleOption<std::string>("plans", "Path to YAML/JSON file containing plans and
    //     actions."));

    // options_.push_back(
    //     new eckit::option::SimpleOption<std::string>("stepRange", "Range of steps to process (e.g. 0-23)"));
}

void MultioMMtg2::init(const eckit::option::CmdArgs& args) {
    bool verbose = false;
    args.get("verbose", verbose);
    if (verbose) {
        verbosity_ = 1;
    }
    args.get("verbosity", verbosity_);

    bool all = false;
    args.get("all", all);
    copyGrib2Messages_ = !all;

    std::string packing;
    args.get("packing", packing);
    if (!packing.empty()) {
        overwritePacking_ = packing;
    }

    args.get("knowledge-root", knowledgeRoot_);
    if (knowledgeRoot_.empty()) {
        knowledgeRoot_ = multio::LibMultio::instance().libraryHome();
    }
    args.get("samples-path", samplePath_);
    if (samplePath_.empty()) {
        samplePath_ = knowledgeRoot_ + "/share/multiom/samples";
    }
    args.get("encodingFile_", encodingFile_);
    if (encodingFile_.empty()) {
        encodingFile_ = knowledgeRoot_ + "/share/multiom/encodings/encoding-rules.yaml";
    }
    args.get("mappingFile_", mappingFile_);
    if (mappingFile_.empty()) {
        mappingFile_ = knowledgeRoot_ + "/share/multiom/mappings/mapping-rules.yaml";
    }

    if (verbosity_ > 0) {
        std::cout << "knowledge-root: " << knowledgeRoot_ << std::endl;
        std::cout << "samples-path: " << samplePath_ << std::endl;
        std::cout << "encoding-rules: " << encodingFile_ << std::endl;
        std::cout << "mapping-rules: " << mappingFile_ << std::endl;
    }
    setenv("IFS_INSTALL_DIR", knowledgeRoot_.c_str(), 0);

    std::string excludeStr = "";
    args.get("exclude", excludeStr);
    if (!excludeStr.empty()) {
        excludeMap_ = parseFieldValueMap(std::move(excludeStr), verbosity_);
    }

    std::string filterStr = "";
    args.get("filter", filterStr);
    if (!filterStr.empty()) {
        filterMap_ = parseFieldValueMap(std::move(filterStr), verbosity_);
    }

    // if (testSubtoc_) {
    //     std::system(std::string{"rm -rf " + fdbRootPath_.asString() + "/*"}.c_str());
    //     fdbRootPath_.mkdir();
    // }


    // args.get("plans", configPath_);

    // if (args.has("stepRange")) {
    //     args.get("stepRange", stepRange_);
    // }
    // else {
    //     stepRange_ = "0-1000000000";
    // }

    // parseStepRange(stepRange_, minStep_, maxStep_);

    // if (!configPath_.empty()) {
    //     ::setenv("MULTIO_PLANS_FILE", configPath_.c_str(), 1);
    // }
}

void MultioMMtg2::finish(const eckit::option::CmdArgs&) {}

void MultioMMtg2::execute(const eckit::option::CmdArgs& args) {
    using eckit::message::ValueRepresentation;
    eckit::message::Reader reader{args(0)};
    eckit::PathName outPath{args(1)};

    if (outPath.exists()) {
        const int result = remove(((std::string)outPath).c_str());
        if (result == 0) {
            std::cout << "Removed existing file " << outPath << std::endl;
        }
        else {
            std::cerr << "Could not remove existing file " << outPath << std::endl;
            return;
        }
    }
    eckit::FileHandle outputFileHandle{outPath, true};  // overwrite output

    outputFileHandle.openForWrite(0);

    void* optDict = NULL;
    void* encoder = NULL;
    ASSERT(multio_grib2_dict_create(&optDict, "options") == 0);
    ASSERT(multio_grib2_init_options(&optDict) == 0);


    ASSERT(multio_grib2_dict_set(optDict, "samples-path", samplePath_.c_str()) == 0);
    ASSERT(multio_grib2_dict_set(optDict, "encoding-rules", encodingFile_.c_str()) == 0);
    ASSERT(multio_grib2_dict_set(optDict, "mapping-rules", mappingFile_.c_str()) == 0);
    // ASSERT(multio_grib2_dict_set(optDict, "sample", "{MULTIO_INSTALL_DIR}/share/multiom/49r2v9/samples/sample.tmpl")
    // == 0); ASSERT(multio_grib2_dict_set(optDict, "sample", "/MEMFS/samples/GRIB2.tmpl") == 0);

    ASSERT(multio_grib2_encoder_open(optDict, &encoder) == 0);

    eckit::message::Message msg;
    while ((msg = reader.next())) {
        // Extract message from datahandle... we expect it to be a memory handle
        // TODO: Alternative would be to explicitly create a eckit::MemoryHandle and write to it
        std::unique_ptr<eckit::DataHandle> dh{msg.readHandle()};
        eckit::MemoryHandle* mh = reinterpret_cast<eckit::MemoryHandle*>(dh.get());

        ASSERT(mh != NULL);
        std::unique_ptr<codes_handle> inputCodesHandle{codes_handle_new_from_message(NULL, mh->data(), mh->size())};

        dh.reset(nullptr);
        mh = NULL;

        metkit::codes::CodesContent* inputCodesContent = new metkit::codes::CodesContent{inputCodesHandle.get(), false};
        eckit::message::Message inputMsg{inputCodesContent};


        if (excludeMap_) {
            bool ret = matches(msg, *excludeMap_, verbosity_);
            if (ret) {
                if (verbosity_ >= 1) {
                    std::cout << "exclude map  matched... skipping message" << std::endl;
                }
                continue;
            }
        }
        if (filterMap_) {
            bool ret = matches(msg, *filterMap_, verbosity_);
            if (!ret) {
                if (verbosity_ >= 1) {
                    std::cout << "filter map did not match... skipping message" << std::endl;
                }
                continue;
            }
        }

        std::string edition = inputMsg.getString("edition");
        // std::string paramId = inputMsg.getString("paramId");
        if (edition == "2" && copyGrib2Messages_) {
            // Write the message directly
            if (verbosity_ > 0) {
                std::cout << "Copying grib2 message..." << std::endl;
            }
            inputMsg.write(outputFileHandle);
        }
        else {

            // now inputCodesHandle is save to use
            void* marsDict = NULL;
            void* parDict = NULL;
            if (verbosity_ > 0) {
                std::cout << "Extracting metadata..." << std::endl;
            }
            ASSERT(multio_grib2_encoder_extract_metadata(encoder, (void*)inputCodesHandle.get(), &marsDict, &parDict)
                   == 0);

            if (overwritePacking_) {
                if (verbosity_ > 0) {
                    std::cout << "Overwrite packing " << *overwritePacking_ << std::endl;
                }
                ASSERT(multio_grib2_dict_set(marsDict, "packing", overwritePacking_->c_str()) == 0);
            }

            if (verbosity_ > 0) {
                std::cout << "Extracted MARS dict:" << std::endl;
                multio_grib2_dict_to_yaml(marsDict, "stdout");
                std::cout << "Extracted PAR dict:" << std::endl;
                multio_grib2_dict_to_yaml(parDict, "stdout");
            }

            codes_handle* rawOutputCodesHandle = NULL;
            std::unique_ptr<codes_handle> outputCodesHandle = NULL;

            std::vector<double> values;
            inputMsg.getDoubleArray("values", values);

            if (verbosity_ > 0) {
                std::cout << "Encoding with extracted metadata..." << std::endl;
            }
            ASSERT(multio_grib2_encoder_encode64(encoder, marsDict, parDict, values.data(), values.size(),
                                                 (void**)&rawOutputCodesHandle)
                   == 0);
            ASSERT(rawOutputCodesHandle != NULL);

            // Output by writing all to the same binary file
            eckit::message::Message outputMsg{new metkit::codes::CodesContent{rawOutputCodesHandle, true}};
            outputMsg.write(outputFileHandle);
        }
    }

    outputFileHandle.close();
    ASSERT(multio_grib2_encoder_close(&encoder) == 0);
    ASSERT(multio_grib2_dict_destroy(&optDict) == 0);
}

}  // namespace test
}  // namespace multiom

//---------------------------------------------------------------------------------------------------------------

int main(int argc, char** argv) {
    multiom::test::MultioMMtg2 tool(argc, argv);
    return tool.start();
}

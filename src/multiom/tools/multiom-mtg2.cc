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

#include <fstream>
#include <regex>

#include "eckit/config/LocalConfiguration.h"
#include "eckit/exception/Exceptions.h"
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
#include "metkit/codes/CodesSplitter.h"
#include "metkit/codes/CodesContent.h"
#include "metkit/codes/CodesHandleDeleter.h"
#include "multio/tools/MultioTool.h"
#include "multiom/api/c/api.h"
#include "eccodes.h"



namespace multiom {
namespace test {

namespace {
class TempFile {
    const std::string path_;

public:
    TempFile(std::string&& path) : path_{std::move(path)} {}
    ~TempFile() { std::remove(path_.c_str()); }

    const std::string& path() const { return path_; }
};

}  // namespace

class MultioMMtg2 final : public multio::MultioTool {
public:  // methods
    MultioMMtg2(int argc, char** argv);

private:
    void usage(const std::string& tool) const override {
        eckit::Log::info() << std::endl << "Usage: " << tool << " [options] inputFile outputFile " << std::endl;
        eckit::Log::info() << std::endl
                           << "\tinputFile:\t"
                           << "GRIB file" << std::endl;
    }

    void init(const eckit::option::CmdArgs& args) override;

    void finish(const eckit::option::CmdArgs&) override;

    void execute(const eckit::option::CmdArgs& args) override;

    bool subtocExists() const;

    int numberOfPositionalArguments() const override { return 2; }
    int minimumPositionalArguments() const override { return 2; }
    

    // bool testSubtoc_ = false;
    // bool encode32_ = false;
    // std::string configPath_ = "";
    // std::string stepRange_ = "";
    // long minStep_ = -1;
    // long maxStep_ = 1000000000;
};

MultioMMtg2::MultioMMtg2(int argc, char** argv) :
    multio::MultioTool{argc, argv} {
    // options_.push_back(new eckit::option::SimpleOption<bool>("test-subtoc", "Test if subtoc has been created"));
    // options_.push_back(
    //     new eckit::option::SimpleOption<bool>("decode",
    //                                           "Decode messages and pass raw data with metadata through the pipeline "
    //                                           "(with data in double precision)"));
    // options_.push_back(
    //     new eckit::option::SimpleOption<bool>("decodeSingle",
    //                                           "Decode messages and pass raw data with metadata through the pipeline "
    //                                           "(with data in single precision)"));
    // options_.push_back(
    //     new eckit::option::SimpleOption<std::string>("plans", "Path to YAML/JSON file containing plans and actions."));

    // options_.push_back(
    //     new eckit::option::SimpleOption<std::string>("stepRange", "Range of steps to process (e.g. 0-23)"));
}

void MultioMMtg2::init(const eckit::option::CmdArgs& args) {
    // args.get("test-subtoc", testSubtoc_);
    // if (testSubtoc_) {
    //     std::system(std::string{"rm -rf " + fdbRootPath_.asString() + "/*"}.c_str());
    //     fdbRootPath_.mkdir();
    // }
    // args.get("encode32", encode32_); // TODO evalue bitsPerValue ?

    // if (decodeDoubleData_ && decodeSingleData_) {
    //     throw eckit::UserError{"Both double and single precision requested", Here()};
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
    eckit::FileHandle outputFileHandle{args(1), true}; // overwrite output
    
    void* optDict;
    void* encoder;
    ASSERT(multio_grib2_init_options(&optDict) == 0);
    
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
        
        // now inputCodesHandle is save to use
        void* marsDict; 
        void* parDict;
        ASSERT(multio_grib2_encoder_extract_metadata(encoder, (void*) inputCodesHandle.get(), &marsDict, &parDict) == 0);
        
        // How to know if data is encoded with single precision or double precision?
        codes_handle* rawOutputCodesHandle=NULL;
        std::unique_ptr<codes_handle> outputCodesHandle=NULL;
        
        std::vector<double> values;
        metkit::codes::CodesContent inputCodesContent{inputCodesHandle.get(), false};
        eckit::message::Message inputMsg{&inputCodesContent};
        inputMsg.getDoubleArray("values", values);
        
        // ASSERT(multio_grib2_encoder_encode64(encoder, &marsDict, &parDict, values.data(), values.size(), &rawOutputCodesHandle) == 0);
        outputCodesHandle.reset(rawOutputCodesHandle);
        rawOutputCodesHandle=NULL;
        
        
        {
            // Output by writing all to the same binary file
            metkit::codes::CodesContent outputCodesContent{outputCodesHandle.get(), false}; // codesContent does not own outputHandle
            eckit::message::Message outputMsg{&outputCodesContent};
            outputMsg.write(outputFileHandle);
        }
    }
    
    outputFileHandle.close(); 
    ASSERT(multio_grib2_encoder_close(&encoder) == 0);
}

}  // namespace test
}  // namespace multiom

//---------------------------------------------------------------------------------------------------------------

int main(int argc, char** argv) {
    multiom::test::MultioMMtg2 tool(argc, argv);
    return tool.start();
}

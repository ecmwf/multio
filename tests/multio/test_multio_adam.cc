#include <fstream>
#include <unistd.h>
#include <limits>
#include <iomanip>
#include <cstring>
#include <iostream>
#include <string.h>

#include "eckit/filesystem/TmpFile.h"
#include "eckit/testing/Test.h"

#include "multio/api/multio_c.h"
#include "multio/api/multio_c_cpp_utils.h"
#include "multio/message/Metadata.h"

#include "eckit/exception/Exceptions.h"
#include "eckit/io/FileHandle.h"
#include "eckit/log/Log.h"
#include "eckit/mpi/Comm.h"
#include "eckit/option/CmdArgs.h"
#include "eckit/option/SimpleOption.h"

#include "multio/api/multio_c_cpp_utils.h"
#include "multio/tools/MultioTool.h"
#include "multio/util/ConfigurationPath.h"

//#include "TestDataContent.h"
//#include "testHelpers.h"

using multio::util::configuration_file_name;
using multio::util::configuration_path_name;


class MultioReplayAdamCApi final : public multio::MultioTool {
public:
    MultioReplayAdamCApi(int argc, char** argv);

private:
    void usage(const std::string& tool) const override {
        eckit::Log::info() << std::endl << "Usage: " << tool << " [options]" << std::endl;
    }

    void init(const eckit::option::CmdArgs& args) override;

    void finish(const eckit::option::CmdArgs& args) override;

    void execute(const eckit::option::CmdArgs& args) override;

    
    std::string transportType_ = "";
    std::string pathtodata_="";

    size_t clientCount_ = 1;
    std::string replayField_ = "";
    int step_ = 1;
    

    bool singlePrecision_;
};

MultioReplayAdamCApi::MultioReplayAdamCApi(int argc, char** argv) :
    multio::MultioTool(argc, argv), singlePrecision_(false) {
    options_.push_back(new eckit::option::SimpleOption<std::string>("transport", "Type of transport layer"));
    options_.push_back(new eckit::option::SimpleOption<std::string>("path", "Path to data"));
    options_.push_back(new eckit::option::SimpleOption<long>("nbclients", "Number of clients"));
    options_.push_back(new eckit::option::SimpleOption<long>("field", "Name of field to replay"));
    options_.push_back(new eckit::option::SimpleOption<long>("step", "Time counter for the field to replay"));

    std::cout << "Tool Created" << std::endl;
    std::cout << "Command line arguments:" << std::endl;
    for(int i = 0; i < argc; i++){
        std::cout << argv[i] << std::endl;
    }
    std::cout << std::endl;
    return;
}

void MultioReplayAdamCApi::init(const eckit::option::CmdArgs& args) {
    std::cout << "INIT" << std::endl;
    args.get("transport", transportType_);
    args.get("path", pathtodata_);
    args.get("step", step_);

    std::cout << "Transport: " << transportType_ << std::endl;
    std::cout << "Path to data: " << pathtodata_ << std::endl;
    std::cout << "Step: " << step_ << std::endl;
}

void MultioReplayAdamCApi::finish(const eckit::option::CmdArgs&) {
    std::cout << "FINISH" << std::endl;
}

void MultioReplayAdamCApi::execute(const eckit::option::CmdArgs&) {
    std::cout << "EXECUTE" << std::endl;
}

namespace multio {
namespace test{

CASE("Initial Test for version") {
    std::cout << "Running Test" << std::endl;
    const char *version = nullptr;
    multio_version(&version);
    std::cout << "Version " << version << std::endl;
    EXPECT(strcmp(version, "1.9.0")==0);
}

CASE("Test loading config") {
    multio_handle_t* multio_handle = nullptr;
    multio_configurationcontext_t* multio_cc = nullptr;
    auto configPath = configuration_file_name();
    std::cout << "Configuration File: " << configPath.asString().c_str() << std::endl;
    multio_new_configurationcontext_from_filename(&multio_cc, configPath.asString().c_str());
    multio_new_handle(&multio_handle, multio_cc);
    multio_delete_configurationcontext(multio_cc);
    EXPECT(1==1);
}


}
}

int main(int argc, char** argv){
    std::cout << "Start Test!" << std::endl;
    MultioReplayAdamCApi tool(argc, argv);
    tool.start();
    return eckit::testing::run_tests(argc, argv);
}

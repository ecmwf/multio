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
#include "multio/multio_version.h"

#include "eckit/exception/Exceptions.h"
#include "eckit/io/FileHandle.h"
#include "eckit/log/Log.h"
#include "eckit/mpi/Comm.h"
#include "eckit/option/CmdArgs.h"
#include "eckit/option/SimpleOption.h"

#include "multio/api/multio_c_cpp_utils.h"
#include "multio/tools/MultioTool.h"
#include "multio/util/ConfigurationPath.h"

using multio::util::configuration_file_name;
using multio::util::configuration_path_name;


namespace std {
template <> struct default_delete<multio_metadata_t> {
    void operator() (multio_metadata_t* md) { 
        EXPECT(multio_delete_metadata(md) == MULTIO_SUCCESS);
        eckit::Log::error() << "Metadata Object Deleted" << std::endl;
    }
};

template <> struct default_delete<multio_handle_t> {
    void operator() (multio_handle_t* mio) { 
        EXPECT(multio_delete_handle(mio) == MULTIO_SUCCESS);
        eckit::Log::error() << "Handle Object Deleted" << std::endl;
    }
};

template <> struct default_delete<multio_configurationcontext_t> {
    void operator() (multio_configurationcontext_t* cc) { 
        EXPECT(multio_delete_configurationcontext(cc) == MULTIO_SUCCESS);
        eckit::Log::error() << "Configuration Context Object Deleted" << std::endl;
    }
};
}

void test_check(int rc, const char* doc){    
    if(rc != MULTIO_SUCCESS){
        eckit::Log::error() << "Failed to " << doc << std::endl;
        EXPECT(rc == MULTIO_SUCCESS);    
    }
    else{
        eckit::Log::error() << doc << std::endl;
        EXPECT(rc == MULTIO_SUCCESS);
    }
}


class MultioReplayExampleCApi final : public multio::MultioTool {
public:
    MultioReplayExampleCApi(int argc, char** argv);

private:
    void usage(const std::string& tool) const override {
        eckit::Log::info() << std::endl << "Usage: " << tool << " [options]" << std::endl;
    }

    void init(const eckit::option::CmdArgs& args) override;

    void finish(const eckit::option::CmdArgs& args) override;

    void execute(const eckit::option::CmdArgs& args) override;

    
    std::string transportType_ = "mpi";
    std::string pathtodata_="/Users/maaw/multio/tests/multio/test.grib";

    //size_t clientCount_ = 1;
    //std::string replayField_ = "";
    int step_ = 1;
    
    bool singlePrecision_;
};

MultioReplayExampleCApi::MultioReplayExampleCApi(int argc, char** argv) :
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

void MultioReplayExampleCApi::init(const eckit::option::CmdArgs& args) {
    std::cout << "INIT" << std::endl;
    args.get("transport", transportType_);
    args.get("path", pathtodata_);
    args.get("step", step_);

    std::cout << "Transport: " << transportType_ << std::endl;
    std::cout << "Path to data: " << pathtodata_ << std::endl;
    std::cout << "Step: " << step_ << std::endl;
}

void MultioReplayExampleCApi::finish(const eckit::option::CmdArgs&) {
    std::cout << "FINISH" << std::endl;
}

void MultioReplayExampleCApi::execute(const eckit::option::CmdArgs&) {
    std::cout << "EXECUTE" << std::endl;
}

namespace multio {
namespace test{

CASE("Initial Test for version") {
    const char *version = nullptr;
    test_check(multio_version(&version), "Version returned");
    EXPECT(std::strcmp(version, multio_version_str())==0);
}

CASE("Test Multio Initialisation") {
    test_check(multio_initialise(), "Initialise Multio");
    eckit::Main::instance(); // throws if not initialised
}

CASE("Test loading configuration") {
    multio_handle_t* multio_handle = nullptr;
    
    multio_configurationcontext_t* multio_cc = nullptr;

    //int rc;

    test_check(multio_new_configurationcontext(&multio_cc), "Config Created from Environment Path");
    std::unique_ptr<multio_configurationcontext_t> configuration_context_deleter(multio_cc);

    const char *conf_path = "/Users/maaw/multio/tests/multio/";

    test_check(multio_conf_set_path(multio_cc, conf_path), "Configuration Path Changed");

    auto configPath = configuration_path_name();
    auto configFile = configuration_file_name();
    eckit::Log::error() << "Configuration Path: " << configPath.asString().c_str() << std::endl;
    eckit::Log::error() << "Configuration File: " << configFile.asString().c_str() << std::endl;

    test_check(multio_new_configurationcontext_from_filename(&multio_cc, configFile.asString().c_str()), "Configuration Context Created From Filename");

    test_check(multio_new_handle(&multio_handle, multio_cc), "Create Handle");
    std::unique_ptr<multio_handle_t> handle_deleter(multio_handle);

    //test_check(multio_start_server(multio_cc), "Start Server");

    test_check(multio_open_connections(multio_handle), "Open Connections");

    test_check(multio_close_connections(multio_handle), "Close Connections");

    //Dont know if these needed as objects already auto deleted using type traits
    //test_check(multio_delete_configurationcontext(multio_cc), "Delete Configuration object");

    //test_check(multio_delete_handle(multio_handle), "Delete Handle");   
}


CASE("Test creating metadata"){
    int rc;
    multio_metadata_t* md = nullptr;

    test_check(multio_new_metadata(&md), "Create New Metadata");
    std::unique_ptr<multio_metadata_t> multio_deleter(md);
  
    const char* key = "test_int";
    int value=1;

    test_check(multio_metadata_set_int(md, key, value), "Set Int");

    key = "test_long";
    long long_value = 1;
 
    test_check(multio_metadata_set_long(md, key, long_value), "Set Long");
    
    key = "test_long_long";
    long long ll_value = 1;

    test_check(multio_metadata_set_longlong(md, key, ll_value), "Set Long Long");

    key = "test_string";
    const char * s_value = "test_val";

    test_check(multio_metadata_set_string(md, key, s_value), "Set String");   
}

CASE("Test read from grib file"){
    const char* path = "/Users/maaw/multio/tests/multio/test.grib";
    auto field = eckit::PathName{path};

    eckit::Log::error() << field << std::endl;

    eckit::FileHandle infile{field.fullName()};
    size_t bytes = infile.openForRead();
    eckit::Length len = field.size();
    eckit::Buffer buffer(len);
    {
        eckit::AutoClose closer(infile);
        EXPECT(infile.read(buffer.data(), len) == len);
    }

    infile.close();
   
    auto sz = static_cast<int>(buffer.size()) / sizeof(double);
    std::cout << "Size of Buffer: " << sz << std::endl;
}

CASE("Test write field"){

    multio_configurationcontext_t* multio_cc = nullptr;

    test_check(multio_new_configurationcontext(&multio_cc), "Configuration Context Created");
    std::unique_ptr<multio_configurationcontext_t> configuration_context_deleter(multio_cc);

    multio_handle_t* multio_handle = nullptr;
    test_check(multio_new_handle(&multio_handle, multio_cc), "Create New handle");
    EXPECT(multio_handle);
    std::unique_ptr<multio_handle_t> handle_deleter(multio_handle);

    eckit::PathName field{"/Users/maaw/multio/tests/multio/test.grib"};
    eckit::Length len = field.size();
    eckit::Buffer buffer(len);

    eckit::FileHandle infile{field};
    infile.openForRead();
    {
        eckit::AutoClose closer(infile);
        EXPECT(infile.read(buffer.data(), len) == len);
    }
   
    auto sz = static_cast<int>(buffer.size()) / sizeof(double);
    std::cout << "Size of Buffer: " << sz << std::endl;

    multio_metadata_t* md = nullptr;
    test_check(multio_new_metadata(&md), "Create New Metadata Object");
    std::unique_ptr<multio_metadata_t> multio_deleter(md);

    test_check(multio_metadata_set_string(md, "category", "test_data"), "Set String");
    test_check(multio_metadata_set_int(md, "globalSize", sz), "Set Int");
    test_check(multio_metadata_set_int(md, "level", 1), "Set Int");
    test_check(multio_metadata_set_int(md, "step", 1), "Set Int");

    test_check(multio_metadata_set_double(md, "missingValue", 0.0), "Set Doubel");
    test_check(multio_metadata_set_bool(md, "bitmapPresent", false), "Set bool");
    test_check(multio_metadata_set_int(md, "bitsPerValue", 16), "Set Int");

    test_check(multio_metadata_set_bool(md, "toAllServers", false), "Set Bool");

    // Overwrite these fields in the existing metadata object
    test_check(multio_metadata_set_string(md, "name", "test"), "Set String)");

    test_check(multio_write_field(multio_handle, md, reinterpret_cast<const double*>(buffer.data()), sz), "Write Field");

}
}
}

int main(int argc, char** argv){
    std::cout << "Start Test!" << std::endl;
    MultioReplayExampleCApi tool(argc, argv);
    tool.start();
    return eckit::testing::run_tests(argc, argv);
}

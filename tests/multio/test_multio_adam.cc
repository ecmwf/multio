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
    void operator() (multio_metadata_t* m) { 
        EXPECT(multio_delete_metadata(m) == MULTIO_SUCCESS);
    }
};

template <> struct default_delete<multio_handle_t> {
    void operator() (multio_handle_t* m) { 
        EXPECT(multio_delete_handle(m) == MULTIO_SUCCESS);
    }
};
}

/***


namespace std {


    template <typename T>
    struct default_delete {
        void operator() (const T* p) { delete p; }
    }


    template <typename T>
    struct unique_ptr {
        unique_ptr(T* p) : ptr_(p) {}
        unique_ptr(unique_ptr<T>&& rhs) { ... }
        ~unique_ptr() {
            default_delete<T>{}(p);
        }
        const T& operator*() const { return *ptr_; }
        T& operator*() { return *ptr_; }
    private:
        T* ptr_;
    };


    template <>
    struct default_delete<multio_metadata_t> {
        void operator() (const multio_metadata_t* p) { EXPECT(multio_metadata_delete(p) == MULTIO_SUCCESS); }
    };
}


multio_metadata_t* md;
EXPECT(multio_new_metadata(&md) == MULTIO_SUCCESS);
unique_ptr<multio_metdata_t> del(md);
...



int abcd = 1234;
std::vector<int> v {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};


eckit::Log::info() << "My value: " << abcd << std::endl;

template <typename T>
struct eckit_vector_print_condense {
    static bool condense = false;
}


template <> struct eckit_vector_print_condense<int> {
    static bool condense = true;
}



std::ostream& operator<<(std::ostream& s, const std::vector<T>& v) {

    if (eckit_vector_print_condense<T>:±condense) {}
    [1, 2, ..., 10]
    ...
    return s;
}





{
    {
        // std::unique_ptr<MY_TYPE> p{new MY_TYPE(a, b, c)};
        MY_TYPE* p = new MY_TYPE(a, b, c)
        
        try {
            ...
            ...
            throw Exception();
            ...
            ...
        } catch (...) {
            delete p;
            throw;
        }

        delete p;
    }
}

***/



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
    EXPECT(multio_version(&version) == MULTIO_SUCCESS);
    EXPECT(std::strcmp(version, multio_version_str())==0);
}

CASE("Test Multio Initialisation") {
    EXPECT(multio_initialise() == MULTIO_SUCCESS);
    eckit::Main::instance(); // throws if not initialised
}

CASE("Test loading config") {
    multio_handle_t* multio_handle = nullptr;
    multio_configurationcontext_t* multio_cc = nullptr;
    int rc;

    rc = multio_new_configurationcontext(&multio_cc);
    if(rc==MULTIO_SUCCESS){
        std::cout << "Config created from environment path" << std::endl;
    }
    else{
        std::cout << "Config NOT created from environment path" << std::endl;
    }

    const char *conf_path = "/Users/maaw/multio/tests/multio/";
    //std::cout << conf_path << std::endl;

    rc = multio_conf_set_path(multio_cc, conf_path);
    if(rc==MULTIO_SUCCESS){
        std::cout << "Config Path set" << std::endl;
    }
    else{
        std::cout << "Config Path NOT set" << std::endl;
    }

    auto configPath = configuration_path_name();
    auto configFile = configuration_file_name();
    std::cout << "Configuration Path: " << configPath.asString().c_str() << std::endl;
    std::cout << "Configuration File: " << configFile.asString().c_str() << std::endl;
    rc = multio_new_configurationcontext_from_filename(&multio_cc, configFile.asString().c_str());
    if(rc==MULTIO_SUCCESS){
        std::cout << "Config created from Filename" << std::endl;
    }
    else{
        std::cout << "Config NOT created from filename" << std::endl;
    }

    rc = multio_new_handle(&multio_handle, multio_cc);
    if(rc==MULTIO_SUCCESS){
        std::cout << "Config handle" << std::endl;
    }
    else{
        std::cout << "Config NOT handle" << std::endl;
    }

    rc = multio_start_server(multio_cc);
    if(rc==MULTIO_SUCCESS){
        std::cout << "Server started" << std::endl;
    }
    else{
        std::cout << "Server NOT started" << std::endl;
    }

    rc = multio_open_connections(multio_handle);
    if(rc==MULTIO_SUCCESS){
        std::cout << "Connections opened" << std::endl;
    }
    else{
        std::cout << "Connections NOT opened" << std::endl;
    }

    rc = multio_close_connections(multio_handle);
    if(rc==MULTIO_SUCCESS){
        std::cout << "Connections closed" << std::endl;
    }
    else{
        std::cout << "Connections NOT closed" << std::endl;
    }

    rc = multio_delete_configurationcontext(multio_cc);
    if(rc==MULTIO_SUCCESS){
        std::cout << "Config deleted" << std::endl;
    }
    else{
        std::cout << "Config NOT deleted" << std::endl;
    }

    rc = multio_delete_handle(multio_handle);
    if(rc==MULTIO_SUCCESS){
        std::cout << "Handle deleted" << std::endl;
    }
    else{
        std::cout << "Handle NOT deleted" << std::endl;
    }
    
    EXPECT(rc==MULTIO_SUCCESS);
}


CASE("Test creating metadata"){
    int rc;
    multio_metadata_t* md = nullptr;
    EXPECT(multio_new_metadata(&md) == MULTIO_SUCCESS);
    EXPECT(md != nullptr);

    std::unique_ptr<multio_metadata_t> multio_deleter(md);
    
    const char* key = "test_int";
    int value=1;

    multio_metadata_set_int(md, key, value);

    key = "test_long";
    long long_value = 1;

    //EXPECT(...);
    
    multio_metadata_set_long(md, key, long_value);
    
    key = "test_long_long";
    long long ll_value = 1;

    multio_metadata_set_longlong(md, key, ll_value);

    key = "test_string";
    const char * s_value = "test_val";

    multio_metadata_set_string(md, key, s_value);   
 
}

CASE("Test read from grib file"){
    const char* path = "/Users/maaw/multio/tests/multio/test.grib";
    auto field = eckit::PathName{path};
    int rc;

    std::cout << field << std::endl;

    eckit::FileHandle infile{field.fullName()};
    size_t bytes = infile.openForRead();

    eckit::Buffer buffer(bytes);
    infile.read(buffer.data(), bytes);

    infile.close();
   
    auto sz = static_cast<int>(buffer.size()) / sizeof(double);
    std::cout << "Size of Buffer: " << sz << std::endl;
    
    EXPECT(1==1);

}

CASE("Test write field"){

    multio_configurationcontext_t* multio_cc = nullptr;
    int rc;

    rc = multio_new_configurationcontext(&multio_cc);
    const char *conf_path = "/Users/maaw/multio/tests/multio/";
    std::cout << conf_path << std::endl;

    rc = multio_conf_set_path(multio_cc, conf_path);
    if(rc==MULTIO_SUCCESS){
        std::cout << "Config Path set" << std::endl;
    }
    else{
        std::cout << "Config Path NOT set" << std::endl;
    }

    multio_handle_t* multio_handle = nullptr;
    EXPECT(multio_new_handle(&multio_handle, multio_cc) == MULTIO_SUCCESS);
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
    multio_new_metadata(&md);

    std::unique_ptr<multio_metadata_t> multio_deleter(md);

    multio_metadata_set_string(md, "category", "test_data");
    multio_metadata_set_int(md, "globalSize", sz);
    multio_metadata_set_int(md, "level", 1);
    multio_metadata_set_int(md, "step", 1);

    multio_metadata_set_double(md, "missingValue", 0.0);
    multio_metadata_set_bool(md, "bitmapPresent", false);
    multio_metadata_set_int(md, "bitsPerValue", 16);

    multio_metadata_set_bool(md, "toAllServers", false);

    // Overwrite these fields in the existing metadata object
    multio_metadata_set_string(md, "name", "test");

    rc = multio_write_field(multio_handle, md, reinterpret_cast<const double*>(buffer.data()), sz);
    
    EXPECT(rc==MULTIO_SUCCESS);

}
}
}

int main(int argc, char** argv){
    std::cout << "Start Test!" << std::endl;
    MultioReplayExampleCApi tool(argc, argv);
    tool.start();
    return eckit::testing::run_tests(argc, argv);
}

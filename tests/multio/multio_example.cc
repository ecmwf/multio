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

using multio::util::configuration_file_name;
using multio::util::configuration_path_name;

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

    void initClient();
    void runClient();

    eckit::Buffer readFields();
    void writeFields();

    
    std::string transportType_ = "mpi";
    std::string pathtodata_="/Users/maaw/multio/tests/multio/test.grib";

    //size_t clientCount_ = 1;
    //std::string replayField_ = "";
    int step_ = 1;
    
    bool singlePrecision_;

    multio_handle_t* multio_handle = nullptr;
};

MultioReplayExampleCApi::MultioReplayExampleCApi(int argc, char** argv) :
    multio::MultioTool(argc, argv), singlePrecision_(false) {
    options_.push_back(new eckit::option::SimpleOption<std::string>("transport", "Type of transport layer"));
    options_.push_back(new eckit::option::SimpleOption<std::string>("path", "Path to data"));
    options_.push_back(new eckit::option::SimpleOption<long>("nbclients", "Number of clients"));
    options_.push_back(new eckit::option::SimpleOption<long>("field", "Name of field to replay"));
    options_.push_back(new eckit::option::SimpleOption<long>("step", "Time counter for the field to replay"));

    eckit::Log::info() << "Tool Created" << std::endl;
    eckit::Log::info() << "Command line arguments:" << std::endl;
    for(int i = 0; i < argc; i++){
        eckit::Log::info() << argv[i] << std::endl;
    }
    return;
}

void MultioReplayExampleCApi::init(const eckit::option::CmdArgs& args) {
    eckit::Log::info() << "INIT" << std::endl;
    args.get("transport", transportType_);
    args.get("path", pathtodata_);
    args.get("step", step_);

    eckit::Log::info() << "Transport: " << transportType_ << std::endl;
    eckit::Log::info() << "Path to data: " << pathtodata_ << std::endl;
    eckit::Log::info() << "Step: " << step_ << std::endl;

    initClient();
}

void MultioReplayExampleCApi::finish(const eckit::option::CmdArgs&) {
    multio_delete_handle(multio_handle);
    eckit::Log::info() << "FINISH" << std::endl;
}

void MultioReplayExampleCApi::execute(const eckit::option::CmdArgs&) {
    eckit::Log::info() << "EXECUTE" << std::endl;

    runClient();
}

void MultioReplayExampleCApi::initClient() {
    eckit::Log::info() << "INIT CLIENT" << std::endl;
    multio_configurationcontext_t* multio_cc = nullptr;
    auto configPath = configuration_file_name();
    multio_new_configurationcontext_from_filename(&multio_cc, configPath.asString().c_str());
    multio_new_handle(&multio_handle, multio_cc);
    multio_delete_configurationcontext(multio_cc);
}

void MultioReplayExampleCApi::runClient() {
    eckit::Log::info() << "RUN CLIENT" << std::endl;

    multio_open_connections(multio_handle);

    writeFields();

    multio_close_connections(multio_handle);
}

eckit::Buffer MultioReplayExampleCApi::readFields() {
    const char *conf_path = pathtodata_.c_str();

    auto field = eckit::PathName{conf_path};

    eckit::Log::error() << field << std::endl;

    eckit::FileHandle infile{field.fullName()};
    size_t bytes = infile.openForRead();
    eckit::Length len = field.size();
    eckit::Buffer buffer(len);
    {
        eckit::AutoClose closer(infile);
        EXPECT(infile.read(buffer.data(), len) == len);
    }

    //infile.close();
   
    return buffer;
}

void MultioReplayExampleCApi::writeFields() {
    auto buffer = readFields();
    auto sz = static_cast<int>(buffer.size()) / sizeof(double);
    std::cout << "Size of Buffer: " << sz << std::endl;

    multio_metadata_t* md = nullptr;

    multio_new_metadata(&md);
  
    const char* key = "test_int";
    int value=1;
    multio_metadata_set_int(md, key, value);

    key = "test_long";
    long long_value = 1;

    multio_metadata_set_long(md, key, long_value);
    
    key = "test_long_long";
    long long ll_value = 1;
    multio_metadata_set_longlong(md, key, ll_value);

    key = "test_string";
    const char * s_value = "test_val";
    multio_metadata_set_string(md, key, s_value);

    multio_write_field(multio_handle, md, reinterpret_cast<const double*>(buffer.data()), sz);

    multio_delete_metadata(md);
}

int main(int argc, char** argv){
    std::cout << "Start Test!" << std::endl;
    MultioReplayExampleCApi tool(argc, argv);
    return tool.start();
}

#include <string.h>
#include <unistd.h>
#include <cstring>
#include <filesystem>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <limits>

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
template <>
struct default_delete<multio_metadata_t> {
    void operator()(multio_metadata_t* md) {
        EXPECT(multio_delete_metadata(md) == MULTIO_SUCCESS);
        eckit::Log::error() << "Metadata Object Deleted" << std::endl;
    }
};

template <>
struct default_delete<multio_handle_t> {
    void operator()(multio_handle_t* mio) {
        EXPECT(multio_delete_handle(mio) == MULTIO_SUCCESS);
        eckit::Log::error() << "Handle Object Deleted" << std::endl;
    }
};

template <>
struct default_delete<multio_configurationcontext_t> {
    void operator()(multio_configurationcontext_t* cc) {
        EXPECT(multio_delete_configurationcontext(cc) == MULTIO_SUCCESS);
        eckit::Log::error() << "Configuration Context Object Deleted" << std::endl;
    }
};
}  // namespace std

void test_check(int rc, const char* doc) {
    if (rc != MULTIO_SUCCESS) {
        eckit::Log::error() << "Failed to " << doc << std::endl;
    }
    EXPECT(rc == MULTIO_SUCCESS);
}


namespace multio {
namespace test {

CASE("Test Multio Initialisation") {
    test_check(multio_initialise(), "Initialise Multio");
    eckit::Main::instance();  // throws if not initialised
}

CASE("Initial Test for version") {
    const char* version = nullptr;
    test_check(multio_version(&version), "Version returned");
    EXPECT(std::strcmp(version, multio_version_str()) == 0);
}


CASE("Test loading configuration") {

    multio_configurationcontext_t* multio_cc = nullptr;

    test_check(multio_new_configurationcontext(&multio_cc), "Config Created from Environment Path");
    std::unique_ptr<multio_configurationcontext_t> configuration_context_deleter(multio_cc);

    auto configFile = configuration_file_name();
    const char* conf_path = configFile.localPath();

    test_check(multio_conf_set_path(multio_cc, conf_path), "Configuration Path Changed");

    auto configPath = configuration_path_name();

    test_check(multio_new_configurationcontext_from_filename(&multio_cc, configFile.asString().c_str()),
               "Configuration Context Created From Filename");

    multio_handle_t* multio_handle = nullptr;
    test_check(multio_new_handle(&multio_handle, multio_cc), "Create Handle");
    std::unique_ptr<multio_handle_t> handle_deleter(multio_handle);

    test_check(multio_open_connections(multio_handle), "Open Connections");

    // test_check(multio_start_server(multio_cc), "Start Server");

    test_check(multio_close_connections(multio_handle), "Close Connections");
}

CASE("Test MPI Functionality") {
    multio_handle_t* multio_handle = nullptr;

    multio_configurationcontext_t* multio_cc = nullptr;

    test_check(multio_new_configurationcontext(&multio_cc), "Config Created from Environment Path");
    std::unique_ptr<multio_configurationcontext_t> configuration_context_deleter(multio_cc);

    bool allow = true;
    test_check(multio_conf_mpi_allow_world_default_comm(multio_cc, allow), "Allow World Default Comm");

    int parent_comm = 0;
    test_check(multio_conf_mpi_parent_comm(multio_cc, parent_comm),
               "Set MPI specific initalization parameters for parent Comm");

    int* client_comm = 0;
    test_check(multio_conf_mpi_return_client_comm(multio_cc, client_comm),
               "Set MPI specific initalization parameters for client Comm");

    int* server_comm = 0;
    test_check(multio_conf_mpi_return_server_comm(multio_cc, server_comm),
               "Set MPI specific initalization parameters for server Comm");

    const char* client_id = "test_client";
    test_check(multio_conf_mpi_client_id(multio_cc, client_id), "Set MPI client ID");
}


CASE("Test creating metadata") {
    multio_metadata_t* md = nullptr;

    test_check(multio_new_metadata(&md), "Create New Metadata");
    std::unique_ptr<multio_metadata_t> multio_deleter(md);

    const char* key = "test_int";
    int value = 1;

    test_check(multio_metadata_set_int(md, key, value), "Set Int");

    key = "test_long";
    long long_value = 1;

    test_check(multio_metadata_set_long(md, key, long_value), "Set Long");

    key = "test_long_long";
    long long ll_value = 1;

    test_check(multio_metadata_set_longlong(md, key, ll_value), "Set Long Long");

    key = "test_string";
    const char* s_value = "test_val";

    test_check(multio_metadata_set_string(md, key, s_value), "Set String");
}


CASE("Test write field") {
    multio_configurationcontext_t* multio_cc = nullptr;

    test_check(multio_new_configurationcontext(&multio_cc), "Configuration Context Created");
    std::unique_ptr<multio_configurationcontext_t> configuration_context_deleter(multio_cc);

    multio_handle_t* multio_handle = nullptr;
    test_check(multio_new_handle(&multio_handle, multio_cc), "Create New handle");
    EXPECT(multio_handle);
    std::unique_ptr<multio_handle_t> handle_deleter(multio_handle);

    const char* files[2] = {"test.grib", "test2.grib"};

    for (const char* file : files) {
        auto field = configuration_path_name() / file;
        eckit::Length len = field.size();
        eckit::Buffer buffer(len);

        eckit::FileHandle infile{field};
        infile.openForRead();
        {
            eckit::AutoClose closer(infile);
            EXPECT(infile.read(buffer.data(), len) == len);
        }

        multio_metadata_t* md = nullptr;
        test_check(multio_new_metadata(&md), "Create New Metadata Object");
        std::unique_ptr<multio_metadata_t> multio_deleter(md);

        test_check(multio_metadata_set_string(md, "category", file), "Set String");
        test_check(multio_metadata_set_int(md, "globalSize", len), "Set Int");
        test_check(multio_metadata_set_int(md, "level", 1), "Set Int");
        test_check(multio_metadata_set_int(md, "step", 1), "Set Int");

        test_check(multio_metadata_set_double(md, "missingValue", 0.0), "Set Double");
        test_check(multio_metadata_set_bool(md, "bitmapPresent", false), "Set bool");
        test_check(multio_metadata_set_int(md, "bitsPerValue", 16), "Set Int");

        test_check(multio_metadata_set_bool(md, "toAllServers", false), "Set Bool");

        // Overwrite these fields in the existing metadata object
        test_check(multio_metadata_set_string(md, "name", "test"), "Set String");

        test_check(multio_write_field(multio_handle, md, reinterpret_cast<const double*>(buffer.data()), len),
                   "Write Field");

        test_check(multio_write_step_complete(multio_handle, md), "Field Written");
    }

    auto file_name = configuration_path_name() / "../../build/tests/multio/testWriteOutput.grib";
    std::cout << file_name.localPath() << std::endl;
    EXPECT(std::filesystem::exists(file_name.localPath()));
}
}  // namespace test
}  // namespace multio

int main(int argc, char** argv) {
    // Disable ECKIT::initialisation by test framework as initialised by first test
    constexpr bool init_eckit = false;
    return eckit::testing::run_tests(argc, argv, init_eckit);
}

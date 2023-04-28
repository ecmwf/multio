/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

// @author Philipp Geier


#include <unistd.h>
#include <cstring>
#include <limits>

#include "eckit/filesystem/TmpFile.h"
#include "eckit/io/FileHandle.h"
#include "eckit/testing/Test.h"

#include "multio/api/multio_c.h"
#include "multio/api/multio_c_cpp_utils.h"
#include "multio/message/Metadata.h"
#include "multio/multio_version.h"

#include "TestDataContent.h"
#include "TestHelpers.h"

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
struct default_delete<multio_configuration_t> {
    void operator()(multio_configuration_t* cc) {
        EXPECT(multio_delete_configuration(cc) == MULTIO_SUCCESS);
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

// TODO: Can we keep this?
// Copied from https://en.cppreference.com/w/cpp/types/numeric_limits/epsilon
template <class T>
typename std::enable_if<!std::numeric_limits<T>::is_integer, bool>::type almost_equal(T x, T y, int ulp) {
    // the machine epsilon has to be scaled to the magnitude of the values used
    // and multiplied by the desired precision in ULPs (units in the last place)
    return std::fabs(x - y) <= std::numeric_limits<T>::epsilon() * std::fabs(x + y) * ulp
        // unless the result is subnormal
        || std::fabs(x - y) < std::numeric_limits<T>::min();
}

static std::string expectedMPIError("No communicator \"multio\" and no default given.");

CASE("Test Multio Initialisation") {
    test_check(multio_initialise(), "Initialise Multio");
    eckit::Main::instance();  // throws if not initialised
}

CASE("Initial Test for version") {
    const char* version = nullptr;
    test_check(multio_version(&version), "Version returned");
    EXPECT(std::strcmp(version, multio_version_str()) == 0);
}

CASE("Try Create handle with wrong configuration path") {
    multio_configuration_t* cc = nullptr;
    int err;
    err = multio_new_configuration_from_filename(&cc, "I_AM_NOT_HERE/multio/config/multio-server.yaml");
    std::unique_ptr<multio_configuration_t> configuration_context_deleter(cc);
    std::string errStr(multio_error_string(err));
    // std::cout << "new handle err" << err << " Message: " << errStr << std::endl;
    EXPECT(err == MULTIO_ERROR_ECKIT_EXCEPTION);
    EXPECT(errStr.rfind("Cannot open I_AM_NOT_HERE/multio/config/multio-server.yaml  (No such file or directory)")
           != std::string::npos);
}

CASE("Create handle with default configuration without MPI splitting") {
    multio_configuration_t* cc = nullptr;
    multio_handle_t* mdp = nullptr;
    int err;
    err = multio_new_configuration(&cc);
    std::unique_ptr<multio_configuration_t> configuration_context_deleter(cc);
    EXPECT(err == MULTIO_SUCCESS);
    err = multio_conf_mpi_allow_world_default_comm(cc, false);
    EXPECT(err == MULTIO_SUCCESS);
    err = multio_new_handle(&mdp, cc);
    std::unique_ptr<multio_handle_t> handle_deleter(mdp);
    std::string errStr(multio_error_string(err));
    // std::cout << "new handle err" << err << " Message: " << errStr << std::endl;
    EXPECT(err == MULTIO_ERROR_ECKIT_EXCEPTION);
    EXPECT(errStr.rfind(expectedMPIError) != std::string::npos);
}

CASE("Create handle with default configuration through nullptr configuration path without MPI splitting") {
    multio_configuration_t* cc = nullptr;
    multio_handle_t* mdp = nullptr;
    int err;
    err = multio_new_configuration_from_filename(&cc, nullptr);
    std::unique_ptr<multio_configuration_t> configuration_context_deleter(cc);
    EXPECT(err == MULTIO_SUCCESS);
    err = multio_conf_mpi_allow_world_default_comm(cc, false);
    EXPECT(err == MULTIO_SUCCESS);
    err = multio_new_handle(&mdp, cc);
    std::unique_ptr<multio_handle_t> handle_deleter(mdp);
    std::string errStr(multio_error_string(err));
    // std::cout << "new handle err" << err << " Message: " << errStr << std::endl;
    EXPECT(err == MULTIO_ERROR_ECKIT_EXCEPTION);
    EXPECT(errStr.rfind(expectedMPIError) != std::string::npos);
}


CASE("Create handle with configuration path without MPI splitting") {
    multio_configuration_t* cc = nullptr;
    multio_handle_t* mdp = nullptr;
    int err;
    const char* env_config_path = std::getenv("MULTIO_SERVER_CONFIG_PATH");
    EXPECT(env_config_path);
    std::ostringstream oss;
    oss << env_config_path << "/multio-server.yaml";
    std::string path = oss.str();
    err = multio_new_configuration_from_filename(&cc, path.c_str());
    std::unique_ptr<multio_configuration_t> configuration_context_deleter(cc);
    EXPECT(err == MULTIO_SUCCESS);
    err = multio_conf_mpi_allow_world_default_comm(cc, false);
    EXPECT(err == MULTIO_SUCCESS);
    err = multio_new_handle(&mdp, cc);
    std::unique_ptr<multio_handle_t> handle_deleter(mdp);
    std::string errStr(multio_error_string(err));
    // std::cout << "new handle err" << err << " Message: " << errStr << std::endl;
    EXPECT(err == MULTIO_ERROR_ECKIT_EXCEPTION);
    EXPECT(errStr.rfind(expectedMPIError) != std::string::npos);
}

// CASE("Start server with default configuration & unknown server name") {
//     multio_configuration_t* cc = nullptr;
//     int err;
//     err = multio_new_configuration(&cc);
//     EXPECT(err == MULTIO_SUCCESS);
//     err = multio_conf_mpi_allow_world_default_comm(cc, false);
//     EXPECT(err == MULTIO_SUCCESS);
//     err = multio_start_server(cc, "I_AM_NOT_HERE");
//     std::string errStr(multio_error_string(err));
//     // std::cout << "new handle err" << err << " Message: " << errStr << std::endl;
//     EXPECT(err == MULTIO_ERROR_ECKIT_EXCEPTION);
//     EXPECT(errStr.rfind("Configuration 'I_AM_NOT_HERE' not found") != std::string::npos);
//     multio_delete_configuration(cc);
// }

CASE("Start server with default configuration") {
    multio_configuration_t* cc = nullptr;
    int err;
    err = multio_new_configuration(&cc);
    std::unique_ptr<multio_configuration_t> configuration_context_deleter(cc);
    EXPECT(err == MULTIO_SUCCESS);
    err = multio_conf_mpi_allow_world_default_comm(cc, false);
    EXPECT(err == MULTIO_SUCCESS);
    err = multio_start_server(cc);
    std::string errStr(multio_error_string(err));
    // std::cout << "new handle err" << err << " Message: " << errStr << std::endl;
    EXPECT(err == MULTIO_ERROR_ECKIT_EXCEPTION);
    EXPECT(errStr.rfind(expectedMPIError) != std::string::npos);
}

CASE("Test loading configuration") {

    multio_configuration_t* multio_cc = nullptr;

    test_check(multio_new_configuration(&multio_cc), "Config Created from Environment Path");
    std::unique_ptr<multio_configuration_t> configuration_context_deleter(multio_cc);

    auto configFile = configuration_file_name();
    const char* conf_path = configFile.localPath();

    test_check(multio_conf_set_path(multio_cc, conf_path), "Configuration Path Changed");

    auto configPath = configuration_path_name() / "testPlan.yaml";

    test_check(multio_new_configuration_from_filename(&multio_cc, configPath.localPath()),
               "Configuration Context Created From Filename");

    multio_handle_t* multio_handle = nullptr;
    test_check(multio_new_handle(&multio_handle, multio_cc), "Create Handle");
    std::unique_ptr<multio_handle_t> handle_deleter(multio_handle);

    test_check(multio_open_connections(multio_handle), "Open Connections");

    test_check(multio_close_connections(multio_handle), "Close Connections");
}

CASE("Metadata is created and delected sucessfully") {
    multio_metadata_t* mdp = nullptr;
    int err;
    err = multio_new_metadata(&mdp);
    EXPECT(err == MULTIO_SUCCESS);
    err = multio_delete_metadata(mdp);
    EXPECT(err == MULTIO_SUCCESS);
}

CASE("Metadata can set values") {
    using multio::message::Metadata;
    multio_metadata_t* mdp = nullptr;
    int err;
    err = multio_new_metadata(&mdp);
    std::unique_ptr<multio_metadata_t> multio_deleter(mdp);
    EXPECT(err == MULTIO_SUCCESS);

    err = multio_metadata_set_string(mdp, "stringValue", "testString");
    EXPECT(err == MULTIO_SUCCESS);
    err = multio_metadata_set_string(mdp, "stringEmptyValue", "");
    EXPECT(err == MULTIO_SUCCESS);
    err = multio_metadata_set_bool(mdp, "boolMinValue", false);
    EXPECT(err == MULTIO_SUCCESS);
    err = multio_metadata_set_bool(mdp, "boolMaxValue", true);
    EXPECT(err == MULTIO_SUCCESS);
    err = multio_metadata_set_int(mdp, "intMinValue", std::numeric_limits<int>::min());
    EXPECT(err == MULTIO_SUCCESS);
    err = multio_metadata_set_int(mdp, "intMaxValue", std::numeric_limits<int>::max());
    EXPECT(err == MULTIO_SUCCESS);
    err = multio_metadata_set_long(mdp, "longMinValue", std::numeric_limits<long>::min());
    EXPECT(err == MULTIO_SUCCESS);
    err = multio_metadata_set_long(mdp, "longMaxValue", std::numeric_limits<long>::max());
    EXPECT(err == MULTIO_SUCCESS);
    err = multio_metadata_set_longlong(mdp, "longlongMinValue", std::numeric_limits<long long>::min());
    EXPECT(err == MULTIO_SUCCESS);
    err = multio_metadata_set_longlong(mdp, "longlongMaxValue", std::numeric_limits<long long>::max());
    EXPECT(err == MULTIO_SUCCESS);
    err = multio_metadata_set_float(mdp, "floatLowestValue",
                                    std::numeric_limits<float>::lowest() + std::numeric_limits<float>::epsilon());
    EXPECT(err == MULTIO_SUCCESS);
    err = multio_metadata_set_float(mdp, "floatMinValue", std::numeric_limits<float>::min() * 2);
    EXPECT(err == MULTIO_SUCCESS);
    err = multio_metadata_set_float(mdp, "floatMaxValue",
                                    std::numeric_limits<float>::max() - std::numeric_limits<float>::epsilon());
    EXPECT(err == MULTIO_SUCCESS);
    err = multio_metadata_set_double(mdp, "doubleLowestValue",
                                     std::numeric_limits<double>::lowest() + std::numeric_limits<double>::epsilon());
    EXPECT(err == MULTIO_SUCCESS);
    err = multio_metadata_set_double(mdp, "doubleMinValue", std::numeric_limits<double>::min() * 2);
    EXPECT(err == MULTIO_SUCCESS);
    err = multio_metadata_set_double(mdp, "doubleMaxValue",
                                     std::numeric_limits<double>::max() - std::numeric_limits<double>::epsilon());
    EXPECT(err == MULTIO_SUCCESS);

    Metadata* md_pCpp = multio_from_c(mdp);

    EXPECT(md_pCpp->getString("stringValue").compare("testString") == 0);
    EXPECT(md_pCpp->getString("stringEmptyValue").compare("") == 0);
    EXPECT(md_pCpp->getBool("boolMinValue") == false);
    EXPECT(md_pCpp->getBool("boolMaxValue") == true);
    EXPECT(md_pCpp->getInt("intMinValue") == std::numeric_limits<int>::min());
    EXPECT(md_pCpp->getInt("intMaxValue") == std::numeric_limits<int>::max());
    EXPECT(md_pCpp->getLong("longMinValue") == std::numeric_limits<long>::min());
    EXPECT(md_pCpp->getLong("longMaxValue") == std::numeric_limits<long>::max());
    long long expctLongLongMin;
    long long expctLongLongMax;
    md_pCpp->get("longlongMinValue", expctLongLongMin);
    md_pCpp->get("longlongMaxValue", expctLongLongMax);
    EXPECT(expctLongLongMin == std::numeric_limits<long long>::min());
    EXPECT(expctLongLongMax == std::numeric_limits<long long>::max());
    EXPECT(md_pCpp->getFloat("floatLowestValue")
           == (std::numeric_limits<float>::lowest() + std::numeric_limits<float>::epsilon()));
    EXPECT(md_pCpp->getFloat("floatMinValue") == (std::numeric_limits<float>::min() * 2));
    EXPECT(md_pCpp->getFloat("floatMaxValue")
           == (std::numeric_limits<float>::max() - std::numeric_limits<float>::epsilon()));
    EXPECT(md_pCpp->getDouble("doubleLowestValue")
           == (std::numeric_limits<double>::lowest() + std::numeric_limits<double>::epsilon()));
    EXPECT(md_pCpp->getDouble("doubleMinValue") == (std::numeric_limits<double>::min() * 2));
    EXPECT(md_pCpp->getDouble("doubleMaxValue")
           == (std::numeric_limits<double>::max() - std::numeric_limits<double>::epsilon()));


    Metadata md_dec = multio::message::to_metadata(multio::message::to_string(*md_pCpp));

    EXPECT(md_pCpp->getString("stringValue").compare(md_dec.getString("stringValue")) == 0);
    EXPECT(md_pCpp->getString("stringEmptyValue").compare(md_dec.getString("stringEmptyValue")) == 0);
    EXPECT(md_pCpp->getBool("boolMinValue") == md_dec.getBool("boolMinValue"));
    EXPECT(md_pCpp->getBool("boolMaxValue") == md_dec.getBool("boolMaxValue"));
    EXPECT(md_pCpp->getInt("intMinValue") == md_dec.getInt("intMinValue"));
    EXPECT(md_pCpp->getInt("intMaxValue") == md_dec.getInt("intMaxValue"));
    EXPECT(md_pCpp->getLong("longMinValue") == md_dec.getLong("longMinValue"));
    EXPECT(md_pCpp->getLong("longMaxValue") == md_dec.getLong("longMaxValue"));
    long long expctDecLongLongMin;
    long long expctDecLongLongMax;
    md_pCpp->get("longlongMinValue", expctLongLongMin);
    md_pCpp->get("longlongMaxValue", expctLongLongMax);
    md_dec.get("longlongMinValue", expctDecLongLongMin);
    md_dec.get("longlongMaxValue", expctDecLongLongMax);
    EXPECT(expctLongLongMin == expctDecLongLongMin);
    EXPECT(expctLongLongMax == expctDecLongLongMax);
    EXPECT(almost_equal(md_pCpp->getFloat("floatLowestValue"), md_dec.getFloat("floatLowestValue"), 1));
    EXPECT(almost_equal(md_pCpp->getFloat("floatMinValue"), md_dec.getFloat("floatMinValue"), 1));
    EXPECT(almost_equal(md_pCpp->getFloat("floatMaxValue"), md_dec.getFloat("floatMaxValue"), 1));
    EXPECT(almost_equal(md_pCpp->getDouble("doubleLowestValue"), md_dec.getDouble("doubleLowestValue"), 1));
    EXPECT(almost_equal(md_pCpp->getDouble("doubleMinValue"), md_dec.getDouble("doubleMinValue"), 1));
    EXPECT(almost_equal(md_pCpp->getDouble("doubleMaxValue"), md_dec.getDouble("doubleMaxValue"), 1));


    // Metadata md_moved = std::move(*md_pCpp);
    // EXPECT(!md_moved.empty());
    // // EXPECT(md_pCpp->empty()); // THIS IS FAILING; Change request: https://jira.ecmwf.int/browse/ECKIT-601

    EXPECT(err == MULTIO_SUCCESS);
}

CASE("Test write field") {
    multio_configuration_t* multio_cc = nullptr;

    auto configPath = configuration_path_name() / "testPlan.yaml";

    test_check(multio_new_configuration_from_filename(&multio_cc, configPath.localPath()),
               "Configuration Context Created From Filename");
    std::unique_ptr<multio_configuration_t> configuration_context_deleter(multio_cc);

    multio_handle_t* multio_handle = nullptr;
    test_check(multio_new_handle(&multio_handle, multio_cc), "Create New handle");
    EXPECT(multio_handle);
    std::unique_ptr<multio_handle_t> handle_deleter(multio_handle);

    const char* files[2] = {"test.grib", "test2.grib"};

    for (const char* file : files) {
        auto field = configuration_path_name() / "../" / file;
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

        // test_check(multio_notify(multio_handle, md), "Field Written");
    }

    auto file_name = configuration_path_name() / "../../../build/tests/multio/testWriteOutput.grib";
    std::cout << file_name.localPath() << std::endl;
    EXPECT(std::filesystem::exists(file_name.localPath()));
}

// TODO:
//  * test multio_open_connections, multio_close_connections, multio_write_step_complete, multio_write_domain,
//  multio_write_mask, multio_write_field
//  * Testing these with MPI in units is not possible here, maybe use another transport layer
//  * test other transport layers....
}  // namespace test
}  // namespace multio

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

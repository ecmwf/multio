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
#include <cmath>
#include <cstring>
#include <limits>

#include "eckit/io/FileHandle.h"
#include "eckit/testing/Test.h"

#include "multio/api/c/multio_c.h"
#include "multio/api/c/multio_c_cpp_utils.h"
#include "multio/message/Metadata.h"
#include "multio/multio_version.h"

#include "multio/config/PathConfiguration.h"
#include "multio/util/Environment.h"

using multio::config::configuration_file_name;
using multio::config::configuration_path_name;

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


namespace multio::test {

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
    std::unique_ptr<multio_configuration_t> configuration_deleter(cc);
    std::string errStr(multio_error_string(err));

    EXPECT(err == MULTIO_ERROR_ECKIT_EXCEPTION);
    EXPECT(errStr.rfind("Cannot open I_AM_NOT_HERE/multio/config/multio-server.yaml  (No such file or directory)")
           != std::string::npos);
}

CASE("Create handle with default configuration without MPI splitting") {
    multio_configuration_t* cc = nullptr;
    multio_handle_t* mdp = nullptr;
    int err;
    err = multio_new_configuration(&cc);
    std::unique_ptr<multio_configuration_t> configuration_deleter(cc);
    EXPECT(err == MULTIO_SUCCESS);
    err = multio_mpi_allow_world_default_comm(cc, false);
    EXPECT(err == MULTIO_SUCCESS);
    err = multio_new_handle(&mdp, cc);
    std::unique_ptr<multio_handle_t> handle_deleter(mdp);
    std::string errStr(multio_error_string(err));

    EXPECT(err == MULTIO_ERROR_ECKIT_EXCEPTION);
    EXPECT(errStr.rfind(expectedMPIError) != std::string::npos);
}

CASE("Create handle with default configuration through nullptr configuration path without MPI splitting") {
    multio_configuration_t* cc = nullptr;
    multio_handle_t* mdp = nullptr;
    int err;
    err = multio_new_configuration_from_filename(&cc, nullptr);
    EXPECT(err == MULTIO_ERROR_ECKIT_EXCEPTION);
    std::string errStr(multio_error_string(err));
    EXPECT(errStr.rfind("Assertion failed: conf_file_name") != std::string::npos);
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
    std::unique_ptr<multio_configuration_t> configuration_deleter(cc);
    EXPECT(err == MULTIO_SUCCESS);
    err = multio_mpi_allow_world_default_comm(cc, false);
    EXPECT(err == MULTIO_SUCCESS);
    err = multio_new_handle(&mdp, cc);
    std::unique_ptr<multio_handle_t> handle_deleter(mdp);
    std::string errStr(multio_error_string(err));

    EXPECT(err == MULTIO_ERROR_ECKIT_EXCEPTION);
    EXPECT(errStr.rfind(expectedMPIError) != std::string::npos);
}

CASE("Start server with default configuration") {
    multio_configuration_t* cc = nullptr;
    int err;
    err = multio_new_configuration(&cc);
    std::unique_ptr<multio_configuration_t> configuration_deleter(cc);
    EXPECT(err == MULTIO_SUCCESS);
    err = multio_mpi_allow_world_default_comm(cc, false);
    EXPECT(err == MULTIO_SUCCESS);
    err = multio_start_server(cc);
    std::string errStr(multio_error_string(err));

    EXPECT(err == MULTIO_ERROR_ECKIT_EXCEPTION);
    EXPECT(errStr.rfind(expectedMPIError) != std::string::npos);
}

CASE("Test loading configuration") {

    multio_configuration_t* multio_cc = nullptr;

    test_check(multio_new_configuration(&multio_cc), "Config Created from Environment Path");
    std::unique_ptr<multio_configuration_t> configuration_deleter(multio_cc);

    auto configFile = configuration_file_name();
    const char* conf_path = configFile.localPath();

    test_check(multio_config_set_path(multio_cc, conf_path), "Configuration Path Changed");

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
    err = multio_new_metadata(&mdp, nullptr);
    EXPECT(err == MULTIO_SUCCESS);
    err = multio_delete_metadata(mdp);
    EXPECT(err == MULTIO_SUCCESS);
}

CASE("Metadata can set values") {
    using multio::message::Metadata;
    multio_metadata_t* mdp = nullptr;
    int err;
    err = multio_new_metadata(&mdp, nullptr);
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
    EXPECT(err == MULTIO_SUCCESS);
    err = multio_metadata_set_int(mdp, "int64MinValue", std::numeric_limits<std::int64_t>::min());
    EXPECT(err == MULTIO_SUCCESS);
    err = multio_metadata_set_int(mdp, "int64MaxValue", std::numeric_limits<std::int64_t>::max());
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

    EXPECT(md_pCpp->get<std::string>("stringValue").compare("testString") == 0);
    EXPECT(md_pCpp->get<std::string>("stringEmptyValue").compare("") == 0);
    EXPECT(md_pCpp->get<bool>("boolMinValue") == false);
    EXPECT(md_pCpp->get<bool>("boolMaxValue") == true);
    EXPECT(md_pCpp->get<std::int64_t>("int64MinValue") == std::numeric_limits<std::int64_t>::min());
    EXPECT(md_pCpp->get<std::int64_t>("int64MaxValue") == std::numeric_limits<std::int64_t>::max());
    EXPECT(md_pCpp->get<double>("doubleLowestValue")
           == (std::numeric_limits<double>::lowest() + std::numeric_limits<double>::epsilon()));
    EXPECT(md_pCpp->get<double>("doubleMinValue") == (std::numeric_limits<double>::min() * 2));
    EXPECT(md_pCpp->get<double>("doubleMaxValue")
           == (std::numeric_limits<double>::max() - std::numeric_limits<double>::epsilon()));


    Metadata md_dec = multio::message::metadataFromYAML(md_pCpp->toString());

    EXPECT(md_pCpp->get<std::string>("stringValue").compare(md_dec.get<std::string>("stringValue")) == 0);
    EXPECT(md_pCpp->get<std::string>("stringEmptyValue").compare(md_dec.get<std::string>("stringEmptyValue")) == 0);
    EXPECT(md_pCpp->get<bool>("boolMinValue") == md_dec.get<bool>("boolMinValue"));
    EXPECT(md_pCpp->get<bool>("boolMaxValue") == md_dec.get<bool>("boolMaxValue"));
    EXPECT(md_pCpp->get<std::int64_t>("int64MinValue") == md_dec.get<std::int64_t>("int64MinValue"));
    EXPECT(md_pCpp->get<std::int64_t>("int64MaxValue") == md_dec.get<std::int64_t>("int64MaxValue"));
    EXPECT(almost_equal(md_pCpp->get<double>("doubleLowestValue"), md_dec.get<double>("doubleLowestValue"), 1));
    EXPECT(almost_equal(md_pCpp->get<double>("doubleMinValue"), md_dec.get<double>("doubleMinValue"), 1));
    EXPECT(almost_equal(md_pCpp->get<double>("doubleMaxValue"), md_dec.get<double>("doubleMaxValue"), 1));

    EXPECT(err == MULTIO_SUCCESS);
}

CASE("Test write field") {
    multio_configuration_t* multio_cc = nullptr;

    auto configPath = configuration_path_name() / "testPlan.yaml";
    eckit::Log::info() << configPath.localPath() << std::endl;

    test_check(multio_new_configuration_from_filename(&multio_cc, configPath.localPath()),
               "Configuration Context Created From Filename");
    std::unique_ptr<multio_configuration_t> configuration_deleter(multio_cc);

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

        {
            multio_metadata_t* md = nullptr;
            test_check(multio_new_metadata(&md, nullptr), "Create New Metadata Object");
            std::unique_ptr<multio_metadata_t> multio_deleter(md);

            test_check(multio_metadata_set_string(md, "category", file), "Set category");
            test_check(multio_metadata_set_int(md, "globalSize", len), "Set globalsize");
            test_check(multio_metadata_set_int(md, "level", 1), "Set level");
            test_check(multio_metadata_set_int(md, "step", 1), "Set step");

            test_check(multio_metadata_set_double(md, "missingValue", 0.0), "Set missingValue");
            test_check(multio_metadata_set_bool(md, "bitmapPresent", false), "Set bitmapPresent");
            test_check(multio_metadata_set_int(md, "bitsPerValue", 16), "Set bitsPerValue");

            test_check(multio_metadata_set_bool(md, "toAllServers", false), "Set toAllServers");

            // Overwrite these fields in the existing metadata object
            test_check(multio_metadata_set_string(md, "name", "test"), "Set name");

            test_check(multio_write_field(multio_handle, md, reinterpret_cast<const double*>(buffer.data()), len),
                       "Write Field");
        }
    }

    {
        multio_metadata_t* md = nullptr;
        test_check(multio_new_metadata(&md, nullptr), "Create New Metadata Object");
        std::unique_ptr<multio_metadata_t> multio_deleter(md);

        test_check(multio_metadata_set_int(md, "step", 123), "Set step");
        test_check(multio_metadata_set_string(md, "trigger", "step"), "Set trigger");
        test_check(multio_notify(multio_handle, md), "Trigger step notification");
    }

    auto path = util::getEnv("CMAKE_BINARY_HOME");
    auto file_name = eckit::PathName{std::string{*path}} / "testWriteOutput.grib";
    EXPECT(file_name.exists());
}

}  // namespace multio::test

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

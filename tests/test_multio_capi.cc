/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include <unistd.h>
#include <cstring>
#include <limits>

#include "eckit/filesystem/TmpFile.h"
#include "eckit/testing/Test.h"

#include "multio/api/multio_c.h"
#include "multio/api/multio_c_cpp_utils.h"
#include "multio/message/Metadata.h"

#include "TestDataContent.h"
#include "TestHelpers.h"


namespace multio {
namespace test {

// TODO: Can we keep this?
// Copied from https://en.cppreference.com/w/cpp/types/numeric_limits/epsilon
template<class T>
typename std::enable_if<!std::numeric_limits<T>::is_integer, bool>::type
    almost_equal(T x, T y, int ulp)
{
    // the machine epsilon has to be scaled to the magnitude of the values used
    // and multiplied by the desired precision in ULPs (units in the last place)
    return std::fabs(x-y) <= std::numeric_limits<T>::epsilon() * std::fabs(x+y) * ulp
        // unless the result is subnormal
        || std::fabs(x-y) < std::numeric_limits<T>::min();
}

CASE("Try Create handle with wrong configuration path") {
    multio_handle_t* mdp = nullptr;
    int err;
    err = multio_new_handle_from_config(&mdp, "I_AM_NOT_HERE/server/config/multio-server.yaml");
    std::string errStr(multio_error_string(err));
    // std::cout << "new handle err" << err << " Message: " << errStr << std::endl;
    EXPECT(err == MULTIO_ERROR_ECKIT_EXCEPTION);
    EXPECT(errStr.compare("Cannot open I_AM_NOT_HERE/server/config/multio-server.yaml  (No such file or directory) ") == 0);
}

CASE("Create handle with default configuration without MPI splitting") {
    multio_handle_t* mdp = nullptr;
    int err;
    err = multio_new_handle(&mdp);
    std::string errStr(multio_error_string(err));
    std::cout << "new handle err" << err << " Message: " << errStr << std::endl;
    EXPECT(err == MULTIO_ERROR_ECKIT_EXCEPTION);
    EXPECT(errStr.rfind("SeriousBug: No communicator called nemo") != std::string::npos);
}

CASE("Create handle with default configuration through nullptr configuration path without MPI splitting") {
    multio_handle_t* mdp = nullptr;
    int err;
    err = multio_new_handle_from_config(&mdp, nullptr);
    std::string errStr(multio_error_string(err));
    // std::cout << "new handle err" << err << " Message: " << errStr << std::endl;
    EXPECT(err == MULTIO_ERROR_ECKIT_EXCEPTION);
    EXPECT(errStr.rfind("SeriousBug: No communicator called nemo") != std::string::npos);
}



CASE("Create handle with configuration path without MPI splitting") {
    multio_handle_t* mdp = nullptr;
    int err;
    const char* env_config_path = std::getenv("MULTIO_SERVER_CONFIG_PATH");
    EXPECT(env_config_path);
    std::ostringstream oss;
    oss << env_config_path << "/multio-server.yaml";
    std::string path = oss.str();
    err = multio_new_handle_from_config(&mdp, path.c_str());
    std::string errStr(multio_error_string(err));
    // std::cout << "new handle err" << err << " Message: " << errStr << std::endl;
    EXPECT(err == MULTIO_ERROR_ECKIT_EXCEPTION);
    EXPECT(errStr.rfind("SeriousBug: No communicator called nemo") != std::string::npos);
}

CASE("Start server with default configuration & unknown server name") {
    int err;
    err = multio_start_server("I_AM_NOT_HERE");
    std::string errStr(multio_error_string(err));
    // std::cout << "new handle err" << err << " Message: " << errStr << std::endl;
    EXPECT(err == MULTIO_ERROR_ECKIT_EXCEPTION);
    EXPECT(errStr.rfind("Configuration 'I_AM_NOT_HERE' not found") != std::string::npos);
}

CASE("Start server with default configuration") {
    int err;
    err = multio_start_server("nemo-ioserver");
    std::string errStr(multio_error_string(err));
    std::cout << "new handle err" << err << " Message: " << errStr << std::endl;
    EXPECT(err == MULTIO_ERROR_ECKIT_EXCEPTION);
    EXPECT(errStr.rfind("SeriousBug: No communicator called nemo") != std::string::npos);
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
    EXPECT(err == MULTIO_SUCCESS);

    err = multio_metadata_set_string_value(mdp, "stringValue", "testString");
    EXPECT(err == MULTIO_SUCCESS);
    err = multio_metadata_set_string_value(mdp, "stringEmptyValue", "");
    EXPECT(err == MULTIO_SUCCESS);
    err = multio_metadata_set_bool_value(mdp, "boolMinValue", false);
    EXPECT(err == MULTIO_SUCCESS);
    err = multio_metadata_set_bool_value(mdp, "boolMaxValue", true);
    EXPECT(err == MULTIO_SUCCESS);
    err = multio_metadata_set_int_value(mdp, "intMinValue", std::numeric_limits<int>::min());
    EXPECT(err == MULTIO_SUCCESS);
    err = multio_metadata_set_int_value(mdp, "intMaxValue", std::numeric_limits<int>::max());
    EXPECT(err == MULTIO_SUCCESS);
    err = multio_metadata_set_long_value(mdp, "longMinValue", std::numeric_limits<long>::min());
    EXPECT(err == MULTIO_SUCCESS);
    err = multio_metadata_set_long_value(mdp, "longMaxValue", std::numeric_limits<long>::max());
    EXPECT(err == MULTIO_SUCCESS);
    err = multio_metadata_set_longlong_value(mdp, "longlongMinValue", std::numeric_limits<long long>::min());
    EXPECT(err == MULTIO_SUCCESS);
    err = multio_metadata_set_longlong_value(mdp, "longlongMaxValue", std::numeric_limits<long long>::max());
    EXPECT(err == MULTIO_SUCCESS);
    err = multio_metadata_set_float_value(mdp, "floatLowestValue", std::numeric_limits<float>::lowest() + std::numeric_limits<float>::epsilon());
    EXPECT(err == MULTIO_SUCCESS);
    err = multio_metadata_set_float_value(mdp, "floatMinValue", std::numeric_limits<float>::min() * 2);
    EXPECT(err == MULTIO_SUCCESS);
    err = multio_metadata_set_float_value(mdp, "floatMaxValue", std::numeric_limits<float>::max() - std::numeric_limits<float>::epsilon());
    EXPECT(err == MULTIO_SUCCESS);
    err = multio_metadata_set_double_value(mdp, "doubleLowestValue", std::numeric_limits<double>::lowest() + std::numeric_limits<double>::epsilon());
    EXPECT(err == MULTIO_SUCCESS);
    err = multio_metadata_set_double_value(mdp, "doubleMinValue", std::numeric_limits<double>::min() * 2);
    EXPECT(err == MULTIO_SUCCESS);
    err = multio_metadata_set_double_value(mdp, "doubleMaxValue", std::numeric_limits<double>::max() - std::numeric_limits<double>::epsilon());
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
    EXPECT(md_pCpp->getFloat("floatLowestValue") == (std::numeric_limits<float>::lowest() + std::numeric_limits<float>::epsilon()));
    EXPECT(md_pCpp->getFloat("floatMinValue") == (std::numeric_limits<float>::min() * 2));
    EXPECT(md_pCpp->getFloat("floatMaxValue") == (std::numeric_limits<float>::max() - std::numeric_limits<float>::epsilon()));
    EXPECT(md_pCpp->getDouble("doubleLowestValue") == (std::numeric_limits<double>::lowest() + std::numeric_limits<double>::epsilon()));
    EXPECT(md_pCpp->getDouble("doubleMinValue") == (std::numeric_limits<double>::min() * 2));
    EXPECT(md_pCpp->getDouble("doubleMaxValue") == (std::numeric_limits<double>::max() - std::numeric_limits<double>::epsilon()));



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

    err = multio_delete_metadata(mdp);
    EXPECT(err == MULTIO_SUCCESS);
}
}  // namespace test
}  // namespace multio

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

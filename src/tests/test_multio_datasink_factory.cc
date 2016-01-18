/*
 * (C) Copyright 1996-2015 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#define BOOST_TEST_MODULE test_multio_datasink

#include "ecbuild/boost_test_framework.h"

#include "eckit/log/Log.h"
#include "multio/DataSink.h"
#include "multio/FileSink.h"

#include <iostream>

using namespace multio;
using namespace eckit;

// -------------------------------------------------------------------------------------------------

class TestDataSink : public DataSink {

public:

    TestDataSink(const Configuration& config) :
        DataSink(config),
        config_(&config) {}

    virtual ~TestDataSink() {}

    virtual void write(eckit::DataBlobPtr blob, JournalRecord *const record) {}

    Configuration const * config_;

protected:

    virtual void print(std::ostream& os) const { os << "tmp"; }
};

// And a factory builder!!
static DataSinkBuilder<TestDataSink> testSinkBuilder("test");

// -------------------------------------------------------------------------------------------------

BOOST_AUTO_TEST_SUITE( test_multio_datasink_factory )

BOOST_AUTO_TEST_CASE( test_factory_generate )
{
    LocalConfiguration config;
    DataSink * sink = DataSinkFactory::build("test", config);

    // Check that we generate a sink of the correct type (and implicitly that the factory
    // is correctly registered).
    TestDataSink * testSink = dynamic_cast<TestDataSink*>(sink);
    BOOST_CHECK(testSink);

    // Test that the configuration is passed through the builder/factory.
    BOOST_CHECK_EQUAL(testSink->config_, &config);
}

BOOST_AUTO_TEST_CASE( test_list_factories )
{
    // DataSinkFactory::list appends the results to a ostream&, so we need to extract them.
    std::stringstream ss;
    DataSinkFactory::list(ss);

    // Copy the string, as strtok is destructive, and then split it into a vector.
    std::string list_str = ss.str();
    ScopedPtr<char> cstr(new char[list_str.length()+1]);
    std::strcpy(cstr.get(), list_str.c_str());

    std::vector<std::string> strings;
    char * p = std::strtok(cstr.get(), ", ");
    while (NULL != p) {
        strings.push_back(std::string(p));
        p = std::strtok(NULL, ", ");
    }

    // We expect the file and MultIO factories to be in there too...
    BOOST_CHECK(std::find(strings.begin(), strings.end(), "file") != strings.end());
    BOOST_CHECK(std::find(strings.begin(), strings.end(), "multio") != strings.end());
    BOOST_CHECK(std::find(strings.begin(), strings.end(), "test") != strings.end());
}

BOOST_AUTO_TEST_SUITE_END()

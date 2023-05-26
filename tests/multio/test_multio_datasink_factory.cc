/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */


#include <algorithm>
#include <cstring>

#include "eckit/log/Log.h"
#include "eckit/testing/Test.h"
#include "eckit/utils/Tokenizer.h"

#include "multio/config/ComponentConfiguration.h"
#include "multio/sink/DataSink.h"
#include "multio/sink/FileSink.h"

using namespace eckit;
using namespace eckit::testing;

using namespace multio::sink;

namespace multio::test {

//----------------------------------------------------------------------------------------------------------------------

class TestDataSink : public DataSink {
public:
    TestDataSink(const config::ComponentConfiguration& compConf) : DataSink(compConf), compConf_(&compConf) {}

    void write(eckit::message::Message) override {}

    config::ComponentConfiguration const* compConf_;

protected:
    void print(std::ostream& os) const override { os << "tmp"; }
};


static DataSinkBuilder<TestDataSink> testSinkBuilder("test");

//----------------------------------------------------------------------------------------------------------------------

CASE("test_factory_generate") {
    LocalConfiguration config;
    config::MultioConfiguration multioConf(config, "", "");
    config::ComponentConfiguration compConf(config, multioConf);
    std::unique_ptr<DataSink> sink(DataSinkFactory::instance().build("test", compConf));

    // Check that we generate a sink of the correct type (and implicitly that the factory
    // is correctly registered).
    auto testSink = dynamic_cast<TestDataSink*>(sink.get());
    EXPECT(testSink);

    // Test that the configuration is passed through the builder/factory.
    EXPECT(testSink->compConf_ == &compConf);
}

CASE("test_list_factories") {
    // DataSinkFactory::list appends the results to a ostream&, so we need to extract them.
    std::stringstream ss;
    DataSinkFactory::instance().list(ss);

    // Extract the seperate components from the string stream into a vector
    std::vector<std::string> strings;
    eckit::Tokenizer(" ,")(ss.str(), strings);

    // We expect the file and MultIO factories to be in there too...
    EXPECT(std::find(strings.begin(), strings.end(), "file") != strings.end());
    EXPECT(std::find(strings.begin(), strings.end(), "multio") != strings.end());
    EXPECT(std::find(strings.begin(), strings.end(), "test") != strings.end());
}


//-----------------------------------------------------------------------------

}  // namespace multio::test

int main(int argc, char** argv) {
    return run_tests(argc, argv);
}

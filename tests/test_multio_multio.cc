/*
 * (C) Copyright 1996-2015 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "eckit/log/Log.h"
#include "multio/DataSink.h"
#include "multio/FileSink.h"

#include "eckit/config/YAMLConfiguration.h"
#include "eckit/testing/Test.h"

using namespace eckit::testing;

namespace multio {
namespace test {

//-----------------------------------------------------------------------------

CASE("test_dummy") {
    EXPECT(true);
}

CASE("test_multio_with_event_trigger") {
    std::istringstream in(
        "{ \"sinks\" : [ {\"type\" : \"file\", \"path\" : \"/dev/null\"} ], \"event_trigger\" : { "
        "\"host\" : \"cob-login-4\", \"port\" : "
        "\"106723\", \"metadata\" : {\"step\" : [0, 3, 6, 9, 12]} } }");

    eckit::YAMLConfiguration config(in.str());

    DataSink* multio_sink = DataSinkFactory::build("multio", config);
}

//-----------------------------------------------------------------------------

}  // namespace test
}  // namespace multio

int main(int argc, char** argv) {
    return run_tests(argc, argv);
}

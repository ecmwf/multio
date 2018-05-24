/*
 * (C) Copyright 1996- ECMWF.
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

#include "eckit/testing/Test.h"

using namespace eckit::testing;

namespace multio {
namespace test {

//-----------------------------------------------------------------------------

CASE("test_dummy") {
    EXPECT(true);
}

//-----------------------------------------------------------------------------

}  // namespace test
}  // namespace multio

int main(int argc, char** argv) {
    return run_tests(argc, argv);
}

/*
 * (C) Copyright 1996-2015 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include <fstream>

#include "eckit/config/YAMLConfiguration.h"
#include "eckit/log/JSON.h"
#include "eckit/testing/Test.h"

#include "multio/ifsio.h"

namespace multio {
namespace test {

//----------------------------------------------------------------------------------------------------------------------

CASE("default bitspervalue") {

      eckit::testing::SetEnv env("MULTIO_CONFIG", "{sinks:[]}");

      int bpv = 0;
      int paramid = 128130;
      std::string levtype{"ml"};

      imultio_encode_bitspervalue_(&bpv, &paramid, levtype.c_str(), levtype.size());
      EXPECT_EQUAL(bpv, 16);
}

//----------------------------------------------------------------------------------------------------------------------

}  // namespace test
}  // namespace multio

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

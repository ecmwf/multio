/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */


/// @author Philipp Geier

#include <unistd.h>
#include <cstring>
#include <limits>
#include <iostream>

#include "eckit/testing/Test.h"
#include "eckit/config/LocalConfiguration.h"

#include "multio/util/ConfigurationPath.h"
#include "multio/util/ConfigurationContext.h"

namespace multio {

using util::configuration_file;
using util::ConfigurationContext;

namespace test {

CASE("Create default context and tranverse client plans") {
    ConfigurationContext confCtx{};
    EXPECT(confCtx.config().has("client"));

    ConfigurationContext clientCtx = confCtx.subContext("client");
    EXPECT(clientCtx.config().has("plans"));

    int i =0;
    for(ConfigurationContext subCtx: clientCtx.subContexts("plans")) {
       // std::cout << subCtx.config().getString("name") << std::endl;
       switch (i) {
        case 0:
            EXPECT(subCtx.config().getString("name") == "ocean-replay-grid-info-stream");
            break;
        case 1:
            EXPECT(subCtx.config().getString("name") == "ocean-replay-test-stream1");
            break;
        case 2:
            EXPECT(subCtx.config().getString("name") == "ocean-replay-test-stream2");
            break;
       }
       ++i;
    }
}

}  // namespace test
}  // namespace multio

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

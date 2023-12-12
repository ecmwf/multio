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
#include <iostream>
#include <limits>

#include "eckit/config/LocalConfiguration.h"
#include "eckit/testing/Test.h"

#include "multio/config/ComponentConfiguration.h"
#include "multio/config/ConfigurationPath.h"

using multio::config::ComponentConfiguration;
using multio::config::configuration_file;
using multio::config::MultioConfiguration;

namespace multio::test {


CASE("Create default context and tranverse client plans") {
    MultioConfiguration multioConf{};
    ComponentConfiguration compConf{multioConf.parsedConfig(), multioConf};

    EXPECT(compConf.parsedConfig().has("client"));

    ComponentConfiguration clientConf = compConf.subComponent("client");
    EXPECT(clientConf.parsedConfig().has("plans"));

    int i = 0;
    for (ComponentConfiguration subConf : clientConf.subComponents("plans")) {
        // std::cout << subConf.parsedConfig().getString("name") << std::endl;
        switch (i) {
            case 0:
                EXPECT(subConf.parsedConfig().getString("name") == "ocean-replay-grid-info-stream");
                break;
            case 1:
                EXPECT(subConf.parsedConfig().getString("name") == "ocean-replay-test-stream1");
                break;
            case 2:
                EXPECT(subConf.parsedConfig().getString("name") == "ocean-replay-test-stream2");
                break;
        }
        ++i;
    }
}

}  // namespace multio::test

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

#include "eckit/testing/Test.h"

#include "../MultioTestEnvironment.h"

namespace multio::test::sink {

using multio::test::MultioTestEnvironment;

CASE("Initialize action 'sink' with Maestro") {
    const std::string plan = R"json({
        "name": "initialize maestro sink",
        "actions": [
            { "type": "sink", "sinks": [ { "type": "maestro", "ready-cdo": false } ] }
        ]
    })json";
    EXPECT_NO_THROW(auto env = MultioTestEnvironment(plan));
}

}  // multio::test::sink

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

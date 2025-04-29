
#include "eckit/testing/Test.h"

#include "../../MultioTestEnvironment.h"


namespace multio::test {

CASE("Initialize action 'encode-mtg2'") {
    const std::string plan = R"json({
        "name": "Initialize action 'encode-mtg2'",
        "actions": [
            { "type": "encode-mtg2" },
            { "type": "sink", "sinks": [] }
        ]
    })json";

    // Creating the environment will throw an exception if the action 'encode-mtg2'
    // cannot be initialized!
    EXPECT_NO_THROW(auto env = MultioTestEnvironment(plan));
}

}  // namespace multio::test

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

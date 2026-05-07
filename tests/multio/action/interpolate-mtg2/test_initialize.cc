#include "eckit/testing/Test.h"

#include "../../MultioTestEnvironment.h"


namespace multio::test {

CASE("Initialize action 'interpolate-mtg2'") {
    const std::string plan = R"json({
        "name": "Initialize action 'interpolate-mtg2'",
        "actions": [
            { "type": "interpolate-mtg2", "outputs": [] },
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

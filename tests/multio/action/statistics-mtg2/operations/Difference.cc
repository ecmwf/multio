
#include "eckit/testing/Test.h"

#include "Operation.h"

namespace multio::test::statistics_mtg2 {

class DifferenceTest : public StatisticsOperationTest {
public:

    DifferenceTest() : StatisticsOperationTest("difference") {}

    double reference(const SinglePointOverTime &input, const double init) override {
        EXPECT_NOT_EQUAL(input.size(), 0);
        return input[input.size()-1] - init;
    }

};

auto test = DifferenceTest();

CASE("single test") { test.runSingle(); }
CASE("multiple test") { test.runMultiple(); }

}  // multio::test::statistics_mtg2

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

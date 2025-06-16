
#include "eckit/testing/Test.h"

#include "Operation.h"

namespace multio::test::statistics_mtg2 {

class MinimumTest : public StatisticsOperationTest {
public:

    MinimumTest() : StatisticsOperationTest("minimum") {}

    double reference(const std::vector<double> &input) {
        EXPECT_NOT_EQUAL(input.size(), 0);
        return *std::min_element(input.begin(), input.end());
    }

};

auto test = MinimumTest();

CASE("single test") { test.runSingle(); }
CASE("multiple test") { test.runMultiple(); }

}  // multio::test::statistics_mtg2

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

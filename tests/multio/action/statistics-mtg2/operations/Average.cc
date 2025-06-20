
#include "eckit/testing/Test.h"

#include "Operation.h"

namespace multio::test::statistics_mtg2 {

class AverageTest : public StatisticsOperationTest {
public:

    AverageTest() : StatisticsOperationTest("average") {}

    double reference(const std::vector<double> &input, const double init) {
        EXPECT_NOT_EQUAL(input.size(), 0);
        return std::accumulate(input.begin(), input.end(), 0.0) / input.size();
    }

};

auto test = AverageTest();

CASE("single test") { test.runSingle(); }
CASE("multiple test") { test.runMultiple(); }

}  // multio::test::statistics_mtg2

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

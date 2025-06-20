
#include "eckit/testing/Test.h"

#include "Operation.h"

namespace multio::test::statistics_mtg2 {

class InverseDifferenceTest : public StatisticsOperationTest {
public:

    InverseDifferenceTest() : StatisticsOperationTest("inverse-difference") {}

    double reference(const std::vector<double> &input, const double init) override {
        EXPECT_NOT_EQUAL(input.size(), 0);
        return init - input[input.size()-1];
    }

};

auto test = InverseDifferenceTest();

CASE("single test") { test.runSingle(); }
CASE("multiple test") { test.runMultiple(); }

}  // multio::test::statistics_mtg2

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

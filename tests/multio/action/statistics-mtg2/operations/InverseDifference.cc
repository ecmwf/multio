
#include "eckit/testing/Test.h"

#include "Operation.h"

namespace multio::test::statistics_mtg2 {

template <typename ElemType>
class InverseDifferenceTest : public StatisticsOperationTest<ElemType> {
public:
    using typename StatisticsOperationTest<ElemType>::SinglePointOverTime;

    InverseDifferenceTest() : StatisticsOperationTest<ElemType>("inverse-difference") {}

    ElemType reference(const SinglePointOverTime &input, const ElemType init) override {
        EXPECT_NOT_EQUAL(input.size(), 0);
        return init - input[input.size()-1];
    }

};

auto testFloat = InverseDifferenceTest<float>();
auto testDouble = InverseDifferenceTest<double>();

CASE("single test float") { testFloat.runSingle(); }
CASE("single test double") { testDouble.runSingle(); }
CASE("multiple test float") { testFloat.runMultiple(); }
CASE("multiple test double") { testDouble.runMultiple(); }

}  // multio::test::statistics_mtg2

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

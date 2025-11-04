
#include "eckit/testing/Test.h"

#include "Operation.h"

namespace multio::test::statistics_mtg2 {

template <typename ElemType>
class AverageTest : public StatisticsOperationTest<ElemType> {
public:
    using typename StatisticsOperationTest<ElemType>::SinglePointOverTime;

    AverageTest() : StatisticsOperationTest<ElemType>("average") {}

    ElemType reference(const SinglePointOverTime &input, const ElemType init) override {
        EXPECT_NOT_EQUAL(input.size(), 0);
        return std::accumulate(input.begin(), input.end(), 0.0) / input.size();
    }

};

auto testFloat = AverageTest<float>();
auto testDouble = AverageTest<double>();

CASE("single test float") { testFloat.runSingle(); }
CASE("single test double") { testDouble.runSingle(); }
CASE("multiple unaligned test float") { testFloat.runMultipleUnaligned(); }
CASE("multiple unaligned test double") { testDouble.runMultipleUnaligned(); }
CASE("multiple aligned test float") { testFloat.runMultipleAligned(); }
CASE("multiple aligned test double") { testDouble.runMultipleAligned(); }

}  // multio::test::statistics_mtg2

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

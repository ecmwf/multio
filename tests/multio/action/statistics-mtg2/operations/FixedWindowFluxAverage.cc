
#include "eckit/testing/Test.h"

#include "Operation.h"

inline constexpr std::size_t SECONDS_IN_24_HOURS = 86400;

namespace multio::test::statistics_mtg2 {

template <typename ElemType>
class FixedWindowFluxAverageTest : public StatisticsOperationTest<ElemType> {
public:
    using typename StatisticsOperationTest<ElemType>::SinglePointOverTime;

    FixedWindowFluxAverageTest() : StatisticsOperationTest<ElemType>("fixed-window-flux-average") {}

    ElemType reference(const SinglePointOverTime &input, const ElemType init) override {
        EXPECT_NOT_EQUAL(input.size(), 0);
        return (input[input.size() - 1] - init) / (input.size() * SECONDS_IN_24_HOURS);
    }

};

auto testFloat = FixedWindowFluxAverageTest<float>();
auto testDouble = FixedWindowFluxAverageTest<double>();

CASE("single test float") { testFloat.runSingle(); }
CASE("single test double") { testDouble.runSingle(); }
CASE("multiple test float") { testFloat.runMultiple(); }
CASE("multiple test double") { testDouble.runMultiple(); }

}  // multio::test::statistics_mtg2

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

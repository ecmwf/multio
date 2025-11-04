
#include "eckit/testing/Test.h"

#include "Operation.h"

inline constexpr std::size_t SECONDS_IN_24_HOURS = 86400;

namespace multio::test::statistics_mtg2 {

template <typename ElemType>
class FluxAverageTest : public StatisticsOperationTest<ElemType> {
public:
    using typename StatisticsOperationTest<ElemType>::SinglePointOverTime;

    FluxAverageTest() : StatisticsOperationTest<ElemType>("flux-average") {}

    ElemType reference(const SinglePointOverTime &input, const ElemType init) override {
        EXPECT_NOT_EQUAL(input.size(), 0);
        return input[input.size() - 1] / (input.size() * SECONDS_IN_24_HOURS);
    }

};

auto testFloat = FluxAverageTest<float>();
auto testDouble = FluxAverageTest<double>();

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

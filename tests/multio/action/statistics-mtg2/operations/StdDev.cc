
#include "eckit/testing/Test.h"

#include "Operation.h"

namespace multio::test::statistics_mtg2 {

template <typename ElemType>
class StdDevTest : public StatisticsOperationTest<ElemType> {
public:
    using typename StatisticsOperationTest<ElemType>::SinglePointOverTime;

    StdDevTest() : StatisticsOperationTest<ElemType>("stddev", 100 * std::numeric_limits<ElemType>::epsilon()) {}

    ElemType reference(const SinglePointOverTime &input, const ElemType init) override {
        EXPECT_NOT_EQUAL(input.size(), 0);
        auto avg = std::accumulate(input.begin(), input.end(), 0.0) / input.size();
        auto var = std::accumulate(input.begin(), input.end(), 0.0,
            [avg](ElemType a, ElemType v) {
                return a + (v - avg) * (v - avg);
            });
        return std::sqrt(var / input.size());
    }

};

auto testFloat = StdDevTest<float>();
auto testDouble = StdDevTest<double>();

CASE("single test float") { testFloat.runSingle(); }
CASE("single test double") { testDouble.runSingle(); }
CASE("multiple test float") { testFloat.runMultiple(); }
CASE("multiple test double") { testDouble.runMultiple(); }

}  // multio::test::statistics_mtg2

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

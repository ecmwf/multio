
#include <numeric>
#include <unistd.h>

#include "eckit/testing/Test.h"
#include "multio/util/ScopedTimer.h"

namespace multio {
namespace test {

/// NOTE: the paramids used in this test are absolutely artificial and invented and do not represent actual codes


//----------------------------------------------------------------------------------------------------------------------

CASE("Accumulate timings correctly") {
    eckit::Timing timing_;
    for (auto ii = 0; ii < 3; ++ii) {
        util::ScopedTimer scTimer{timing_};
        ::usleep(10000);
    }

    eckit::Log::info() << "Elapsed time = " << timing_ << "s" << std::endl;

    EXPECT(0.025 < timing_.elapsed_);
    EXPECT(timing_.elapsed_ < 0.035);
}

//----------------------------------------------------------------------------------------------------------------------

}  // namespace test
}  // namespace multio

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

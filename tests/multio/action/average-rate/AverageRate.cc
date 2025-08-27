#include "eckit/testing/Test.h"

#include "../../MultioTestEnvironment.h"

namespace multio::test::average_rate {

using multio::test::MultioTestEnvironment;
using multio::message::Message;
using multio::message::Metadata;
using eckit::testing::ArrayView;

// Accumulated param -> Time-mean rate param
using Param2ParamMapping = std::tuple<std::int64_t, std::int64_t>;

std::vector<Param2ParamMapping> mappings = {
    {228228, 235055},
    {162108, 235013}
};

void testParameterMapping(std::int64_t paramIn, std::int64_t paramOut) {
    const auto plan = R"json({
        "name": "average_rate_test",
        "actions": [
            {
                "type": "average-rate"
            },
            {
                "type": "debug-sink"
            }
        ]
    })json";
    auto env = MultioTestEnvironment(plan);
    EXPECT_EQUAL(env.debugSink().size(), 0);

    {   // Send a single input field message
        auto md = Metadata({
            {"param", paramIn},
            {"levtype", "sfc"},
            {"grid", "none"},
            {"startDate", 19961001},
            {"startTime", 0000},
            {"step", 24},
            {"timespan", 24},
            {"misc-precision", "double"}
        });
        std::vector<double> values = {0.0, 1.0, 86400};
        auto pl = eckit::Buffer(values.data(), values.size() * sizeof(double));
        auto msg = Message({Message::Tag::Field, {}, {}, std::move(md)}, std::move(pl));
        EXPECT_NO_THROW(env.process(std::move(msg)));
        EXPECT_EQUAL(env.debugSink().size(), 1);
        EXPECT(env.debugSink().front().tag() == Message::Tag::Field);
    }

    {   // Check the correctness of the output field message
        auto md = env.debugSink().front().metadata();
        EXPECT_EQUAL(md.get<std::int64_t>("param"), paramOut);
        EXPECT_EQUAL(md.get<std::int64_t>("timespan"), 24);

        // Check the result
        std::vector<double> ref = {0.0, 1.0/86400.0, 1.0};
        auto res = ArrayView<double>(static_cast<const double*>(env.debugSink().front().payload().data()),
                                     env.debugSink().front().payload().size() / sizeof(double));
        EXPECT_EQUAL(res.size(), ref.size());
        EXPECT(res.isApproximatelyEqual(ref, 10 * std::numeric_limits<double>::epsilon()));
    }
}

CASE("average-rate parameter mappings") {
    for (Param2ParamMapping mapping : mappings) {
        std::int64_t paramIn = std::get<0>(mapping);
        std::int64_t paramOut = std::get<1>(mapping);

        std::ostringstream os;
        os << "paramIn=" << paramIn << ", paramOut=" << paramOut;

        SECTION(os.str()) {
            testParameterMapping(paramIn, paramOut);
        }
    }
}

}  // multio::test::average_rate

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

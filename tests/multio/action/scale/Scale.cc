#include "eckit/testing/Test.h"

#include "../../MultioTestEnvironment.h"

namespace multio::test::scale {

using multio::test::MultioTestEnvironment;
using multio::message::Message;
using multio::message::Metadata;
using eckit::testing::ArrayView;

CASE("user defined mapping") {
    const auto plan = R"json({
        "name": "average_rate_test",
        "actions": [
            {
                "type": "scale",
                "custom-mappings": [
                    {
                        "param-in": 228,
                        "param-out": 228228,
                        "scaling": 1000
                    }
                ]
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
            {"param", 228},
            {"misc-precision", "double"}
        });
        std::vector<double> values = {0.0, 1.0};
        auto pl = eckit::Buffer(values.data(), values.size() * sizeof(double));
        auto msg = Message({Message::Tag::Field, {}, {}, std::move(md)}, std::move(pl));
        EXPECT_NO_THROW(env.process(std::move(msg)));
        EXPECT_EQUAL(env.debugSink().size(), 1);
        EXPECT(env.debugSink().front().tag() == Message::Tag::Field);
    }

    {   // Check the correctness of the output field message
        auto md = env.debugSink().front().metadata();
        EXPECT_EQUAL(md.get<std::int64_t>("param"), 228228);

        // Check the result
        std::vector<double> ref = {0.0, 1000.0};
        auto res = ArrayView<double>(static_cast<const double*>(env.debugSink().front().payload().data()),
                                     env.debugSink().front().payload().size() / sizeof(double));
        EXPECT_EQUAL(res.size(), ref.size());
        EXPECT(res.isApproximatelyEqual(ref, 10 * std::numeric_limits<double>::epsilon()));
    }
}

CASE("local-to-wmo mapping") {
    const auto plan = R"json({
        "name": "average_rate_test",
        "actions": [
            {
                "type": "scale",
                "preset-mappings": "local-to-wmo"
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
            {"param", 228},
            {"misc-precision", "double"}
        });
        std::vector<double> values = {0.0, 1.0};
        auto pl = eckit::Buffer(values.data(), values.size() * sizeof(double));
        auto msg = Message({Message::Tag::Field, {}, {}, std::move(md)}, std::move(pl));
        EXPECT_NO_THROW(env.process(std::move(msg)));
        EXPECT_EQUAL(env.debugSink().size(), 1);
        EXPECT(env.debugSink().front().tag() == Message::Tag::Field);
    }

    {   // Check the correctness of the output field message
        auto md = env.debugSink().front().metadata();
        EXPECT_EQUAL(md.get<std::int64_t>("param"), 228228);

        // Check the result
        std::vector<double> ref = {0.0, 1000.0};
        auto res = ArrayView<double>(static_cast<const double*>(env.debugSink().front().payload().data()),
                                     env.debugSink().front().payload().size() / sizeof(double));
        EXPECT_EQUAL(res.size(), ref.size());
        EXPECT(res.isApproximatelyEqual(ref, 10 * std::numeric_limits<double>::epsilon()));
    }
}

CASE("wmo-to-local mapping") {
    const auto plan = R"json({
        "name": "average_rate_test",
        "actions": [
            {
                "type": "scale",
                "preset-mappings": "wmo-to-local"
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
            {"param", 228228},
            {"misc-precision", "double"}
        });
        std::vector<double> values = {0.0, 1.0};
        auto pl = eckit::Buffer(values.data(), values.size() * sizeof(double));
        auto msg = Message({Message::Tag::Field, {}, {}, std::move(md)}, std::move(pl));
        EXPECT_NO_THROW(env.process(std::move(msg)));
        EXPECT_EQUAL(env.debugSink().size(), 1);
        EXPECT(env.debugSink().front().tag() == Message::Tag::Field);
    }

    {   // Check the correctness of the output field message
        auto md = env.debugSink().front().metadata();
        EXPECT_EQUAL(md.get<std::int64_t>("param"), 228);

        // Check the result
        std::vector<double> ref = {0.0, 1/1000.0};
        auto res = ArrayView<double>(static_cast<const double*>(env.debugSink().front().payload().data()),
                                     env.debugSink().front().payload().size() / sizeof(double));
        EXPECT_EQUAL(res.size(), ref.size());
        EXPECT(res.isApproximatelyEqual(ref, 10 * std::numeric_limits<double>::epsilon()));
    }
}

CASE("local-to-wmo mapping with missing value") {
    const auto plan = R"json({
        "name": "average_rate_test",
        "actions": [
            {
                "type": "scale",
                "preset-mappings": "local-to-wmo"
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
            {"param", 228},
            {"misc-precision", "double"},
            {"missingValue", 999.0}
        });
        std::vector<double> values = {0.0, 999.0, 1.0};
        auto pl = eckit::Buffer(values.data(), values.size() * sizeof(double));
        auto msg = Message({Message::Tag::Field, {}, {}, std::move(md)}, std::move(pl));
        EXPECT_NO_THROW(env.process(std::move(msg)));
        EXPECT_EQUAL(env.debugSink().size(), 1);
        EXPECT(env.debugSink().front().tag() == Message::Tag::Field);
    }

    {   // Check the correctness of the output field message
        auto md = env.debugSink().front().metadata();
        EXPECT_EQUAL(md.get<std::int64_t>("param"), 228228);

        // Check the result
        std::vector<double> ref = {0.0, 999.0, 1000.0};
        auto res = ArrayView<double>(static_cast<const double*>(env.debugSink().front().payload().data()),
                                     env.debugSink().front().payload().size() / sizeof(double));
        EXPECT_EQUAL(res.size(), ref.size());
        EXPECT(res.isApproximatelyEqual(ref, 10 * std::numeric_limits<double>::epsilon()));
    }
}

CASE("fake preset") {
    const auto plan = R"json({
        "name": "average_rate_test",
        "actions": [
            {
                "type": "scale",
                "preset-mappings": "foo-to-bar"
            },
            {
                "type": "debug-sink"
            }
        ]
    })json";

    EXPECT_THROWS(auto env = MultioTestEnvironment(plan));
}

}  // multio::test::scale

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

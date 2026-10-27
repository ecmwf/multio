#include "eckit/testing/Test.h"

#include "../../MultioTestEnvironment.h"

namespace multio::test::scale {

using eckit::testing::ArrayView;
using multio::message::Message;
using multio::message::Metadata;
using multio::test::MultioTestEnvironment;

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

    {  // Send a single input field message
        auto md = Metadata({{"param", 228}, {"misc-precision", "double"}});
        std::vector<double> values = {0.0, 1.0};
        auto pl = eckit::Buffer(values.data(), values.size() * sizeof(double));
        auto msg = Message({Message::Tag::Field, {}, {}, std::move(md)}, std::move(pl));
        EXPECT_NO_THROW(env.process(std::move(msg)));
        EXPECT_EQUAL(env.debugSink().size(), 1);
        EXPECT(env.debugSink().front().tag() == Message::Tag::Field);
    }

    {  // Check the correctness of the output field message
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

    {  // Send a single input field message
        auto md = Metadata({{"param", 228}, {"misc-precision", "double"}});
        std::vector<double> values = {0.0, 1.0};
        auto pl = eckit::Buffer(values.data(), values.size() * sizeof(double));
        auto msg = Message({Message::Tag::Field, {}, {}, std::move(md)}, std::move(pl));
        EXPECT_NO_THROW(env.process(std::move(msg)));
        EXPECT_EQUAL(env.debugSink().size(), 1);
        EXPECT(env.debugSink().front().tag() == Message::Tag::Field);
    }

    {  // Check the correctness of the output field message
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

    {  // Send a single input field message
        auto md = Metadata({{"param", 228228}, {"misc-precision", "double"}});
        std::vector<double> values = {0.0, 1.0};
        auto pl = eckit::Buffer(values.data(), values.size() * sizeof(double));
        auto msg = Message({Message::Tag::Field, {}, {}, std::move(md)}, std::move(pl));
        EXPECT_NO_THROW(env.process(std::move(msg)));
        EXPECT_EQUAL(env.debugSink().size(), 1);
        EXPECT(env.debugSink().front().tag() == Message::Tag::Field);
    }

    {  // Check the correctness of the output field message
        auto md = env.debugSink().front().metadata();
        EXPECT_EQUAL(md.get<std::int64_t>("param"), 228);

        // Check the result
        std::vector<double> ref = {0.0, 1 / 1000.0};
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

    {  // Send a single input field message
        auto md = Metadata(
            {{"param", 228}, {"misc-precision", "double"}, {"misc-bitmapPresent", true}, {"misc-missingValue", 999.0}});
        std::vector<double> values = {0.0, 999.0, 1.0};
        auto pl = eckit::Buffer(values.data(), values.size() * sizeof(double));
        auto msg = Message({Message::Tag::Field, {}, {}, std::move(md)}, std::move(pl));
        EXPECT_NO_THROW(env.process(std::move(msg)));
        EXPECT_EQUAL(env.debugSink().size(), 1);
        EXPECT(env.debugSink().front().tag() == Message::Tag::Field);
    }

    {  // Check the correctness of the output field message
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

namespace {

struct ScaledField {
    std::int64_t param;
    std::vector<double> values;
};

// Send a single field message through a plan consisting of a scale action using the given preset
// followed by a debug sink and return the param and the values of the message that reached the sink.
ScaledField scaleWithPreset(const std::string& preset, std::int64_t param, const std::vector<double>& values) {
    const auto plan = R"json({
        "name": "scale_preset_test",
        "actions": [
            {
                "type": "scale",
                "preset-mappings": ")json"
                    + preset + R"json("
            },
            {
                "type": "debug-sink"
            }
        ]
    })json";

    auto env = MultioTestEnvironment(plan);
    EXPECT_EQUAL(env.debugSink().size(), 0);

    auto md = Metadata({{"param", param}, {"misc-precision", "double"}});
    auto pl = eckit::Buffer(values.data(), values.size() * sizeof(double));
    auto msg = Message({Message::Tag::Field, {}, {}, std::move(md)}, std::move(pl));
    EXPECT_NO_THROW(env.process(std::move(msg)));
    EXPECT_EQUAL(env.debugSink().size(), 1);
    EXPECT(env.debugSink().front().tag() == Message::Tag::Field);

    const auto& out = env.debugSink().front();
    const auto* data = static_cast<const double*>(out.payload().data());
    return {out.metadata().get<std::int64_t>("param"), {data, data + out.payload().size() / sizeof(double)}};
}

void expectScaled(const std::string& preset, std::int64_t paramIn, const std::vector<double>& valuesIn,
                  std::int64_t paramOut, const std::vector<double>& valuesOut) {
    const auto res = scaleWithPreset(preset, paramIn, valuesIn);
    EXPECT_EQUAL(res.param, paramOut);
    EXPECT_EQUAL(res.values.size(), valuesOut.size());
    EXPECT(ArrayView<double>(res.values.data(), res.values.size())
               .isApproximatelyEqual(valuesOut, 10 * std::numeric_limits<double>::epsilon()));
}

}  // namespace

CASE("local-to-wmo mapping converts local params only") {
    // Local params are mapped to the WMO param and scaled
    expectScaled("local-to-wmo", 174, {0.0, 0.5}, 260509, {0.0, 50.0});    // albedo: fraction -> %
    expectScaled("local-to-wmo", 198, {0.0, 0.5}, 160198, {0.0, 500.0});   // skin reservoir: m -> kg m-2
    expectScaled("local-to-wmo", 228, {0.0, 1.0}, 228228, {0.0, 1000.0});  // total precipitation: m -> kg m-2
}

CASE("local-to-wmo mapping leaves params already in WMO units untouched") {
    // Fields that already carry the WMO param must pass through unchanged, they must not be mapped back
    expectScaled("local-to-wmo", 260509, {0.0, 50.0}, 260509, {0.0, 50.0});
    expectScaled("local-to-wmo", 160198, {0.0, 500.0}, 160198, {0.0, 500.0});
    expectScaled("local-to-wmo", 228228, {0.0, 1000.0}, 228228, {0.0, 1000.0});
}

CASE("wmo-to-local mapping converts WMO params only") {
    // WMO params are mapped back to the local param and scaled with the inverse factor
    expectScaled("wmo-to-local", 160198, {0.0, 500.0}, 198, {0.0, 0.5});
    expectScaled("wmo-to-local", 228228, {0.0, 1000.0}, 228, {0.0, 1.0});

    // Fields that already carry the local param must pass through unchanged
    expectScaled("wmo-to-local", 174, {0.0, 0.5}, 174, {0.0, 0.5});
    expectScaled("wmo-to-local", 198, {0.0, 0.5}, 198, {0.0, 0.5});
    expectScaled("wmo-to-local", 228, {0.0, 1.0}, 228, {0.0, 1.0});
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

}  // namespace multio::test::scale

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

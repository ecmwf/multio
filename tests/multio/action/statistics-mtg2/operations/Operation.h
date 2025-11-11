#pragma once

#include <optional>
#include <random>

#include "../../../MultioTestEnvironment.h"
#include "eckit/testing/Test.h"


inline constexpr std::size_t SIZE = 4096;


namespace multio::test::statistics_mtg2 {

using multio::test::MultioTestEnvironment;
using multio::message::Message;
using multio::message::Metadata;
using eckit::testing::ArrayView;

template <typename ElemType>
class StatisticsOperationTest {
public:
    using SinglePointOverTime = std::vector<ElemType>;
    using SpatialData = std::vector<ElemType>;
    using SpatialDataOverTime = std::vector<SpatialData>;

    StatisticsOperationTest(
        const std::string &name,
        const ElemType tolerance = 10 * std::numeric_limits<ElemType>::epsilon()
    ) : name_{name}, tolerance_{tolerance} {};

    void runSingle() {
        const std::string plan = getPlan();
        auto env = MultioTestEnvironment(plan);
        EXPECT_EQUAL(env.debugSink().size(), 0);

        // Initial values + single field at 21st july
        auto pls = SpatialDataOverTime(2);
        std::int64_t step = 0;
        for (std::size_t i = 0; i < 2; ++i) {
            pls[i] = getPayload(SIZE, step);
            EXPECT_NO_THROW(env.process(getMessage(pls[i], step, 2020'07'21)));
            step += 24;
        }
        EXPECT_EQUAL(env.debugSink().size(), 0);

        // Flush last-step should trigger emitting the statistics message
        EXPECT_NO_THROW(env.process({{Message::Tag::Flush, {}, {}, {{"flushKind", "last-step"}}}}));
        EXPECT_EQUAL(env.debugSink().size(), 2);

        // Check the values
        auto ref = reference(pls);
        auto res = ArrayView<ElemType>(static_cast<ElemType const *>(env.debugSink().front().payload().data()),
                                     env.debugSink().front().payload().size() / sizeof(ElemType));
        EXPECT_EQUAL(res.size(), SIZE);
        EXPECT(res.isApproximatelyEqual(ref, tolerance_));

        // Check the metadata
        EXPECT_EQUAL(24, env.debugSink().front().metadata().get<std::int64_t>("step"));
        if (name_ == "instant") {
            EXPECT(std::nullopt == env.debugSink().front().metadata().getOpt<std::int64_t>("timespan"));
        } else {
            EXPECT_EQUAL(24, env.debugSink().front().metadata().get<std::int64_t>("timespan"));
        }
    }

    void runMultipleUnaligned() {
        const std::string plan = getPlan();
        auto env = MultioTestEnvironment(plan);
        EXPECT_EQUAL(env.debugSink().size(), 0);

        // Send 45 messages stating from 21st july
        auto pls = SpatialDataOverTime(45);
        std::int64_t step = 0;
        for (std::size_t i = 0; i < 45; ++i) {
            pls[i] = getPayload(SIZE, step);
            EXPECT_NO_THROW(env.process(getMessage(pls[i], step, 2020'07'21)));
            step += 24;
        }
        EXPECT_EQUAL(env.debugSink().size(), 2);

        // Flush last-step should trigger emitting the last statistics message
        EXPECT_NO_THROW(env.process({{Message::Tag::Flush, {}, {}, {{"flushKind", "last-step"}}}}));
        EXPECT_EQUAL(env.debugSink().size(), 4);

        // Check the results
        {   // July (11 days)
            // Check the values
            auto ref = reference(pls, 1, 12);
            auto res = ArrayView<ElemType>(static_cast<ElemType const *>(env.debugSink().front().payload().data()),
                                        env.debugSink().front().payload().size() / sizeof(ElemType));
            EXPECT_EQUAL(res.size(), SIZE);
            EXPECT(res.isApproximatelyEqual(ref, tolerance_));

            // Check the metadata
            EXPECT_EQUAL(264, env.debugSink().front().metadata().get<std::int64_t>("step"));
            if (name_ == "instant") {
                EXPECT(std::nullopt == env.debugSink().front().metadata().getOpt<std::int64_t>("timespan"));
            } else {
                EXPECT_EQUAL(264, env.debugSink().front().metadata().get<std::int64_t>("timespan"));
            }

            env.debugSink().pop();
        }
        {   // August (31 days)
            // Check the values
            auto ref = reference(pls, 12, 43);
            auto res = ArrayView<ElemType>(static_cast<ElemType const *>(env.debugSink().front().payload().data()),
                                        env.debugSink().front().payload().size() / sizeof(ElemType));
            EXPECT_EQUAL(res.size(), SIZE);
            EXPECT(res.isApproximatelyEqual(ref, tolerance_));

            // Check the metadata
            EXPECT_EQUAL(1008, env.debugSink().front().metadata().get<std::int64_t>("step"));
            if (name_ == "instant") {
                EXPECT(std::nullopt == env.debugSink().front().metadata().getOpt<std::int64_t>("timespan"));
            } else {
                EXPECT_EQUAL(744, env.debugSink().front().metadata().get<std::int64_t>("timespan"));
            }

            env.debugSink().pop();
        }
        {   // September (2 days)
            // Check the values
            auto ref = reference(pls, 43, 45);
            auto res = ArrayView<ElemType>(static_cast<ElemType const *>(env.debugSink().front().payload().data()),
                                        env.debugSink().front().payload().size() / sizeof(ElemType));
            EXPECT_EQUAL(res.size(), SIZE);
            EXPECT(res.isApproximatelyEqual(ref, tolerance_));

            // Check the metadata
            EXPECT_EQUAL(1056, env.debugSink().front().metadata().get<std::int64_t>("step"));
            if (name_ == "instant") {
                EXPECT(std::nullopt == env.debugSink().front().metadata().getOpt<std::int64_t>("timespan"));
            } else {
                EXPECT_EQUAL(48, env.debugSink().front().metadata().get<std::int64_t>("timespan"));
            }

            env.debugSink().pop();
        }
    }

    void runMultipleAligned() {
        const std::string plan = getPlan();
        auto env = MultioTestEnvironment(plan);
        EXPECT_EQUAL(env.debugSink().size(), 0);

        // Send 93 messages stating from 1st october
        auto pls = SpatialDataOverTime(93);
        std::int64_t step = 0;
        for (std::size_t i = 0; i < 93; ++i) {
            pls[i] = getPayload(SIZE, step);
            EXPECT_NO_THROW(env.process(getMessage(pls[i], step, 2025'10'01)));
            step += 24;
        }
        EXPECT_EQUAL(env.debugSink().size(), 2);

        // Flush last-step should trigger emitting the last statistics message
        EXPECT_NO_THROW(env.process({{Message::Tag::Flush, {}, {}, {{"flushKind", "last-step"}}}}));
        EXPECT_EQUAL(env.debugSink().size(), 4);

        // Check the results
        {   // October (31 days)
            // Check the values
            auto ref = reference(pls, 1, 32);
            auto res = ArrayView<ElemType>(static_cast<ElemType const *>(env.debugSink().front().payload().data()),
                                        env.debugSink().front().payload().size() / sizeof(ElemType));
            EXPECT_EQUAL(res.size(), SIZE);
            EXPECT(res.isApproximatelyEqual(ref, tolerance_));

            // Check the metadata
            EXPECT_EQUAL(744, env.debugSink().front().metadata().get<std::int64_t>("step"));
            if (name_ == "instant") {
                EXPECT(std::nullopt == env.debugSink().front().metadata().getOpt<std::int64_t>("timespan"));
            } else {
                EXPECT_EQUAL(744, env.debugSink().front().metadata().get<std::int64_t>("timespan"));
            }

            env.debugSink().pop();
        }
        {   // November (30 days)
            // Check the values
            auto ref = reference(pls, 32, 62);
            auto res = ArrayView<ElemType>(static_cast<ElemType const *>(env.debugSink().front().payload().data()),
                                        env.debugSink().front().payload().size() / sizeof(ElemType));
            EXPECT_EQUAL(res.size(), SIZE);
            EXPECT(res.isApproximatelyEqual(ref, tolerance_));

            // Check the metadata
            EXPECT_EQUAL(1464, env.debugSink().front().metadata().get<std::int64_t>("step"));
            if (name_ == "instant") {
                EXPECT(std::nullopt == env.debugSink().front().metadata().getOpt<std::int64_t>("timespan"));
            } else {
                EXPECT_EQUAL(720, env.debugSink().front().metadata().get<std::int64_t>("timespan"));
            }

            env.debugSink().pop();
        }
        {   // December (31 days)
            // Check the values
            auto ref = reference(pls, 62, 93);
            auto res = ArrayView<ElemType>(static_cast<ElemType const *>(env.debugSink().front().payload().data()),
                                        env.debugSink().front().payload().size() / sizeof(ElemType));
            EXPECT_EQUAL(res.size(), SIZE);
            EXPECT(res.isApproximatelyEqual(ref, tolerance_));

            // Check the metadata
            EXPECT_EQUAL(2208, env.debugSink().front().metadata().get<std::int64_t>("step"));
            if (name_ == "instant") {
                EXPECT(std::nullopt == env.debugSink().front().metadata().getOpt<std::int64_t>("timespan"));
            } else {
                EXPECT_EQUAL(744, env.debugSink().front().metadata().get<std::int64_t>("timespan"));
            }

            env.debugSink().pop();
        }
    }

protected:
    // The output of the operation is checked against the implementation of
    // this reference method. The 'input' is a vector of values over time
    // in the same spatial point. The last value from the previous window
    // is given as 'init'.
    virtual ElemType reference(const SinglePointOverTime &input, const ElemType init) = 0;

private:
    const std::string name_;
    const ElemType tolerance_;

    SpatialData reference(const SpatialDataOverTime& input) {
        return reference(input, 1, input.size());
    }

    SpatialData reference(const SpatialDataOverTime& input, std::size_t start, std::size_t stop) {
        const std::size_t steps = input.size();
        EXPECT(start > 0 && start <= stop && stop <= steps && steps != 0);

        const std::size_t size = input[start].size();
        EXPECT_NOT_EQUAL(size, 0);
        for (std::size_t i = start - 1; i < stop; ++i) {
            EXPECT_EQUAL(input[i].size(), size);
        }

        auto output = SpatialData(size);
        auto column = SinglePointOverTime(stop - start);
        for (std::size_t i = 0; i < size; ++i) {
            for (std::size_t j = 0; j < (stop - start); ++j) {
                column[j] = input[start+j][i];
            }
            output[i] = reference(column, input[start-1][i]);
        }

        return output;
    }

    std::string getPlan() {
        return "{ \"name\": \"operation_" + name_ + "_test\", "
                "\"actions\": [ { "
                "\"type\": \"statistics-mtg2\", "
                "\"output-frequency\": \"1m\", "
                "\"operations\": [ \"" + name_ + "\" ], "
                "\"options\": { \"initial-condition-present\": true,"
                "               \"disable-strict-mapping\": true } },"
                "{ \"type\": \"debug-sink\" } ] }";
    }

    Message getMessage(SpatialData payload, std::int64_t step, std::int64_t date, std::int64_t time = 00'00'00) {
        static_assert(std::is_same_v<ElemType, float> || std::is_same_v<ElemType, double>, "type must be float or double");

        auto md = Metadata({
            {"param", 130},
            {"levtype", "sfc"},
            {"grid", "custom"},
            {"date", date},
            {"time", time},
            {"step", step},
            {"misc-precision", std::is_same_v<ElemType, float> ? "single" : "double"}
        });
        auto pl = eckit::Buffer(payload.data(), payload.size() * sizeof(ElemType));
        return Message({Message::Tag::Field, {}, {}, std::move(md)}, std::move(pl));
    }

    SpatialData getPayload(std::size_t size, std::size_t seed, ElemType min = 183.95, ElemType max = 329.85) {
        std::mt19937 gen(seed);
        SpatialData v(size);
        std::uniform_real_distribution<ElemType> dis(min, max);
        std::transform(v.begin(), v.end(), v.begin(), [&dis, &gen](ElemType val) { return dis(gen); });
        return v;
    }

};

}  // namespace multio::test::statistics_mtg2

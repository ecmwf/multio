#pragma once

#include <random>

#include "../../../MultioTestEnvironment.h"


inline constexpr std::size_t SIZE = 4096;


namespace multio::test::statistics_mtg2 {

using multio::test::MultioTestEnvironment;
using multio::message::Message;
using multio::message::Metadata;
using eckit::testing::ArrayView;

class StatisticsOperationTest {
public:
    using SinglePointOverTime = std::vector<double>;
    using SpatialData = std::vector<double>;
    using SpatialDataOverTime = std::vector<SpatialData>;

    StatisticsOperationTest(const std::string &name) : name_{name} {};

    void runSingle() {
        const std::string plan = getPlan();
        auto env = MultioTestEnvironment(plan);
        EXPECT_EQUAL(env.debugSink().size(), 0);

        // Initial values + single field at 21st july
        auto pls = SpatialDataOverTime(2);
        std::int64_t step = 0;
        for (std::size_t i = 0; i < 2; ++i) {
            pls[i] = getPayload(SIZE, step);
            EXPECT_NO_THROW(env.plan().process(getMessage(pls[i], step)));
            step += 24;
        }
        EXPECT_EQUAL(env.debugSink().size(), 0);

        // Flush last-step should trigger emitting the statistics message
        EXPECT_NO_THROW(env.plan().process({{Message::Tag::Flush, {}, {}, {{"flushKind", "last-step"}}}}));
        EXPECT_EQUAL(env.debugSink().size(), 2);

        // Check the result
        auto ref = reference(pls);
        auto res = ArrayView<double>(static_cast<double const *>(env.debugSink().front().payload().data()),
                                     env.debugSink().front().payload().size() / sizeof(double));
        EXPECT_EQUAL(res.size(), SIZE);
        EXPECT(res.isApproximatelyEqual(ref, std::numeric_limits<double>::epsilon()));
    }

    void runMultiple() {
        const std::string plan = getPlan();
        auto env = MultioTestEnvironment(plan);
        EXPECT_EQUAL(env.debugSink().size(), 0);

        // Send 45 messages stating from 21st july
        auto pls = SpatialDataOverTime(45);
        std::int64_t step = 0;
        for (std::size_t i = 0; i < 45; ++i) {
            pls[i] = getPayload(SIZE, step);
            EXPECT_NO_THROW(env.plan().process(getMessage(pls[i], step)));
            step += 24;
        }
        EXPECT_EQUAL(env.debugSink().size(), 2);

        // Flush last-step should trigger emitting the last statistics message
        EXPECT_NO_THROW(env.plan().process({{Message::Tag::Flush, {}, {}, {{"flushKind", "last-step"}}}}));
        EXPECT_EQUAL(env.debugSink().size(), 4);

        // Check the results
        {   // July (11 days)
            auto ref = reference(pls, 1, 12);
            auto res = ArrayView<double>(static_cast<double const *>(env.debugSink().front().payload().data()),
                                        env.debugSink().front().payload().size() / sizeof(double));
            EXPECT_EQUAL(res.size(), SIZE);
            EXPECT(res.isApproximatelyEqual(ref, 11 * std::numeric_limits<double>::epsilon()));
            env.debugSink().pop();
        }
        {   // August (31 days)
            auto ref = reference(pls, 12, 43);
            auto res = ArrayView<double>(static_cast<double const *>(env.debugSink().front().payload().data()),
                                        env.debugSink().front().payload().size() / sizeof(double));
            EXPECT_EQUAL(res.size(), SIZE);
            EXPECT(res.isApproximatelyEqual(ref, 31 * std::numeric_limits<double>::epsilon()));
            env.debugSink().pop();
        }
        {   // September (2 days)
            auto ref = reference(pls, 43, 45);
            auto res = ArrayView<double>(static_cast<double const *>(env.debugSink().front().payload().data()),
                                        env.debugSink().front().payload().size() / sizeof(double));
            EXPECT_EQUAL(res.size(), SIZE);
            EXPECT(res.isApproximatelyEqual(ref, 2 * std::numeric_limits<double>::epsilon()));
            env.debugSink().pop();
        }
    }

protected:
    virtual double reference(const SinglePointOverTime &input, const double init) = 0;

private:
    const std::string name_;

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
                "\"options\": { \"initial-condition-present\": \"true\", \"disable-strict-mapping\": \"true\" } },"
                "{ \"type\": \"debug-sink\" } ] }";
    }

    Message getMessage(SpatialData payload, std::int64_t step) {
        auto md = Metadata({
            {"param", 130},
            {"levtype", "sfc"},
            {"grid", "custom"},
            {"startDate", 20200721},
            {"startTime", 0000},
            {"step", step},
            {"misc-precision", "double"}
        });
        auto pl = eckit::Buffer(payload.data(), payload.size() * sizeof(double));
        return Message({Message::Tag::Field, {}, {}, std::move(md)}, std::move(pl));
    }

    SpatialData getPayload(std::size_t size, std::size_t seed, double min = 183.95, double max = 329.85) {
        std::mt19937 gen(seed);
        SpatialData v(size);
        std::uniform_real_distribution<double> dis(min, max);
        std::transform(v.begin(), v.end(), v.begin(), [&dis, &gen](double val) { return dis(gen); });
        return v;
    }

};

}  // namespace multio::test::statistics_mtg2

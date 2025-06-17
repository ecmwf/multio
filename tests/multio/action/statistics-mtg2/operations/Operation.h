#pragma once

#include <random>

#include "../../../MultioTestEnvironment.h"


#define SIZE 4096
#define TOLERANCE 0.000001


namespace multio::test::statistics_mtg2 {

using multio::test::MultioTestEnvironment;
using multio::message::Message;
using multio::message::Metadata;
using eckit::testing::ArrayView;

class StatisticsOperationTest {
public:
    StatisticsOperationTest(const std::string &name) : name_{name} {};

    virtual double reference(const std::vector<double> &input) = 0;

    std::vector<double> reference(const std::vector<std::vector<double>>& input) {
        return reference(input, 0, input.size());
    }

    std::vector<double> reference(const std::vector<std::vector<double>>& input, std::size_t start, std::size_t stop) {
        const std::size_t steps = input.size();
        EXPECT(start <= stop && stop <= steps && steps != 0);

        const std::size_t size = input[start].size();
        EXPECT_NOT_EQUAL(size, 0);
        for (std::size_t i = start; i < stop; ++i) {
            EXPECT_EQUAL(input[i].size(), size);
        }

        auto output = std::vector<double>(size);
        auto column = std::vector<double>(stop - start);
        for (std::size_t i = 0; i < size; ++i) {
            for (std::size_t j = 0; j < (stop - start); ++j) {
                column[j] = input[start+j][i];
            }
            output[i] = reference(column);
        }

        return output;
    }

    void runSingle() {
        const std::string plan = getPlan();
        auto env = MultioTestEnvironment(plan);
        EXPECT_EQUAL(env.debugSink().size(), 0);

        // Single field message at 21st july
        EXPECT_NO_THROW(env.plan().process(getMessage(getPayload(SIZE, 0), 0)));
        auto pl = getPayload(SIZE, 1);
        EXPECT_NO_THROW(env.plan().process(getMessage(pl, 1)));
        EXPECT_EQUAL(env.debugSink().size(), 0);

        // Flush last-step should trigger emitting the statistics message
        EXPECT_NO_THROW(env.plan().process({{Message::Tag::Flush, {}, {}, {{"flushKind", "last-step"}}}}));
        EXPECT_EQUAL(env.debugSink().size(), 2);

        // Check the result
        auto ref = reference(std::vector<std::vector<double>>{pl});
        auto res = ArrayView<double>(static_cast<double const *>(env.debugSink().front().payload().data()),
                                     env.debugSink().front().payload().size() / sizeof(double));
        EXPECT_EQUAL(res.size(), SIZE);
        EXPECT(res.isApproximatelyEqual(ref, TOLERANCE));
    }

    void runMultiple() {
        const std::string plan = getPlan();
        auto env = MultioTestEnvironment(plan);
        EXPECT_EQUAL(env.debugSink().size(), 0);

        // Send 45 messages stating from 21st july
        EXPECT_NO_THROW(env.plan().process(getMessage(getPayload(SIZE, 0), 0)));  // First one doesn't count :^)
        auto pls = std::vector<std::vector<double>>(45);
        std::int64_t step = 0;
        for (std::size_t i = 0; i < 45; ++i) {
            step += 24;
            pls[i] = getPayload(SIZE, step);
            EXPECT_NO_THROW(env.plan().process(getMessage(pls[i], step)));
        }
        EXPECT_EQUAL(env.debugSink().size(), 2);

        // Flush last-step should trigger emitting the last statistics message
        EXPECT_NO_THROW(env.plan().process({{Message::Tag::Flush, {}, {}, {{"flushKind", "last-step"}}}}));
        EXPECT_EQUAL(env.debugSink().size(), 4);

        // Check the results
        {   // July (11 days)
            auto ref = reference(pls, 0, 11);
            auto res = ArrayView<double>(static_cast<double const *>(env.debugSink().front().payload().data()),
                                        env.debugSink().front().payload().size() / sizeof(double));
            EXPECT_EQUAL(res.size(), SIZE);
            EXPECT(res.isApproximatelyEqual(ref, TOLERANCE));
            env.debugSink().pop();
        }
        {   // August (31 days)
            auto ref = reference(pls, 11, 42);
            auto res = ArrayView<double>(static_cast<double const *>(env.debugSink().front().payload().data()),
                                        env.debugSink().front().payload().size() / sizeof(double));
            EXPECT_EQUAL(res.size(), SIZE);
            EXPECT(res.isApproximatelyEqual(ref, TOLERANCE));
            env.debugSink().pop();
        }
        {   // September (3 days)
            auto ref = reference(pls, 42, 45);
            auto res = ArrayView<double>(static_cast<double const *>(env.debugSink().front().payload().data()),
                                        env.debugSink().front().payload().size() / sizeof(double));
            EXPECT_EQUAL(res.size(), SIZE);
            EXPECT(res.isApproximatelyEqual(ref, TOLERANCE));
            env.debugSink().pop();
        }
    }

private:
    const std::string name_;

    std::string getPlan() {
        return "{ \"name\": \"operation_" + name_ + "_test\", "
                "\"actions\": [ { "
                "\"type\": \"statistics-mtg2\", "
                "\"output-frequency\": \"1m\", "
                "\"operations\": [ \"" + name_ + "\" ], "
                "\"options\": { \"initial-condition-present\": \"true\", \"disable-strict-mapping\": \"true\" } },"
                "{ \"type\": \"debug-sink\" } ] }";
    }

    Message getMessage(std::vector<double> payload, std::int64_t step) {
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

    std::vector<double> getPayload(std::size_t size, std::size_t seed, double min = 183.95, double max = 329.85) {
        std::mt19937 gen(seed);
        std::vector<double> v(size);
        std::uniform_real_distribution<double> dis(min, max);
        std::transform(v.begin(), v.end(), v.begin(), [&dis, &gen](double val) { return dis(gen); });
        return v;
    }

};

}  // namespace multio::test::statistics_mtg2

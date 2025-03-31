/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */


/// @author Kevin Nobel


#include "eckit/testing/Test.h"

#include "testStatisticsUtils.h"


namespace multio::test {


template <typename T>
std::vector<T> expectedPayloadDifference(const std::vector<T>& first, const std::vector<T>& last) {
    std::vector<T> expected(first.size());
    std::transform(first.begin(), first.end(), last.begin(), expected.begin(), [](T vf, T vl) { return vl - vf; });
    return expected;
}


const std::string actionsDifference(
    R"json({
        "name": "Test statistics difference",
        "actions" : [
            {
                "type": "statistics",
                "output-frequency": "1d",
                "operations": [ "difference" ],
                "options": {
                    "use-current-time": "false",
                    "step-frequency": 1,
                    "time-step": 3600,
                    "initial-condition-present": "true",
                    "solver-reset-accumulate-fields-every": "never"
                }
            },
            {
                "type": "debug-sink"
            }
        ]
    })json");


CASE("Statistics Action Difference Static") {
    const size_t payloadSize = 10;
    auto env = MultioTestEnvironment(actionsDifference);

    std::vector<double> payloadFirst{{0, 1, 2, 3, 4, 5, 6, 7, 8, 9}};
    std::vector<double> payloadLast{{9, 8, 7, 6, 5, 4, 3, 2, 1, 0}};
    auto payloadExpected = expectedPayloadDifference<double>(payloadFirst, payloadLast);

    EXPECT_NO_THROW(env.plan().process(createStatisticsMessage(0, payloadFirst)));
    for (size_t step = 1; step <= 23; ++step) {
        auto payloadRandom = randomVector<double>(10, 0, 10, step);
        EXPECT_NO_THROW(env.plan().process(createStatisticsMessage(step, payloadRandom)));
    }
    EXPECT_EQUAL(env.debugSink().size(), 0);
    EXPECT_NO_THROW(env.plan().process(createStatisticsMessage(24, payloadLast)));
    EXPECT_EQUAL(env.debugSink().size(), 1);

    auto& payload = env.debugSink().front().payload();
    EXPECT_EQUAL(payloadSize, payload.size() / sizeof(double));
    auto payloadData = static_cast<const double*>(payload.data());

    for (size_t i = 0; i < payloadSize; ++i) {
        EXPECT_EQUAL(payloadData[i], payloadExpected[i]);
    }
};


CASE("Statistics Action Difference Random") {
    const size_t payloadSize = 4096;
    auto env = MultioTestEnvironment(actionsDifference);

    std::vector<std::vector<double>> payloads;
    for (size_t step = 0; step <= 24; ++step) {
        payloads.push_back(randomVector<double>(payloadSize, 0, 10, step));
    }
    auto payloadExpected = expectedPayloadDifference(payloads[0], payloads[payloads.size() - 1]);

    for (size_t step = 0; step <= 23; ++step) {
        EXPECT_NO_THROW(env.plan().process(createStatisticsMessage(step, payloads[step])));
    }
    EXPECT_EQUAL(env.debugSink().size(), 0);
    EXPECT_NO_THROW(env.plan().process(createStatisticsMessage(24, payloads[24])));
    EXPECT_EQUAL(env.debugSink().size(), 1);

    auto& payload = env.debugSink().front().payload();
    EXPECT_EQUAL(payloadSize, payload.size() / sizeof(double));
    auto payloadData = static_cast<const double*>(payload.data());

    for (size_t i = 0; i < payloadSize; ++i) {
        EXPECT_EQUAL(payloadData[i], payloadExpected[i]);
    }
};


CASE("Statistics Action Difference Random Multiple Windows") {
    const size_t payloadSize = 4096;
    auto env = MultioTestEnvironment(actionsDifference);

    std::vector<std::vector<double>> payloads;
    for (size_t step = 0; step <= 48; ++step) {
        payloads.push_back(randomVector<double>(payloadSize, 0, 10, step));
    }
    auto firstPayloadExpected = expectedPayloadDifference<double>(payloads[0], payloads[24]);
    auto secondPayloadExpected = expectedPayloadDifference<double>(payloads[24], payloads[48]);

    // Run first window
    {
        for (size_t step = 0; step <= 23; ++step) {
            EXPECT_NO_THROW(env.plan().process(createStatisticsMessage(step, payloads[step])));
        }
        EXPECT_EQUAL(env.debugSink().size(), 0);
        EXPECT_NO_THROW(env.plan().process(createStatisticsMessage(24, payloads[24])));
        EXPECT_EQUAL(env.debugSink().size(), 1);

        auto& payload = env.debugSink().front().payload();
        EXPECT_EQUAL(payloadSize, payload.size() / sizeof(double));
        auto payloadData = static_cast<const double*>(payload.data());

        for (size_t i = 0; i < payloadSize; ++i) {
            EXPECT_EQUAL(payloadData[i], firstPayloadExpected[i]);
        }
    }

    env.debugSink().pop();  // Empty the queue!

    // Run second window (continue)
    {
        for (size_t step = 25; step <= 47; ++step) {
            EXPECT_NO_THROW(env.plan().process(createStatisticsMessage(step, payloads[step])));
        }
        EXPECT_EQUAL(env.debugSink().size(), 0);
        EXPECT_NO_THROW(env.plan().process(createStatisticsMessage(48, payloads[48])));
        EXPECT_EQUAL(env.debugSink().size(), 1);

        auto& payload = env.debugSink().front().payload();
        EXPECT_EQUAL(payloadSize, payload.size() / sizeof(double));
        auto payloadData = static_cast<const double*>(payload.data());

        for (size_t i = 0; i < payloadSize; ++i) {
            EXPECT_EQUAL(payloadData[i], secondPayloadExpected[i]);
        }
    }
};


}  // namespace multio::test

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

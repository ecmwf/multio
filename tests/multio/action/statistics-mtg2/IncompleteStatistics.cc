/*
 * (C) Copyright 2026- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "eckit/io/Buffer.h"
#include "eckit/testing/Test.h"

#include <sstream>

#include "../../MultioTestEnvironment.h"

namespace multio::test::statistics_mtg2 {

using multio::message::Message;
using multio::message::Metadata;
using multio::test::MultioTestEnvironment;

MultioTestEnvironment makeEnvironment(bool emitIncomplete = false, bool allowNonUniform = false) {
    std::ostringstream plan;
    plan << R"json({
        "name": "incomplete-statistics",
        "actions": [
            {
                "type": "statistics-mtg2",
                "output-frequency": "1d",
                "operations": [ "average" ],
                "options": {
                    "emit-incomplete-statistics": )json"
         << (emitIncomplete ? "true" : "false") << R"json(,
                    "allow-non-uniform-statistics": )json"
         << (allowNonUniform ? "true" : "false") << R"json(
                }
            },
            { "type": "debug-sink" }
        ]
    })json";
    return MultioTestEnvironment{plan.str()};
}

void startSimulation(MultioTestEnvironment& env) {
    env.process(
        {{Message::Tag::Flush, {}, {}, {{"flushKind", "first-step"}, {"date", 2026'01'01}, {"time", 0}, {"step", 0}}}});
    env.debugSink().pop();
}

void sendField(MultioTestEnvironment& env, std::int64_t step, std::int64_t distance = 3600) {
    Metadata md{{{"param", 130},
                 {"levtype", "sfc"},
                 {"grid", "none"},
                 {"date", 2026'01'01},
                 {"time", 0},
                 {"step", step},
                 {"misc-outputStepInSeconds", 3600},
                 {"misc-integrationStepInSeconds", 600},
                 {"misc-distanceFromPreviousStepInSeconds", distance},
                 {"misc-precision", "double"}}};
    double value = static_cast<double>(step);
    env.process({{Message::Tag::Field, {}, {}, std::move(md)}, eckit::Buffer{&value, sizeof(value)}});
}

void flushLastStep(MultioTestEnvironment& env) {
    env.process({{Message::Tag::Flush, {}, {}, {{"flushKind", "last-step"}}}});
}

CASE("complete window is emitted") {
    auto env = makeEnvironment();
    startSimulation(env);
    for (std::int64_t step = 1; step <= 24; ++step) {
        sendField(env, step);
    }
    const auto beforeFlush = env.debugSink().size();
    EXPECT_NO_THROW(flushLastStep(env));
    EXPECT_EQUAL(env.debugSink().size(), beforeFlush + 2);
    EXPECT_EQUAL(env.debugSink().front().metadata().get<std::int64_t>("misc-distanceFromPreviousStepInSeconds"), 86400);
}

CASE("trailing partial window is suppressed by default") {
    auto env = makeEnvironment();
    startSimulation(env);
    for (std::int64_t step = 1; step <= 12; ++step) {
        sendField(env, step);
    }
    const auto beforeFlush = env.debugSink().size();
    EXPECT_NO_THROW(flushLastStep(env));
    EXPECT_EQUAL(env.debugSink().size(), beforeFlush + 1);
}

CASE("trailing partial window can be emitted") {
    auto env = makeEnvironment(true);
    startSimulation(env);
    for (std::int64_t step = 1; step <= 12; ++step) {
        sendField(env, step);
    }
    const auto beforeFlush = env.debugSink().size();
    EXPECT_NO_THROW(flushLastStep(env));
    EXPECT_EQUAL(env.debugSink().size(), beforeFlush + 2);
    EXPECT_EQUAL(env.debugSink().front().metadata().get<std::int64_t>("timespan"), 24);
}

CASE("internal gap makes a window incomplete") {
    auto env = makeEnvironment();
    startSimulation(env);
    for (std::int64_t step = 1; step <= 24; ++step) {
        if (step != 12) {
            sendField(env, step);
        }
    }
    const auto beforeFlush = env.debugSink().size();
    EXPECT_NO_THROW(flushLastStep(env));
    EXPECT_EQUAL(env.debugSink().size(), beforeFlush + 1);
}

CASE("non-uniform input is rejected by default") {
    auto env = makeEnvironment();
    startSimulation(env);
    sendField(env, 1, 3600);
    sendField(env, 3, 7200);
    EXPECT_THROWS(flushLastStep(env));
}

CASE("contiguous non-uniform input can be emitted") {
    auto env = makeEnvironment(false, true);
    startSimulation(env);
    sendField(env, 1, 3600);
    for (std::int64_t step = 3; step <= 23; step += 2) {
        sendField(env, step, 7200);
    }
    sendField(env, 24, 3600);
    EXPECT_NO_THROW(flushLastStep(env));
    EXPECT_EQUAL(env.debugSink().size(), 2);
}

}  // namespace multio::test::statistics_mtg2

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

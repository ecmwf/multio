/*
 * (C) Copyright 2026- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

// A (re)started run without initial condition sends its first field one output step after the simulation start.
// The window must still be reported as starting at the simulation start, not at the first field.

#include "eckit/io/Buffer.h"
#include "eckit/testing/Test.h"

#include "../../MultioTestEnvironment.h"

namespace multio::test::statistics_mtg2 {

using multio::message::Message;
using multio::message::Metadata;
using multio::test::MultioTestEnvironment;

MultioTestEnvironment makeEnvironment() {
    return MultioTestEnvironment{R"json({
        "name": "restart-without-initial-condition",
        "actions": [
            {
                "type": "statistics-mtg2",
                "output-frequency": "1d",
                "operations": [ "average" ],
                "options": {
                    "initial-condition-present": false,
                    "output-time-reference": "start-of-window"
                }
            },
            { "type": "debug-sink" }
        ]
    })json"};
}

void startSimulation(MultioTestEnvironment& env, std::int64_t step) {
    env.process({{Message::Tag::Flush,
                  {},
                  {},
                  {{"flushKind", "first-step"}, {"date", 2026'01'01}, {"time", 0}, {"step", step}}}});
    env.debugSink().pop();
}

void sendField(MultioTestEnvironment& env, std::int64_t step) {
    Metadata md{{{"param", 130},
                 {"levtype", "sfc"},
                 {"grid", "none"},
                 {"date", 2026'01'01},
                 {"time", 0},
                 {"step", step},
                 {"misc-outputStepInSeconds", 3600},
                 {"misc-integrationStepInSeconds", 600},
                 {"misc-distanceFromPreviousStepInSeconds", 3600},
                 {"misc-precision", "double"}}};
    double value = static_cast<double>(step);
    env.process({{Message::Tag::Field, {}, {}, std::move(md)}, eckit::Buffer{&value, sizeof(value)}});
}

void flushLastStep(MultioTestEnvironment& env) {
    env.process({{Message::Tag::Flush, {}, {}, {{"flushKind", "last-step"}}}});
}

void checkDailyWindow(MultioTestEnvironment& env, std::int64_t date, double average) {
    EXPECT_EQUAL(env.debugSink().size(), 2);
    const auto& msg = env.debugSink().front();
    const auto& md = msg.metadata();
    EXPECT_EQUAL(md.get<std::int64_t>("date"), date);
    EXPECT_EQUAL(md.get<std::int64_t>("time"), 0);
    EXPECT_EQUAL(md.get<std::int64_t>("step"), 24);
    EXPECT_EQUAL(md.get<std::int64_t>("timespan"), 24);
    EXPECT_EQUAL(static_cast<const double*>(msg.payload().data())[0], average);
}

CASE("cold start without initial condition reports the full first window") {
    auto env = makeEnvironment();
    startSimulation(env, 0);
    for (std::int64_t step = 1; step <= 24; ++step) {
        sendField(env, step);
    }
    EXPECT_NO_THROW(flushLastStep(env));
    checkDailyWindow(env, 2026'01'01, 12.5);
}

CASE("restart on a window boundary without initial condition reports the full first window") {
    auto env = makeEnvironment();
    startSimulation(env, 24);
    for (std::int64_t step = 25; step <= 48; ++step) {
        sendField(env, step);
    }
    EXPECT_NO_THROW(flushLastStep(env));
    checkDailyWindow(env, 2026'01'02, 36.5);
}

CASE("restart within a window suppresses the partial window and reports the next one") {
    auto env = makeEnvironment();
    startSimulation(env, 12);
    for (std::int64_t step = 13; step <= 48; ++step) {
        sendField(env, step);
    }
    EXPECT_NO_THROW(flushLastStep(env));
    checkDailyWindow(env, 2026'01'02, 36.5);
}

}  // namespace multio::test::statistics_mtg2

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

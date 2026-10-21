/*
 * (C) Copyright 2026- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// Tests of the statistics-mtg2 option `window-origin: forecast-start`: the statistic accumulates from the simulation
/// start and the running value is emitted at every `output-frequency`, with `timespan=fs`, instead of being closed and
/// reset after each output-frequency window.

#include <string>
#include <vector>

#include "eckit/io/Buffer.h"
#include "eckit/testing/Test.h"

#include "multio/message/Message.h"
#include "multio/message/Metadata.h"

#include "../../MultioTestEnvironment.h"
#include "StatisticsTestHelpers.h"

namespace multio::test::statistics_mtg2 {

using multio::message::Message;
using multio::message::Metadata;
using multio::test::MultioTestEnvironment;

namespace {

constexpr std::int64_t DATE = 20260101;
constexpr std::int64_t PARAM = 228143;  // total precipitation kg m**-2: accumulated under its own paramId

// The increment received at hour h is h, so the accumulation over 0..t is t(t+1)/2: 0-3 -> 6, 0-6 -> 21, 0-9 -> 45
double increment(std::int64_t hour) {
    return static_cast<double>(hour);
}

double accumulation(std::int64_t hours) {
    return static_cast<double>(hours * (hours + 1) / 2);
}

// `options` is a list of `"key": value,` entries (each with a trailing comma)
std::string makePlan(const std::string& operations, const std::string& options, const std::string& frequency = "3h") {
    return R"json({
        "name": "forecast-start-window",
        "actions": [
            {
                "type": "statistics-mtg2",
                "output-frequency": ")json"
         + frequency + R"json(",
                "operations": [ )json"
         + operations + R"json( ],
                "options": { )json"
         + options + R"json( "debug": false }
            },
            { "type": "debug-sink" }
        ]
    })json";
}

const std::string FORECAST_START = R"json( "window-origin": "forecast-start",)json";
const std::string IC_PRESENT = R"json( "initial-condition-present": true,)json";
const std::string EMIT_INCOMPLETE = R"json( "emit-incomplete-statistics": true,)json";

std::string forecastStart(const std::string& more = "") {
    return IC_PRESENT + FORECAST_START + more;
}

Metadata fieldMetadata(std::int64_t param, std::int64_t step) {
    Metadata md{{{"param", param},
                 {"levtype", "sfc"},
                 {"grid", "N80"},
                 {"date", DATE},
                 {"time", 0},
                 {"step", step},
                 {"misc-precision", "double"}}};
    setTimingMetadata(md, 3600);
    return md;
}

void sendField(MultioTestEnvironment& env, std::int64_t step, std::int64_t param = PARAM) {
    const double value = increment(step);
    env.process({{Message::Tag::Field, {}, {}, fieldMetadata(param, step)}, eckit::Buffer{&value, sizeof(value)}});
}

void flushLastStep(MultioTestEnvironment& env) {
    env.process({{Message::Tag::Flush, {}, {}, {{"flushKind", "last-step"}}}});
}

struct Emitted {
    double value;
    std::int64_t param;
    std::int64_t step;
    std::string timespan;
    std::int64_t distanceInSeconds;
};

// Removes and describes the field at the front of the sink
Emitted popEmitted(MultioTestEnvironment& env) {
    auto& sink = env.debugSink();
    EXPECT(!sink.empty());
    EXPECT(sink.front().tag() == Message::Tag::Field);
    const auto& md = sink.front().metadata();
    // timespan is the string "fs" or a number of hours
    const auto timespan = md.get("timespan").visit([](const auto& v) -> std::string {
        using Value = std::decay_t<decltype(v)>;
        if constexpr (std::is_same_v<Value, std::string>) {
            return v;
        }
        else if constexpr (std::is_integral_v<Value>) {
            return std::to_string(v);
        }
        else {
            return "unexpected type";
        }
    });
    Emitted e{*static_cast<const double*>(sink.front().payload().data()), md.get<std::int64_t>("param"),
              md.get<std::int64_t>("step"), timespan, md.get<std::int64_t>("misc-distanceFromPreviousStepInSeconds")};
    sink.pop();
    return e;
}

// The flush the simulation start left in the sink is already consumed by sendSimulationStart
void sendHours(MultioTestEnvironment& env, std::int64_t first, std::int64_t last, std::vector<Emitted>& emitted) {
    for (std::int64_t step = first; step <= last; ++step) {
        sendField(env, step);
        while (!env.debugSink().empty()) {
            emitted.push_back(popEmitted(env));
        }
    }
}

}  // namespace

CASE("forecast-start emits the running accumulation at every output step") {
    auto env = MultioTestEnvironment{makePlan(R"("accumulate")", forecastStart())};
    sendSimulationStart(env, DATE);

    std::vector<Emitted> emitted;
    // Step 0 is the initial condition; the increments of hours 1..9 follow
    for (std::int64_t step = 0; step <= 9; ++step) {
        sendField(env, step);
        const bool outputStep = step > 0 && step % 3 == 0;
        // The accumulation is available as soon as the output step has been received: no later field, no flush
        const std::size_t expectedSize = outputStep ? 1 : 0;
        EXPECT_EQUAL(env.debugSink().size(), expectedSize);
        if (outputStep) {
            emitted.push_back(popEmitted(env));
        }
    }

    EXPECT_EQUAL(emitted.size(), 3);
    const std::vector<std::int64_t> hours{3, 6, 9};
    for (std::size_t i = 0; i < emitted.size(); ++i) {
        const auto t = hours[i];
        EXPECT_EQUAL(emitted[i].value, accumulation(t));
        EXPECT_EQUAL(emitted[i].param, PARAM);
        EXPECT_EQUAL(emitted[i].step, t);
        EXPECT_EQUAL(emitted[i].timespan, std::string{"fs"});
        EXPECT_EQUAL(emitted[i].distanceInSeconds, t * 3600);
    }
    // 0-3: 1+2+3, 0-6: 1+..+6, 0-9: 1+..+9
    EXPECT_EQUAL(emitted[0].value, 6.0);
    EXPECT_EQUAL(emitted[1].value, 21.0);
    EXPECT_EQUAL(emitted[2].value, 45.0);

    // The forecast ended on an output step: nothing is left open, a last-step flush adds only the flush itself
    EXPECT_NO_THROW(flushLastStep(env));
    EXPECT_EQUAL(env.debugSink().size(), 1);
    EXPECT(env.debugSink().front().tag() == Message::Tag::Flush);
}

CASE("forecast-start with an output frequency of one hour emits every step") {
    auto env = MultioTestEnvironment{makePlan(R"("accumulate")", forecastStart(), "1h")};
    sendSimulationStart(env, DATE);
    sendField(env, 0);

    std::vector<Emitted> emitted;
    sendHours(env, 1, 4, emitted);
    EXPECT_EQUAL(emitted.size(), 4);
    for (std::size_t i = 0; i < emitted.size(); ++i) {
        const auto t = static_cast<std::int64_t>(i) + 1;
        EXPECT_EQUAL(emitted[i].value, accumulation(t));
        EXPECT_EQUAL(emitted[i].step, t);
        EXPECT_EQUAL(emitted[i].distanceInSeconds, t * 3600);
    }
}

CASE("forecast-start emits the open tail once when the forecast ends between output steps") {
    auto env = MultioTestEnvironment{makePlan(R"("accumulate")", forecastStart())};
    sendSimulationStart(env, DATE);
    sendField(env, 0);

    std::vector<Emitted> emitted;
    sendHours(env, 1, 7, emitted);
    // 0-3 and 0-6 were emitted on the way, hour 7 is still open
    EXPECT_EQUAL(emitted.size(), 2);
    EXPECT_EQUAL(emitted[1].value, accumulation(6));

    EXPECT_NO_THROW(flushLastStep(env));
    EXPECT_EQUAL(env.debugSink().size(), 2);
    const auto tail = popEmitted(env);
    EXPECT_EQUAL(tail.value, accumulation(7));
    EXPECT_EQUAL(tail.step, 7);
    EXPECT_EQUAL(tail.timespan, std::string{"fs"});
    EXPECT_EQUAL(tail.distanceInSeconds, 7 * 3600);
    EXPECT(env.debugSink().front().tag() == Message::Tag::Flush);
    env.debugSink().pop();

    // A repeated flush has nothing left to emit
    EXPECT_NO_THROW(flushLastStep(env));
    EXPECT_EQUAL(env.debugSink().size(), 1);
    EXPECT(env.debugSink().front().tag() == Message::Tag::Flush);
}

CASE("forecast-start without a last-step flush leaves the tail unemitted") {
    auto env = MultioTestEnvironment{makePlan(R"("accumulate")", forecastStart())};
    sendSimulationStart(env, DATE);
    sendField(env, 0);
    std::vector<Emitted> emitted;
    sendHours(env, 1, 4, emitted);
    EXPECT_EQUAL(emitted.size(), 1);
    EXPECT_EQUAL(emitted[0].step, 3);
}

CASE("forecast-start works without the initial condition") {
    auto env = MultioTestEnvironment{makePlan(R"("accumulate")", FORECAST_START)};
    sendSimulationStart(env, DATE);
    std::vector<Emitted> emitted;
    sendHours(env, 1, 6, emitted);
    EXPECT_EQUAL(emitted.size(), 2);
    EXPECT_EQUAL(emitted[0].value, accumulation(3));
    EXPECT_EQUAL(emitted[1].value, accumulation(6));
}

CASE("forecast-start average, maximum and minimum are running statistics over the elapsed span") {
    // The increments are 1, 2, 3, ...: over 1..t the average is (t+1)/2, the maximum t and the minimum 1
    auto env = MultioTestEnvironment{
        makePlan(R"("average", "maximum", "minimum")", forecastStart(R"json( "disable-strict-mapping": true,)json"))};
    sendSimulationStart(env, DATE);
    sendField(env, 0, 130);

    for (std::int64_t step = 1; step <= 6; ++step) {
        const double value = increment(step);
        env.process({{Message::Tag::Field, {}, {}, fieldMetadata(130, step)}, eckit::Buffer{&value, sizeof(value)}});
        if (step % 3 != 0) {
            EXPECT_EQUAL(env.debugSink().size(), 0);
            continue;
        }
        EXPECT_EQUAL(env.debugSink().size(), 3);
        const auto average = popEmitted(env);
        const auto maximum = popEmitted(env);
        const auto minimum = popEmitted(env);
        EXPECT_EQUAL(average.value, (static_cast<double>(step) + 1.0) / 2.0);
        EXPECT_EQUAL(maximum.value, static_cast<double>(step));
        EXPECT_EQUAL(minimum.value, 1.0);
        for (const auto& e : {average, maximum, minimum}) {
            EXPECT_EQUAL(e.step, step);
            EXPECT_EQUAL(e.timespan, std::string{"fs"});
            EXPECT_EQUAL(e.distanceInSeconds, step * 3600);
        }
    }
}

CASE("forecast-start accumulates every field independently") {
    auto env = MultioTestEnvironment{makePlan(R"("accumulate")", forecastStart())};
    sendSimulationStart(env, DATE);
    for (const auto param : {228143, 3062}) {
        sendField(env, 0, param);
    }
    std::vector<std::int64_t> params;
    for (std::int64_t step = 1; step <= 3; ++step) {
        for (const auto param : {228143, 3062}) {
            sendField(env, step, param);
        }
    }
    EXPECT_EQUAL(env.debugSink().size(), 2);
    for (int i = 0; i < 2; ++i) {
        const auto e = popEmitted(env);
        EXPECT_EQUAL(e.value, 6.0);
        EXPECT_EQUAL(e.step, 3);
        params.push_back(e.param);
    }
    EXPECT_EQUAL(params[0], 228143);
    EXPECT_EQUAL(params[1], 3062);
}

CASE("forecast-start window opened after the forecast epoch reports its elapsed duration, not fs") {
    // A run that starts at step 6 without restored state accumulates from step 6: it must not claim 0-9
    auto env = MultioTestEnvironment{makePlan(R"("accumulate")", forecastStart())};
    sendSimulationStart(env, DATE, 0, 6);
    sendField(env, 6);

    std::vector<Emitted> emitted;
    sendHours(env, 7, 9, emitted);
    EXPECT_EQUAL(emitted.size(), 1);
    EXPECT_EQUAL(emitted[0].value, increment(7) + increment(8) + increment(9));
    EXPECT_EQUAL(emitted[0].step, 9);
    EXPECT_EQUAL(emitted[0].timespan, std::string{"3"});
    EXPECT_EQUAL(emitted[0].distanceInSeconds, 3 * 3600);
}

CASE("forecast-start suppresses accumulations with a missing hour unless incomplete statistics are requested") {
    {
        auto env = MultioTestEnvironment{makePlan(R"("accumulate")", forecastStart())};
        sendSimulationStart(env, DATE);
        sendField(env, 0);
        sendField(env, 1);
        sendField(env, 3);  // hour 2 is missing: the sum 0-3 is wrong
        EXPECT_EQUAL(env.debugSink().size(), 0);
        // the gap is permanent, later emissions are suppressed too
        sendField(env, 4);
        sendField(env, 5);
        sendField(env, 6);
        EXPECT_EQUAL(env.debugSink().size(), 0);
    }
    {
        auto env = MultioTestEnvironment{makePlan(R"("accumulate")", forecastStart(EMIT_INCOMPLETE))};
        sendSimulationStart(env, DATE);
        sendField(env, 0);
        sendField(env, 1);
        sendField(env, 3);
        EXPECT_EQUAL(env.debugSink().size(), 1);
        EXPECT_EQUAL(popEmitted(env).value, increment(1) + increment(3));
    }
}

CASE("forecast-start rejects input that skips an output step") {
    auto env = MultioTestEnvironment{makePlan(R"("accumulate")", forecastStart())};
    sendSimulationStart(env, DATE);
    sendField(env, 0);
    sendField(env, 1);
    sendField(env, 2);
    EXPECT_THROWS(sendField(env, 4));
}

CASE("forecast-start rejects unsupported configurations") {
    // Operations that re-baseline on compute
    EXPECT_THROWS(MultioTestEnvironment{makePlan(R"("difference")", forecastStart())});
    EXPECT_THROWS(MultioTestEnvironment{makePlan(R"("instant")", forecastStart())});
    // Calendar frequencies
    EXPECT_THROWS(MultioTestEnvironment{makePlan(R"("accumulate")", forecastStart(), "1d")});
    // Backward offset windows
    EXPECT_THROWS(MultioTestEnvironment{
        makePlan(R"("accumulate")", forecastStart(R"json( "window-type": "backward-offset",)json"))});
    EXPECT_THROWS(MultioTestEnvironment{
        makePlan(R"("accumulate")", forecastStart(R"json( "output-time-reference": "start-of-window",)json"))});
    // Unknown origin
    EXPECT_THROWS(MultioTestEnvironment{makePlan(R"("accumulate")", R"json( "window-origin": "elsewhere",)json")});

    // Statistical input
    auto env = MultioTestEnvironment{makePlan(R"("accumulate")", forecastStart())};
    sendSimulationStart(env, DATE);
    auto md = fieldMetadata(PARAM, 1);
    md.set("timespan", 1);
    md.set("misc-timeIncrementInSeconds", 3600);
    const double value = 1.0;
    EXPECT_THROWS(env.process({{Message::Tag::Field, {}, {}, std::move(md)}, eckit::Buffer{&value, sizeof(value)}}));
}

CASE("window-start is the default and unchanged: the window is closed by the next field and reset") {
    for (const std::string& origin : {std::string{}, std::string{R"json( "window-origin": "window-start",)json"}}) {
        auto env = MultioTestEnvironment{makePlan(R"("accumulate")", IC_PRESENT + origin)};
        sendSimulationStart(env, DATE);
        sendField(env, 0);

        std::vector<Emitted> emitted;
        sendHours(env, 1, 3, emitted);
        // Nothing before a field outside the window arrives
        EXPECT_EQUAL(emitted.size(), 0);
        sendHours(env, 4, 6, emitted);
        // Window 0-3 closes on hour 4 with its nominal length; 3-6 stays open (and complete) until hour 7
        EXPECT_EQUAL(emitted.size(), 1);
        EXPECT_EQUAL(emitted[0].value, accumulation(3));
        EXPECT_EQUAL(emitted[0].step, 3);
        EXPECT_EQUAL(emitted[0].timespan, std::string{"3"});
        EXPECT_EQUAL(emitted[0].distanceInSeconds, 3 * 3600);

        sendHours(env, 7, 7, emitted);
        EXPECT_EQUAL(emitted.size(), 2);
        // The second window is a 3-6 window: the reset accumulation is 4+5+6, not 1..6
        EXPECT_EQUAL(emitted[1].value, increment(4) + increment(5) + increment(6));
        EXPECT_EQUAL(emitted[1].step, 6);
        EXPECT_EQUAL(emitted[1].timespan, std::string{"3"});
    }
}

CASE("forecast-start restart continues the accumulation from the original forecast start") {
    const std::string restartOptions = R"json( "write-restart": true, "read-restart": false,
        "restart-path": ".", "restart-prefix": "ForecastStartDump", "restart-lib": "eckit_codec",
        "restart-time": "latest",)json";
    const std::string readOptions = R"json( "write-restart": false, "read-restart": true,
        "restart-path": ".", "restart-prefix": "ForecastStartDump", "restart-lib": "eckit_codec",
        "restart-time": "latest",)json";

    {
        auto env = MultioTestEnvironment{makePlan(R"("accumulate")", forecastStart(restartOptions))};
        sendSimulationStart(env, DATE);
        sendField(env, 0);
        std::vector<Emitted> emitted;
        sendHours(env, 1, 4, emitted);
        // 0-3 was emitted before the restart, hour 4 is part of the open tail stored in the restart
        EXPECT_EQUAL(emitted.size(), 1);
        EXPECT_EQUAL(emitted[0].value, accumulation(3));

        EXPECT_NO_THROW(env.process({{Message::Tag::Flush, {}, {}, {{"flushKind", "step-and-restart"}, {"step", 4}}}}));
        EXPECT_EQUAL(env.debugSink().size(), 1);
        EXPECT(env.debugSink().front().tag() == Message::Tag::Flush);
        env.debugSink().pop();
    }

    {
        auto env = MultioTestEnvironment{makePlan(R"("accumulate")", forecastStart(readOptions))};
        sendSimulationStart(env, DATE);

        std::vector<Emitted> emitted;
        // Hour 4 may be sent again by the solver after the restart: it is recognised as already accumulated
        sendHours(env, 4, 7, emitted);
        EXPECT_EQUAL(emitted.size(), 1);
        // 0-6 contains the hours accumulated before the restart: the origin is the original forecast start
        EXPECT_EQUAL(emitted[0].value, accumulation(6));
        EXPECT_EQUAL(emitted[0].step, 6);
        EXPECT_EQUAL(emitted[0].timespan, std::string{"fs"});
        EXPECT_EQUAL(emitted[0].distanceInSeconds, 6 * 3600);

        EXPECT_NO_THROW(flushLastStep(env));
        EXPECT_EQUAL(env.debugSink().size(), 2);
        const auto tail = popEmitted(env);
        EXPECT_EQUAL(tail.value, accumulation(7));
        EXPECT_EQUAL(tail.step, 7);
        EXPECT_EQUAL(tail.timespan, std::string{"fs"});
    }
}

}  // namespace multio::test::statistics_mtg2

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

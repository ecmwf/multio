/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */


/// @author Kevin Nobel


#include "eckit/io/Buffer.h"
#include "eckit/testing/Test.h"

#include "../../MultioTestEnvironment.h"

namespace multio::test::statistics_mtg2 {

using multio::message::Message;
using multio::message::Metadata;
using multio::test::MultioTestEnvironment;

std::int64_t triangleSum(const std::int64_t n) {
    return (n * (n + 1)) / 2;
}

CASE("hourly -> monthly accumulated") {
    const auto plan = R"json({
        "name": "hourly to monthly",
        "actions": [
            {
                "type": "statistics-mtg2",
                "output-frequency": "1m",
                "operations": [ "accumulate" ],
                "options": {
                    "initial-condition-present": true
                }
            },
            {
                "type": "debug-sink"
            }
        ]
    })json";
    auto env = MultioTestEnvironment(plan);
    EXPECT_EQUAL(env.debugSink().size(), 0);

    for (std::int64_t step = 0; step <= 24*(31+30+31); ++step) {
        auto md = Metadata({{"param", 228228},  // Total Precipitation
                            {"levtype", "sfc"},
                            {"grid", "none"},
                            {"date", 1996'10'01},
                            {"time", 00'00'00},
                            {"step", step},
                            {"timespan", (step == 0 ? 0 : 1)},
                            {"misc-precision", "double"}});

        auto data = static_cast<double>(step);
        auto pl = eckit::Buffer{&data, sizeof(double)};
        auto msg = Message({Message::Tag::Field, {}, {}, std::move(md)}, std::move(pl));
        // auto msg = Message({Message::Tag::Field, {}, {}, std::move(md)});
        EXPECT_NO_THROW(env.process(std::move(msg)));
    }
    EXPECT_EQUAL(env.debugSink().size(), 2);

    // Send a flush last-step to trigger emitting the statistics message
    EXPECT_NO_THROW(env.process({{Message::Tag::Flush, {}, {}, {{"flushKind", "last-step"}}}}));
    EXPECT_EQUAL(env.debugSink().size(), 4);

    {
        EXPECT_EQUAL(sizeof(double), env.debugSink().front().size());
        auto pl = static_cast<const double*>(env.debugSink().front().payload().data());
        EXPECT_EQUAL(triangleSum(744), *pl);  // 1 + 2 + ... + 743 + 744

        auto md = env.debugSink().front().metadata();
        EXPECT_EQUAL(228228, md.get<std::int64_t>("param"));
        EXPECT_EQUAL(24*31, md.get<std::int64_t>("step"));
        EXPECT_EQUAL(24*31, md.get<std::int64_t>("timespan"));
        EXPECT(std::nullopt == md.getOpt<std::string>("stattype"));
        env.debugSink().pop();
    }

    {
        EXPECT_EQUAL(sizeof(double), env.debugSink().front().size());
        auto pl = static_cast<const double*>(env.debugSink().front().payload().data());
        EXPECT_EQUAL(triangleSum(1464) - triangleSum(744), *pl);  // 746 + 747 + ... + 1463 + 1464

        auto md = env.debugSink().front().metadata();
        EXPECT_EQUAL(228228, md.get<std::int64_t>("param"));
        EXPECT_EQUAL(24*(31+30), md.get<std::int64_t>("step"));
        EXPECT_EQUAL(24*30, md.get<std::int64_t>("timespan"));
        EXPECT(std::nullopt == md.getOpt<std::string>("stattype"));
        env.debugSink().pop();
    }

    {
        EXPECT_EQUAL(sizeof(double), env.debugSink().front().size());
        auto pl = static_cast<const double*>(env.debugSink().front().payload().data());
        EXPECT_EQUAL(triangleSum(2208) - triangleSum(1464), *pl);  // 1465 + 1466 + ... + 2207 + 2208

        auto md = env.debugSink().front().metadata();
        EXPECT_EQUAL(228228, md.get<std::int64_t>("param"));
        EXPECT_EQUAL(24*(31+30+31), md.get<std::int64_t>("step"));
        EXPECT_EQUAL(24*31, md.get<std::int64_t>("timespan"));
        EXPECT(std::nullopt == md.getOpt<std::string>("stattype"));
        env.debugSink().pop();
    }
}

CASE("monthly -> hourly accumulated") {
    const auto plan = R"json({
        "name": "monthly to hourly",
        "actions": [
            {
                "type": "statistics-mtg2",
                "output-frequency": "1h",
                "operations": [ "difference" ],
                "options": {
                    "initial-condition-present": true
                }
            },
            {
                "type": "debug-sink"
            }
        ]
    })json";
    auto env = MultioTestEnvironment(plan);
    EXPECT_EQUAL(env.debugSink().size(), 0);

    for (std::int64_t step = 0; step <= 24*(31+30+31); ++step) {
        std::int64_t resetStep = (step > 1464 ? 1464 : (step > 744 ? 744 : 0));
        auto md = Metadata({{"param", 228228},  // Total Precipitation
                            {"levtype", "sfc"},
                            {"grid", "none"},
                            {"date", 1996'10'01},
                            {"time", 00'00'00},
                            {"step", step},
                            {"timespan", step - resetStep},
                            {"misc-precision", "double"}});

        auto data = static_cast<double>(step - resetStep);
        auto pl = eckit::Buffer{&data, sizeof(double)};
        auto msg = Message({Message::Tag::Field, {}, {}, std::move(md)}, std::move(pl));
        EXPECT_NO_THROW(env.process(std::move(msg)));
    }
    EXPECT_EQUAL(env.debugSink().size(), 2207);

    // Send a flush last-step to trigger emitting the statistics message
    EXPECT_NO_THROW(env.process({{Message::Tag::Flush, {}, {}, {{"flushKind", "last-step"}}}}));
    EXPECT_EQUAL(env.debugSink().size(), 2209);

    for (std::int64_t step = 1; step <= 24*(31+30+31); ++step) {
        std::cout << "Checking step=" << step << std::endl;
        EXPECT_EQUAL(sizeof(double), env.debugSink().front().size());
        auto pl = static_cast<const double*>(env.debugSink().front().payload().data());
        EXPECT_EQUAL(1, *pl);

        auto md = env.debugSink().front().metadata();
        EXPECT_EQUAL(228228, md.get<std::int64_t>("param"));
        EXPECT_EQUAL(step, md.get<std::int64_t>("step"));
        EXPECT_EQUAL(1, md.get<std::int64_t>("timespan"));
        EXPECT(std::nullopt == md.getOpt<std::string>("stattype"));
        env.debugSink().pop();
    }
}


}  // namespace multio::test::statistics_mtg2

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

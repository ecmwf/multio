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


#include <optional>
#include "eckit/testing/Test.h"

#include "../../MultioTestEnvironment.h"

namespace multio::test::statistics_mtg2 {

using multio::message::Message;
using multio::message::Metadata;
using multio::test::MultioTestEnvironment;

CASE("Average rate + daily average + monthly average") {
    const auto plan = R"json({
        "name": "statistics_average_high_test",
        "actions": [
            {
                "type": "average-rate"
            },
            {
                "type": "statistics-mtg2",
                "output-frequency": "1d",
                "operations": [ "average" ],
                "options": {
                    "initial-condition-present": false
                }
            },
            {
                "type": "statistics-mtg2",
                "output-frequency": "1m",
                "operations": [ "average" ],
                "options": {
                    "initial-condition-present": false
                }
            },
            {
                "type": "debug-sink"
            }
        ]
    })json";
    auto env = MultioTestEnvironment(plan);
    EXPECT_EQUAL(env.debugSink().size(), 0);

    for (std::int64_t step = 1; step <= 24*(31+30+31); ++step) {
        auto md = Metadata({{"param", 228228},  // Total Precipitation
                            {"levtype", "sfc"},
                            {"grid", "none"},
                            {"date", 1996'10'01},
                            {"time", 00'00'00},
                            {"step", step},
                            {"timespan", 1},
                            {"misc-precision", "double"}});

        auto msg = Message({Message::Tag::Field, {}, {}, std::move(md)});
        EXPECT_NO_THROW(env.process(std::move(msg)));
    }
    EXPECT_EQUAL(env.debugSink().size(), 2);

    // Send a flush last-step to trigger emitting the statistics message
    EXPECT_NO_THROW(env.process({{Message::Tag::Flush, {}, {}, {{"flushKind", "last-step"}}}}));
    EXPECT_EQUAL(env.debugSink().size(), 4);

    {
        auto md = env.debugSink().front().metadata();
        EXPECT_EQUAL(235055, md.get<std::int64_t>("param"));  // Average Precipitation Rate
        EXPECT_EQUAL(24*31, md.get<std::int64_t>("step"));
        EXPECT_EQUAL(24*31, md.get<std::int64_t>("timespan"));
        EXPECT(std::nullopt == md.getOpt<std::string>("stattype"));  // No stattype because of squashing
        env.debugSink().pop();
    }

    {
        auto md = env.debugSink().front().metadata();
        EXPECT_EQUAL(235055, md.get<std::int64_t>("param"));
        EXPECT_EQUAL(24*(31+30), md.get<std::int64_t>("step"));
        EXPECT_EQUAL(24*30, md.get<std::int64_t>("timespan"));
        EXPECT(std::nullopt == md.getOpt<std::string>("stattype"));
        env.debugSink().pop();
    }

    {
        auto md = env.debugSink().front().metadata();
        EXPECT_EQUAL(235055, md.get<std::int64_t>("param"));
        EXPECT_EQUAL(24*(31+30+31), md.get<std::int64_t>("step"));
        EXPECT_EQUAL(24*31, md.get<std::int64_t>("timespan"));
        EXPECT(std::nullopt == md.getOpt<std::string>("stattype"));
        env.debugSink().pop();
    }
}

CASE("Average rate + daily average + monthly maximum") {
    const auto plan = R"json({
        "name": "statistics_average_high_test",
        "actions": [
            {
                "type": "average-rate"
            },
            {
                "type": "statistics-mtg2",
                "output-frequency": "1d",
                "operations": [ "average" ],
                "options": {
                    "initial-condition-present": false
                }
            },
            {
                "type": "statistics-mtg2",
                "output-frequency": "1m",
                "operations": [ "maximum" ],
                "options": {
                    "initial-condition-present": false
                }
            },
            {
                "type": "debug-sink"
            }
        ]
    })json";
    auto env = MultioTestEnvironment(plan);
    EXPECT_EQUAL(env.debugSink().size(), 0);

    for (std::int64_t step = 1; step <= 24*(31+30+31); ++step) {
        auto md = Metadata({{"param", 228228},  // Total Precipitation
                            {"levtype", "sfc"},
                            {"grid", "none"},
                            {"date", 19961001},
                            {"time", 0000},
                            {"step", step},
                            {"timespan", 1},
                            {"misc-precision", "double"}});

        auto msg = Message({Message::Tag::Field, {}, {}, std::move(md)});
        EXPECT_NO_THROW(env.process(std::move(msg)));
    }
    EXPECT_EQUAL(env.debugSink().size(), 2);

    // Send a flush last-step to trigger emitting the statistics message
    EXPECT_NO_THROW(env.process({{Message::Tag::Flush, {}, {}, {{"flushKind", "last-step"}}}}));
    // EXPECT_EQUAL(env.debugSink().size(), 4);

    {
        auto md = env.debugSink().front().metadata();
        EXPECT_EQUAL(235055, md.get<std::int64_t>("param"));  // Average Precipitation Rate
        EXPECT_EQUAL(24*31, md.get<std::int64_t>("step"));
        EXPECT_EQUAL(24, md.get<std::int64_t>("timespan"));  // Timespan from daily average
        EXPECT_EQUAL("momx", md.get<std::string>("stattype"));
        env.debugSink().pop();
    }

        {
        auto md = env.debugSink().front().metadata();
        EXPECT_EQUAL(235055, md.get<std::int64_t>("param"));
        EXPECT_EQUAL(24*(31+30), md.get<std::int64_t>("step"));
        EXPECT_EQUAL(24, md.get<std::int64_t>("timespan"));
        EXPECT_EQUAL("momx", md.get<std::string>("stattype"));
        env.debugSink().pop();
    }

    {
        auto md = env.debugSink().front().metadata();
        EXPECT_EQUAL(235055, md.get<std::int64_t>("param"));
        EXPECT_EQUAL(24*(31+30+31), md.get<std::int64_t>("step"));
        EXPECT_EQUAL(24, md.get<std::int64_t>("timespan"));
        EXPECT_EQUAL("momx", md.get<std::string>("stattype"));
        env.debugSink().pop();
    }
}

}  // namespace multio::test::statistics_mtg2

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

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

#include "../../MultioTestEnvironment.h"

namespace multio::test::statistics_mtg2 {

using multio::message::Message;
using multio::message::Metadata;
using multio::test::MultioTestEnvironment;

CASE("Squash daily + monthly average") {
    const auto plan = R"json({
        "name": "statistics_average_high_test",
        "actions": [
            {
                "type": "statistics-mtg2",
                "output-frequency": "1d",
                "operations": [ "average" ],
                "options": {
                    "initial-condition-present": true
                }
            },
            {
                "type": "statistics-mtg2",
                "output-frequency": "1m",
                "operations": [ "average" ],
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

    for (std::int64_t step = 0; step <= 2208; ++step) {
        auto md = Metadata({{"param", 167},
                            {"levtype", "sfc"},
                            {"grid", "none"},
                            {"date", 19961001},
                            {"time", 0000},
                            {"step", step},
                            {"misc-precision", "double"}});

        auto msg = Message({Message::Tag::Field, {}, {}, std::move(md)});
        EXPECT_NO_THROW(env.process(std::move(msg)));
    }
    EXPECT_EQUAL(env.debugSink().size(), 2);

    // Send a flush last-step to trigger emitting the statistics message
    EXPECT_NO_THROW(env.process({{Message::Tag::Flush, {}, {}, {{"flushKind", "last-step"}}}}));
    EXPECT_EQUAL(env.debugSink().size(), 4);

    {   // October (31 days)
        auto md = env.debugSink().front().metadata();
        EXPECT_EQUAL(228004, md.get<std::int64_t>("param"));
        EXPECT_EQUAL(744, md.get<std::int64_t>("step"));
        EXPECT_EQUAL(744, md.get<std::int64_t>("timespan"));
        env.debugSink().pop();
    }
    {   // November (31 days)
        auto md = env.debugSink().front().metadata();
        EXPECT_EQUAL(228004, md.get<std::int64_t>("param"));
        EXPECT_EQUAL(1464, md.get<std::int64_t>("step"));
        EXPECT_EQUAL(720, md.get<std::int64_t>("timespan"));
        env.debugSink().pop();
    }
    {   // December (31 days)
        auto md = env.debugSink().front().metadata();
        EXPECT_EQUAL(228004, md.get<std::int64_t>("param"));
        EXPECT_EQUAL(2208, md.get<std::int64_t>("step"));
        EXPECT_EQUAL(744, md.get<std::int64_t>("timespan"));
        env.debugSink().pop();
    }
}

}  // namespace multio::test::statistics_mtg2

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

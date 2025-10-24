
#include "eckit/testing/Test.h"

#include "../../MultioTestEnvironment.h"

namespace multio::test::statistics_mtg2 {

using multio::message::Message;
using multio::message::Metadata;
using multio::test::MultioTestEnvironment;

CASE("Monthly average of daily high temperature") {
    const auto plan = R"json({
        "name": "statistics_average_high_test",
        "actions": [
            {
                "type": "statistics-mtg2",
                "output-frequency": "1d",
                "operations": [ "maximum" ],
                "options": {
                    "initial-condition-present": true,
                    "disable-strict-mapping": true
                }
            },
            {
                "type": "statistics-mtg2",
                "output-frequency": "1m",
                "operations": [ "average" ],
                "options": {
                    "initial-condition-present": true,
                    "disable-strict-mapping": true
                }
            },
            {
                "type": "debug-sink"
            }
        ]
    })json";
    auto env = MultioTestEnvironment(plan);
    EXPECT_EQUAL(env.debugSink().size(), 0);

    for (std::int64_t step = 0; step <= 744; ++step) {
        auto md = Metadata({{"param", 167},
                            {"levtype", "sfc"},
                            {"grid", "none"},
                            {"date", 19961001},
                            {"time", 0000},
                            {"step", step},
                            {"misc-precision", "double"}});

        auto msg = Message({Message::Tag::Field, {}, {}, std::move(md)});
        EXPECT_NO_THROW(env.process(std::move(msg)));
        EXPECT_EQUAL(env.debugSink().size(), 0);
    }

    // Send a flush last-step to trigger emitting the statistics message
    EXPECT_NO_THROW(env.process({{Message::Tag::Flush, {}, {}, {{"flushKind", "last-step"}}}}));
    EXPECT_EQUAL(env.debugSink().size(), 2);

    auto md = env.debugSink().front().metadata();
    EXPECT_EQUAL(237167, md.get<std::int64_t>("param"));
    EXPECT_EQUAL(744, md.get<std::int64_t>("step"));
    EXPECT_EQUAL(24, md.get<std::int64_t>("timespan"));
    EXPECT_EQUAL("moav", md.get<std::string>("stattype"));
}

CASE("Montly average of daily high of average 3 hourly temperature") {
    const auto plan = R"json({
        "name": "statistics_average_high_test",
        "actions": [
            {
                "type": "statistics-mtg2",
                "output-frequency": "3h",
                "operations": [ "average" ],
                "options": {
                    "initial-condition-present": true,
                    "disable-strict-mapping": true
                }
            },
            {
                "type": "statistics-mtg2",
                "output-frequency": "1d",
                "operations": [ "maximum" ],
                "options": {
                    "initial-condition-present": true,
                    "disable-strict-mapping": true
                }
            },
            {
                "type": "statistics-mtg2",
                "output-frequency": "1m",
                "operations": [ "average" ],
                "options": {
                    "initial-condition-present": true,
                    "disable-strict-mapping": true,
                    "disable-squashing": true
                }
            },
            {
                "type": "debug-sink"
            }
        ]
    })json";
    auto env = MultioTestEnvironment(plan);
    EXPECT_EQUAL(env.debugSink().size(), 0);

    for (std::int64_t step = 0; step <= 744; ++step) {
        auto md = Metadata({{"param", 167},
                            {"levtype", "sfc"},
                            {"grid", "none"},
                            {"date", 19961001},
                            {"time", 0000},
                            {"step", step},
                            {"misc-precision", "double"}});

        auto msg = Message({Message::Tag::Field, {}, {}, std::move(md)});
        EXPECT_NO_THROW(env.process(std::move(msg)));
        EXPECT_EQUAL(env.debugSink().size(), 0);
    }

    // Send a flush last-step to trigger emitting the statistics message
    EXPECT_NO_THROW(env.process({{Message::Tag::Flush, {}, {}, {{"flushKind", "last-step"}}}}));
    EXPECT_EQUAL(env.debugSink().size(), 2);

    auto md = env.debugSink().front().metadata();
    EXPECT_EQUAL(228004, md.get<std::int64_t>("param"));
    EXPECT_EQUAL(744, md.get<std::int64_t>("step"));
    EXPECT_EQUAL(3, md.get<std::int64_t>("timespan"));
    EXPECT_EQUAL("moav_damx", md.get<std::string>("stattype"));
}

}  // namespace multio::test::statistics_mtg2

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

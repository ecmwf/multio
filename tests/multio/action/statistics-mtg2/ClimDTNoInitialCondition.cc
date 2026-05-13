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

#include "metkit/codes/api/CodesAPI.h"

#include "../../MultioTestEnvironment.h"

/// This file contains daily and monthly statistics.
/// This primarily tests current behaviour for ocean output.
///
/// Test param: 262100 -> 263100 (average)

namespace multio::test::statistics_mtg2 {

using multio::message::Message;
using multio::message::Metadata;
using multio::test::MultioTestEnvironment;

struct SampleParams {
    std::int64_t paramIn;
    std::int64_t paramOut;
    std::string levtype;
    std::int64_t levelist;
};


message::Metadata mkMd(const SampleParams& p, int64_t step) {
    return {{"param", p.paramIn},  // Total Precipitation
            {"levelist", p.levelist},
            {"date", 19880101},
            {"time", 0},
            {"step", step},
            {"levtype", p.levtype},
            {"grid", "eORCA1_T"},
            {"activity", "baseline"},
            {"class", "d1"},
            {"dataset", "climate-dt"},
            {"experiment", "hist"},
            {"expver", "j36u"},
            {"generation", 2},
            {"model", "IFS-NEMO"},
            {"realization", 1},
            {"resolution", "standard"},
            {"type", "fc"},
            {"packing", "ccsds"},
            {"misc-subCentre", 1003},
            {"misc-generatingProcessIdentifier", 156},
            {"misc-timeIncrementInSeconds", 3600},
            {"misc-precision", "double"}};
}

static const std::vector<SampleParams> params{{262100, 263100, "o2d", 1}, {262507, 263507, "o3d", 67}};

CASE("hourly to daily - no initial condition") {
    for (const auto& p : params) {
        const auto plan = R"json({
            "name": "hourly to daily",
            "actions": [
                {
                    "type": "statistics-mtg2",
                    "output-frequency": "1d",
                    "operations": [ "average" ],
                    "options": { 
                        "set-metadata": {
                          "stream": "clte" 
                        }
                    }
                },
                {
                    "type": "debug-sink"
                }
            ]
        })json";
        auto env = MultioTestEnvironment(plan);
        std::vector<double> payloadData(1024, 1.23);

        EXPECT_EQUAL(env.debugSink().size(), 0);

        // No initia
        for (std::int64_t step = 1; step <= 24 * (31 + 29 + 31); ++step) {
            auto md = mkMd(p, step);

            eckit::Buffer payload{payloadData.data(), sizeof(double) * payloadData.size()};
            Message msg{{Message::Tag::Field, {}, {}, std::move(md)}, std::move(payload)};
            EXPECT_NO_THROW(env.process(std::move(msg)));
        }

        // Expect last message not to be flushed, hence we substract -1
        EXPECT_EQUAL(env.debugSink().size(), 31 + 29 + 31 - 1);

        // Send a flush last-step to trigger emitting the statistics message
        EXPECT_NO_THROW(env.process({{Message::Tag::Flush, {}, {}, {{"flushKind", "last-step"}}}}));

        // Now we got our last message plus additional flush message
        EXPECT_EQUAL(env.debugSink().size(), 31 + 29 + 31 + 1);


        for (auto [month, daysInMonth] : std::vector<std::pair<int64_t, int64_t>>{{1, 31}, {2, 29}, {3, 31}}) {
            for (std::int64_t day = 1; day <= daysInMonth; ++day) {
                EXPECT_EQUAL(env.debugSink().front().payload().size() / sizeof(double), payloadData.size());

                auto md = env.debugSink().front().metadata();
                EXPECT_EQUAL(p.paramOut, md.get<std::int64_t>("param"));
                EXPECT_EQUAL(24, md.get<std::int64_t>("step"));
                EXPECT_EQUAL(24, md.get<std::int64_t>("timespan"));
                int64_t date = 19880000 + month * 100 + day;
                EXPECT_EQUAL(date, md.get<std::int64_t>("date"));
                EXPECT_EQUAL(0, md.get<std::int64_t>("time"));
                EXPECT_EQUAL("clte", md.get<std::string>("stream"));
                EXPECT(std::nullopt == md.getOpt<std::string>("stattype"));

                EXPECT_EQUAL(1003, md.get<int64_t>("misc-subCentre"));
                EXPECT_EQUAL(156, md.get<int64_t>("misc-generatingProcessIdentifier"));
                EXPECT_EQUAL(3600, md.get<int64_t>("misc-timeIncrementInSeconds"));

                env.debugSink().pop();
            }
        }

        EXPECT(env.debugSink().front().tag() == Message::Tag::Flush);
        env.debugSink().pop();
        EXPECT_EQUAL(env.debugSink().size(), 0);
    }
}


CASE("hourly to daily to monthly - no initial condition") {
    for (const auto& p : params) {
        const auto plan = R"json({
            "name": "hourly to daily",
            "actions": [
                {
                    "type": "statistics-mtg2",
                    "output-frequency": "1d",
                    "operations": [ "average" ],
                    "options": { 
                        "set-metadata": {
                          "stream": "clte" 
                        }
                    }
                },
                {
                    "type": "statistics-mtg2",
                    "output-frequency": "1m",
                    "operations": [ "average" ],
                    "options": { 
                        "set-metadata": {
                          "stream": "clmn",
                          "misc-timeIncrementInSeconds": 86400
                        }
                    }
                },
                {
                    "type": "debug-sink"
                }
            ]
        })json";
        auto env = MultioTestEnvironment(plan);
        std::vector<double> payloadData(1024, 1.23);

        EXPECT_EQUAL(env.debugSink().size(), 0);

        // No initia
        for (std::int64_t step = 1; step <= 24 * (31 + 29 + 31); ++step) {
            auto md = mkMd(p, step);

            eckit::Buffer payload{payloadData.data(), sizeof(double) * payloadData.size()};
            Message msg{{Message::Tag::Field, {}, {}, std::move(md)}, std::move(payload)};
            EXPECT_NO_THROW(env.process(std::move(msg)));
        }

        // Expect last message not to be flushed, hence we expect just two messages for two month
        EXPECT_EQUAL(env.debugSink().size(), 2);

        // Send a flush last-step to trigger emitting the statistics message
        EXPECT_NO_THROW(env.process({{Message::Tag::Flush, {}, {}, {{"flushKind", "last-step"}}}}));

        // Now we got our last message plus additional flush message
        EXPECT_EQUAL(env.debugSink().size(), 4);


        for (auto [month, daysInMonth] : std::vector<std::pair<int64_t, int64_t>>{{1, 31}, {2, 29}, {3, 31}}) {
            EXPECT_EQUAL(env.debugSink().front().payload().size() / sizeof(double), payloadData.size());

            auto md = env.debugSink().front().metadata();
            EXPECT_EQUAL(p.paramOut, md.get<std::int64_t>("param"));
            int64_t timespan = daysInMonth * 24;
            EXPECT_EQUAL(timespan, md.get<std::int64_t>("step"));
            EXPECT_EQUAL(timespan, md.get<std::int64_t>("timespan"));

            int64_t date = 19880001 + month * 100;
            EXPECT_EQUAL(date, md.get<std::int64_t>("date"));
            EXPECT_EQUAL(0, md.get<std::int64_t>("time"));
            EXPECT_EQUAL("clmn", md.get<std::string>("stream"));
            EXPECT(std::nullopt == md.getOpt<std::string>("stattype"));

            EXPECT_EQUAL(1003, md.get<int64_t>("misc-subCentre"));
            EXPECT_EQUAL(156, md.get<int64_t>("misc-generatingProcessIdentifier"));
            EXPECT_EQUAL(3600 * 24, md.get<int64_t>("misc-timeIncrementInSeconds"));
            env.debugSink().pop();
        }

        EXPECT(env.debugSink().front().tag() == Message::Tag::Flush);
        env.debugSink().pop();
        EXPECT_EQUAL(env.debugSink().size(), 0);
    }
}


CASE("hourly to daily - no initial condition - with encoding") {
    for (const auto& p : params) {
        const auto plan = R"json({
            "name": "hourly to daily",
            "actions": [
                {
                    "type": "statistics-mtg2",
                    "output-frequency": "1d",
                    "operations": [ "average" ],
                    "options": { 
                        "set-metadata": {
                          "stream": "clte" 
                        }
                    }
                },
                {
                    "type": "encode-mtg2",
                    "cached": true
                },
                {
                    "type": "debug-sink"
                }
            ]
        })json";
        auto env = MultioTestEnvironment(plan);
        std::vector<double> payloadData(1024, 1.23);

        EXPECT_EQUAL(env.debugSink().size(), 0);

        // No initia
        for (std::int64_t step = 1; step <= 24 * (31 + 29 + 31); ++step) {
            auto md = mkMd(p, step);

            eckit::Buffer payload{payloadData.data(), sizeof(double) * payloadData.size()};
            Message msg{{Message::Tag::Field, {}, {}, std::move(md)}, std::move(payload)};
            EXPECT_NO_THROW(env.process(std::move(msg)));
        }

        // Expect last message not to be flushed, hence we substract -1
        EXPECT_EQUAL(env.debugSink().size(), 31 + 29 + 31 - 1);

        // Send a flush last-step to trigger emitting the statistics message
        EXPECT_NO_THROW(env.process({{Message::Tag::Flush, {}, {}, {{"flushKind", "last-step"}}}}));

        // Now we got our last message plus additional flush message
        EXPECT_EQUAL(env.debugSink().size(), 31 + 29 + 31 + 1);


        for (auto [month, daysInMonth] : std::vector<std::pair<int64_t, int64_t>>{{1, 31}, {2, 29}, {3, 31}}) {
            for (std::int64_t day = 1; day <= daysInMonth; ++day) {
                auto codesHandle = metkit::codes::codesHandleFromMessage(
                    {static_cast<const uint8_t*>(env.debugSink().front().payload().data()),
                     env.debugSink().front().payload().size()});

                auto md = env.debugSink().front().metadata();
                EXPECT_EQUAL(p.paramOut, md.get<std::int64_t>("param"));
                EXPECT_EQUAL(p.paramOut, codesHandle->getLong("param"));
                EXPECT_EQUAL(24, md.get<std::int64_t>("step"));
                EXPECT_EQUAL(24, codesHandle->getLong("step"));
                int64_t date = 19880000 + month * 100 + day;
                EXPECT_EQUAL(date, md.get<std::int64_t>("date"));
                EXPECT_EQUAL(date, codesHandle->getLong("date"));
                EXPECT_EQUAL(0, md.get<std::int64_t>("time"));
                EXPECT_EQUAL(0, codesHandle->getLong("time"));
                EXPECT_EQUAL("clte", md.get<std::string>("stream"));
                EXPECT_EQUAL("clte", codesHandle->getString("stream"));
                EXPECT(std::nullopt == md.getOpt<std::string>("stattype"));

                EXPECT_EQUAL(1003, codesHandle->getLong("subCentre"));
                EXPECT_EQUAL(156, codesHandle->getLong("generatingProcessIdentifier"));
                EXPECT_EQUAL(3600, codesHandle->getLong("timeIncrement"));

                env.debugSink().pop();
            }
        }

        EXPECT(env.debugSink().front().tag() == Message::Tag::Flush);
        env.debugSink().pop();
        EXPECT_EQUAL(env.debugSink().size(), 0);
    }
}


CASE("hourly to daily to monthly - no initial condition - with encoding") {
    for (const auto& p : params) {
        const auto plan = R"json({
            "name": "hourly to daily",
            "actions": [
                {
                    "type": "statistics-mtg2",
                    "output-frequency": "1d",
                    "operations": [ "average" ],
                    "options": { 
                        "set-metadata": {
                          "stream": "clte" 
                        }
                    }
                },
                {
                    "type": "statistics-mtg2",
                    "output-frequency": "1m",
                    "operations": [ "average" ],
                    "options": { 
                        "set-metadata": {
                          "stream": "clmn",
                          "misc-timeIncrementInSeconds": 86400
                        }
                    }
                },
                {
                    "type": "encode-mtg2",
                    "cached": true
                },
                {
                    "type": "debug-sink"
                }
            ]
        })json";
        auto env = MultioTestEnvironment(plan);
        std::vector<double> payloadData(1024, 1.23);

        EXPECT_EQUAL(env.debugSink().size(), 0);

        // No initia
        for (std::int64_t step = 1; step <= 24 * (31 + 29 + 31); ++step) {
            auto md = mkMd(p, step);

            eckit::Buffer payload{payloadData.data(), sizeof(double) * payloadData.size()};
            Message msg{{Message::Tag::Field, {}, {}, std::move(md)}, std::move(payload)};
            EXPECT_NO_THROW(env.process(std::move(msg)));
        }

        // Expect last message not to be flushed, hence we expect just two messages for two month
        EXPECT_EQUAL(env.debugSink().size(), 2);

        // Send a flush last-step to trigger emitting the statistics message
        EXPECT_NO_THROW(env.process({{Message::Tag::Flush, {}, {}, {{"flushKind", "last-step"}}}}));

        // Now we got our last message plus additional flush message
        EXPECT_EQUAL(env.debugSink().size(), 4);


        for (auto [month, daysInMonth] : std::vector<std::pair<int64_t, int64_t>>{{1, 31}, {2, 29}, {3, 31}}) {
            auto codesHandle = metkit::codes::codesHandleFromMessage(
                {static_cast<const uint8_t*>(env.debugSink().front().payload().data()),
                 env.debugSink().front().payload().size()});

            auto md = env.debugSink().front().metadata();
            EXPECT_EQUAL(p.paramOut, md.get<std::int64_t>("param"));
            EXPECT_EQUAL(p.paramOut, codesHandle->getLong("param"));
            int64_t timespan = daysInMonth * 24;
            EXPECT_EQUAL(timespan, md.get<std::int64_t>("step"));
            EXPECT_EQUAL(timespan, codesHandle->getLong("step"));
            EXPECT_EQUAL(timespan, md.get<std::int64_t>("step"));
            EXPECT_EQUAL(timespan, codesHandle->getLong("step"));
            int64_t date = 19880001 + month * 100;
            EXPECT_EQUAL(date, md.get<std::int64_t>("date"));
            EXPECT_EQUAL(date, codesHandle->getLong("date"));
            EXPECT_EQUAL(0, md.get<std::int64_t>("time"));
            EXPECT_EQUAL(0, codesHandle->getLong("time"));
            EXPECT_EQUAL("clmn", md.get<std::string>("stream"));
            EXPECT_EQUAL("clmn", codesHandle->getString("stream"));
            EXPECT(std::nullopt == md.getOpt<std::string>("stattype"));

            EXPECT_EQUAL(1003, codesHandle->getLong("subCentre"));
            EXPECT_EQUAL(156, codesHandle->getLong("generatingProcessIdentifier"));
            EXPECT_EQUAL(3600 * 24, codesHandle->getLong("timeIncrement"));

            env.debugSink().pop();
        }

        EXPECT(env.debugSink().front().tag() == Message::Tag::Flush);
        env.debugSink().pop();
        EXPECT_EQUAL(env.debugSink().size(), 0);
    }
}

}  // namespace multio::test::statistics_mtg2

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

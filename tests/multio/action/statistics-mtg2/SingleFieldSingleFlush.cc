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

#include "multio/message/Message.h"
#include "multio/message/Metadata.h"

#include "../../MultioTestEnvironment.h"


namespace multio::test::statistics_mtg2 {

using multio::test::MultioTestEnvironment;
using multio::message::Message;
using multio::message::Metadata;


void testFieldAndFlush(std::string flushKind, int64_t steps=1) {
    ASSERT(steps == 1 || steps == 2);
    int64_t expect = flushKind == "last-step" && steps == 2 ? 2 : 1;

    const std::string plan = R"json({
        "name": "MULTIO_TEST",
        "actions" : [
            {
                "type": "statistics-mtg2",
                "output-frequency": "1d",
                "operations": [ "average" ],
                "options": {
                     "initial-condition-present": "true"
                }
            },
            {
                "type": "debug-sink"
            }
        ]
    })json";
    auto env = MultioTestEnvironment(plan);
    EXPECT_EQUAL(env.debugSink().size(), 0);

    for (int64_t step = 0; step < steps; ++step) {
        Metadata md{{
            {"param", 130},
            {"levtype", "sfc"},
            {"grid", "custom"},
            {"date", 20250425},
            {"time", 0000},
            {"step", step},
            {"misc-precision", "double"}
        }};
        eckit::Buffer pl{};
        Message msg{{Message::Tag::Field, {}, {}, std::move(md)}, std::move(pl)};
        EXPECT_NO_THROW(env.process(std::move(msg)));
        EXPECT_EQUAL(env.debugSink().size(), 0);
    }
    {
        Metadata md{{
            {"flushKind", flushKind},
            {"step", steps-1}
        }};
        eckit::Buffer pl{};
        Message msg{{Message::Tag::Flush, {}, {}, std::move(md)}, pl};
        EXPECT_NO_THROW(env.process(std::move(msg)));
        EXPECT_EQUAL(env.debugSink().size(), expect);
        // First comes a field (but only on last-step), then the flush
        if (expect == 2) {
            EXPECT(env.debugSink().front().tag() == Message::Tag::Field);
        }
        EXPECT(env.debugSink().back().tag() == Message::Tag::Flush);
    }
}

CASE("single field single flush default") { testFieldAndFlush("default"); }
CASE("single field single flush first-step") { testFieldAndFlush("first-step"); }
CASE("single field single flush step-and-restart") { testFieldAndFlush("step-and-restart"); }
CASE("single field single flush end-of-simulation") { testFieldAndFlush("end-of-simulation"); }
CASE("single field single flush close-connection") { testFieldAndFlush("close-connection"); }
CASE("single field single flush last-step") { testFieldAndFlush("last-step"); }

CASE("two fields single flush default") { testFieldAndFlush("default", 2); }
CASE("two fields single flush first-step") { testFieldAndFlush("first-step", 2); }
CASE("two fields single flush step-and-restart") { testFieldAndFlush("step-and-restart", 2); }
CASE("two fields single flush end-of-simulation") { testFieldAndFlush("end-of-simulation", 2); }
CASE("two fields single flush close-connection") { testFieldAndFlush("close-connection", 2); }
CASE("two fields single flush last-step") { testFieldAndFlush("last-step", 2); }


}  // multio::test::statistics_mtg2

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

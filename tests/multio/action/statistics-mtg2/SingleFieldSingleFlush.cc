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


void testSingleFieldSingleFlush(std::string flushKind) {
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

    {
        Metadata md{{
            {"param", 130},
            {"levtype", "sfc"},
            {"grid", "custom"},
            {"startDate", 20250425},
            {"startTime", 0000},
            {"step", 0},
            {"misc-precision", "double"}
        }};
        eckit::Buffer pl{};
        Message msg{{Message::Tag::Field, {}, {}, std::move(md)}, std::move(pl)};
        EXPECT_NO_THROW(env.plan().process(msg));
        EXPECT_EQUAL(env.debugSink().size(), 0);
    }
    {
        Metadata md{{
            {"flushKind", flushKind},
            {"step", 0}
        }};
        eckit::Buffer pl{};
        Message msg{{Message::Tag::Flush, {}, {}, std::move(md)}, pl};
        EXPECT_NO_THROW(env.plan().process(msg));
        EXPECT_EQUAL(env.debugSink().size(), 1);
        EXPECT(env.debugSink().front().tag() == Message::Tag::Flush);
    }
}

CASE("single message single flush default") { testSingleFieldSingleFlush("default"); }
CASE("single message single flush first-step") { testSingleFieldSingleFlush("first-step"); }
CASE("single message single flush step-and-restart") { testSingleFieldSingleFlush("step-and-restart"); }
CASE("single message single flush end-of-simulation") { testSingleFieldSingleFlush("end-of-simulation"); }
CASE("single message single flush close-connection") { testSingleFieldSingleFlush("close-connection"); }


void testSingleFieldSingleFlushLastStep() {
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

    {
        Metadata md{{
            {"param", 130},
            {"levtype", "sfc"},
            {"grid", "custom"},
            {"startDate", 20250425},
            {"startTime", 0000},
            {"step", 0},
            {"misc-precision", "double"}
        }};
        eckit::Buffer pl{};
        Message msg{{Message::Tag::Field, {}, {}, std::move(md)}, std::move(pl)};
        EXPECT_NO_THROW(env.plan().process(msg));
        EXPECT_EQUAL(env.debugSink().size(), 0);
    }
    {
        Metadata md{{
            {"flushKind", "last-step"},
            {"step", 0}
        }};
        eckit::Buffer pl{};
        Message msg{{Message::Tag::Flush, {}, {}, std::move(md)}, pl};
        EXPECT_NO_THROW(env.plan().process(msg));
        EXPECT_EQUAL(env.debugSink().size(), 2);
        EXPECT(env.debugSink().front().tag() == Message::Tag::Field);
        env.debugSink().pop();
        EXPECT(env.debugSink().front().tag() == Message::Tag::Flush);
    }
}

CASE("single field single flush last-step") { testSingleFieldSingleFlushLastStep(); }


}  // multio::test::statistics_mtg2

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

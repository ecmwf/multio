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


void testSingleFlush(std::string flushKind) {
    const std::string plan = R"json({
        "name": "MULTIO_TEST",
        "actions" : [
            {
                "type": "statistics-mtg2",
                "output-frequency": "1d",
                "operations": [ "average" ]
            },
            {
                "type": "debug-sink"
            }
        ]
    })json";
    auto env = MultioTestEnvironment(plan);
    EXPECT_EQUAL(env.debugSink().size(), 0);

    Metadata md{{
        {"flushKind", flushKind},
        {"step", 0}
    }};
    eckit::Buffer pl{};
    Message msg{{Message::Tag::Flush, {}, {}, std::move(md)}, std::move(pl)};
    EXPECT_NO_THROW(env.plan().process(msg));
    EXPECT_EQUAL(env.debugSink().size(), 1);
    EXPECT(env.debugSink().front().tag() == Message::Tag::Flush);
}

CASE("single flush default") { testSingleFlush("default"); }
CASE("single flush first-step") { testSingleFlush("first-step"); }
CASE("single flush last-step") { testSingleFlush("last-step"); }
CASE("single flush step-and-restart") { testSingleFlush("step-and-restart"); }
CASE("single flush end-of-simulation") { testSingleFlush("end-of-simulation"); }
CASE("single flush close-connection") { testSingleFlush("close-connection"); }


}  // multio::test::statistics_mtg2

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

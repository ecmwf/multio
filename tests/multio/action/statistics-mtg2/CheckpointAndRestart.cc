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
#include "StatisticsTestHelpers.h"


namespace multio::test::statistics_mtg2 {

using multio::message::Message;
using multio::message::Metadata;
using multio::test::MultioTestEnvironment;


CASE("simple checkpoint and restart") {
    {
        const std::string plan = R"json({
            "name": "MULTIO_TEST",
            "actions": [
                {
                    "type": "statistics-mtg2",
                    "output-frequency": "1d",
                    "operations": [ "average" ],
                    "options": {
                        "initial-condition-present": true,
                        "debug-restart": true,
                        "restart-path": ".",
                        "read-restart": false,
                        "write-restart": true,
                        restart-lib: "eckit_codec",
                        "restart-time": "latest"
                    }
                },
                {
                    "type": "debug-sink"
                }
            ]
        })json";
        auto env = MultioTestEnvironment(plan);
        EXPECT_EQUAL(env.debugSink().size(), 0);
        sendSimulationStart(env, 20250430);

        for (int step = 0; step < 6; ++step) {
            const double val = 1.0;
            Metadata md{{{"param", 130},
                         {"levtype", "sfc"},
                         {"grid", "custom"},
                         {"date", 20250430},
                         {"time", 0000},
                         {"step", step},
                         {"misc-precision", "double"}}};
            setTimingMetadata(md, 3600);
            eckit::Buffer pl{&val, sizeof(double)};
            Message msg{{Message::Tag::Field, {}, {}, std::move(md)}, std::move(pl)};
            EXPECT_NO_THROW(env.process(std::move(msg)));
            EXPECT_EQUAL(env.debugSink().size(), 0);
        }

        {
            Metadata md{{{"flushKind", "step-and-restart"}, {"step", 5}}};
            eckit::Buffer pl{};
            Message msg{{Message::Tag::Flush, {}, {}, std::move(md)}, std::move(pl)};

            EXPECT_NO_THROW(env.process(std::move(msg)));
            EXPECT_EQUAL(env.debugSink().size(), 1);
            EXPECT(env.debugSink().front().tag() == Message::Tag::Flush);
            env.debugSink().pop();
        }
        {
            Metadata md{{{"flushKind", "end-of-simulation"}, {"step", 5}}};
            eckit::Buffer pl{};
            Message msg{{Message::Tag::Flush, {}, {}, std::move(md)}, std::move(pl)};
            EXPECT_NO_THROW(env.process(std::move(msg)));
            EXPECT_EQUAL(env.debugSink().size(), 1);
            EXPECT(env.debugSink().front().tag() == Message::Tag::Flush);
            env.debugSink().pop();
        }
    }


    {
        const std::string plan = R"json({
            "name": "MULTIO_TEST",
            "actions": [
                {
                    "type": "statistics-mtg2",
                    "output-frequency": "1d",
                    "operations": [ "average" ],
                    "options": {
                        "initial-condition-present": true,
                        "debug-restart": true,
                        "restart-path": ".",
                        "read-restart": true,
                        "write-restart": false,
                        "emit-incomplete-statistics": true,
                        restart-lib: "eckit_codec",
                        "restart-time": "latest"
                    }
                },
                {
                    "type": "debug-sink"
                }
            ]
        })json";
        auto env = MultioTestEnvironment(plan);
        EXPECT_EQUAL(env.debugSink().size(), 0);
        sendSimulationStart(env, 20250430);

        for (int step = 6; step < 12; ++step) {
            const double val = 3.0;
            Metadata md{{{"param", 130},
                         {"levtype", "sfc"},
                         {"grid", "custom"},
                         {"date", 20250430},
                         {"time", 0000},
                         {"step", step},
                         {"misc-precision", "double"}}};
            setTimingMetadata(md, 3600);
            eckit::Buffer pl{&val, sizeof(double)};
            Message msg{{Message::Tag::Field, {}, {}, std::move(md)}, std::move(pl)};
            EXPECT_NO_THROW(env.process(std::move(msg)));
            EXPECT_EQUAL(env.debugSink().size(), 0);
        }
        {
            Metadata md{{{"flushKind", "last-step"}, {"step", 11}}};
            eckit::Buffer pl{};
            Message msg{{Message::Tag::Flush, {}, {}, std::move(md)}, std::move(pl)};
            EXPECT_NO_THROW(env.process(std::move(msg)));
            EXPECT_EQUAL(env.debugSink().size(), 2);
            EXPECT(env.debugSink().front().tag() == Message::Tag::Field);
            const double* payload = static_cast<const double*>(env.debugSink().front().payload().data());
            // Step 0 is on the lower window boundary and not a sample: 5 samples of 1.0 before the restart
            // (steps 1-5) and 6 samples of 3.0 after it (steps 6-11)
            const double diff = payload[0] - (5 * 1.0 + 6 * 3.0) / 11;
            const double err2 = diff * diff;
            std::cout << "diff=" << diff << std::endl;
            std::cout << "err2=" << err2 << std::endl;
            EXPECT(err2 < 0.01);
            env.debugSink().pop();
            EXPECT(env.debugSink().front().tag() == Message::Tag::Flush);
            env.debugSink().pop();
        }
    }
}


}  // namespace multio::test::statistics_mtg2

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

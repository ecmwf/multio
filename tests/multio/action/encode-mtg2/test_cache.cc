/*
 * (C) Copyright 2026- ECMWF and individual contributors.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */


#include <variant>
#include "eckit/testing/Test.h"

#include "multio/message/Message.h"
#include "multio/message/Metadata.h"

#include "../../MultioTestEnvironment.h"


using multio::message::Message;
using multio::message::Metadata;
using multio::message::MetadataValue;
using multio::test::MultioTestEnvironment;


std::string getPlan(bool cache, const std::string& path) {
    return R"json({
        "name": "MULTIO_TEST",
        "actions": [
            {
                "type": "encode-mtg2",
                "cached": )json" + std::string(cache ? "true" : "false") + R"json(
            },
            {
                "type": "sink",
                "sinks": [
                    {
                        "type": "file",
                        "append": false,
                        "per-server": false,
                        "path": ")json" + path + R"json("
                    }
                ]
            }
        ]
    })json";
}

Message getMessage(int step, std::variant<std::string, int> timespan) {
    const Metadata md{{
        {"misc-precision", "double"},
        {"class", "od"},
        {"stream", "oper"},
        {"type", "fc"},
        {"expver", "test"},
        {"grid", "45/45"},
        {"packing", "ccsds"},
        {"param", 228},
        {"levtype", "sfc"},
        {"date", 2026'10'02},
        {"time", 00'00},
        {"step", step},
        {"timespan", std::visit([](const auto& v) -> MetadataValue { return v; }, timespan)}
    }};

    const std::vector<double> values(40, 0.010);
    const eckit::Buffer pl{values.data(), sizeof(double) * values.size()};

    return Message{{Message::Tag::Field, {}, {}, std::move(md)}, std::move(pl)};
}

void runFsEncodePlan(bool cache, const std::string& path) {
    auto env = MultioTestEnvironment(getPlan(cache, path));
    for (int step = 0; step <= 6; ++step) {
        auto msg = getMessage(step, "fs");
        EXPECT_NO_THROW(env.process(std::move(msg)));
    }
}

CASE("encode timespan=fs") {
    runFsEncodePlan(false, "timespan-fs.grib");
}

CASE("encode timespan=fs with cache") {
    runFsEncodePlan(true, "timespan-fs-with-cache.grib");
}

void runTsEncodePlan(bool cache, const std::string& path) {
    auto env = MultioTestEnvironment(getPlan(cache, path));

    {
        auto msg = getMessage(12, 3);
        EXPECT_NO_THROW(env.process(std::move(msg)));
    }

    {
        auto msg = getMessage(12, 6);
        EXPECT_NO_THROW(env.process(std::move(msg)));
    }
}

CASE("encode timespan=3/6") {
    runTsEncodePlan(false, "timespan-3-6.grib");
}

CASE("encode timespan=3/6 with cache") {
    runTsEncodePlan(true, "timespan-3-6-with-cache.grib");
}

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

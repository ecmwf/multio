/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// Integration test: `mars2mars` followed by `encode-mtg2`, checked by decoding the GRIB
/// message with eccodes. This is the AIFS v3.0 archive scenario: the model sends soil layers
/// and 100 m winds on levtype=sfc, the archive wants sot/vsw on levtype=sol and 100u/100v on
/// levtype=sfc. The latter works because the AIFS v3.0 entries of the eccodes MTG2 switch
/// concept make eccodes read the 0/2/2 @ 103/100 m header back as paramId 228246 on sfc.

#include <memory>
#include <string>

#include "eccodes.h"

#include "eckit/testing/Test.h"

#include "../../MultioTestEnvironment.h"

namespace multio::test::mars2mars {

using multio::message::Message;
using multio::message::Metadata;
using multio::test::MultioTestEnvironment;

namespace {

const std::string plan = R"json({
    "name": "mars2mars_encode_aifs_test",
    "actions": [
        {
            "type": "mars2mars",
            "selector": {
                "match": [
                    {
                        "param": [139, 170, 183, 236, 39, 40, 41, 42, 228246, 228247],
                        "levtype": ["sfc"]
                    }
                ]
            }
        },
        { "type": "encode-mtg2" },
        { "type": "debug-sink" }
    ]
})json";

constexpr std::size_t N320_POINTS = 542080;

/// Metadata of an AIFS single (MSE) v3.0 forecast field as the anemoi multio plugin sends it.
Metadata aifsField(std::int64_t param, const std::string& levtype) {
    return Metadata({
        {"class", "ai"},
        {"stream", "oper"},
        {"type", "fc"},
        {"expver", "0001"},
        {"model", "aifs-single-mse"},
        {"date", 20260930},
        {"time", 0},
        {"step", 6},
        {"timespan", "none"},
        {"param", param},
        {"levtype", levtype},
        {"grid", "N320"},
        {"packing", "ccsds"},
        {"misc-generatingProcessIdentifier", 6},
        {"misc-bitmapPresent", false},
        {"misc-precision", "double"},
    });
}

Message fieldMessage(Metadata md) {
    std::vector<double> values(N320_POINTS, 1.5);
    auto pl = eckit::Buffer(values.data(), values.size() * sizeof(double));
    return Message({Message::Tag::Field, {}, {}, std::move(md)}, std::move(pl));
}

struct CodesHandleDeleter {
    void operator()(codes_handle* h) const { codes_handle_delete(h); }
};
using HandlePtr = std::unique_ptr<codes_handle, CodesHandleDeleter>;

HandlePtr decode(const Message& msg) {
    codes_handle* h = codes_handle_new_from_message(nullptr, msg.payload().data(), msg.payload().size());
    EXPECT(h != nullptr);
    return HandlePtr{h};
}

long getLong(codes_handle* h, const char* key) {
    long v = -1;
    EXPECT_EQUAL(codes_get_long(h, key, &v), 0);
    return v;
}

std::string getString(codes_handle* h, const char* key) {
    char buf[256];
    std::size_t len = sizeof(buf);
    EXPECT_EQUAL(codes_get_string(h, key, buf, &len), 0);
    return std::string{buf};
}

}  // namespace

//----------------------------------------------------------------------------------------------------------------------

CASE("stl1 on sfc is archived as sot on sol level 1") {
    auto env = MultioTestEnvironment(plan);

    EXPECT_NO_THROW(env.process(fieldMessage(aifsField(139, "sfc"))));
    EXPECT_EQUAL(env.debugSink().size(), 1);

    auto h = decode(env.debugSink().front());
    EXPECT_EQUAL(getLong(h.get(), "paramId"), 260360);
    EXPECT_EQUAL(getString(h.get(), "typeOfLevel"), "soilLayer");
    EXPECT_EQUAL(getLong(h.get(), "topLevel"), 0);
    EXPECT_EQUAL(getLong(h.get(), "bottomLevel"), 1);

    // What FDB / MARS will see
    EXPECT_EQUAL(getString(h.get(), "mars.param"), "260360");
    EXPECT_EQUAL(getString(h.get(), "mars.levtype"), "sol");
    EXPECT_EQUAL(getLong(h.get(), "mars.levelist"), 1);
}

CASE("swvl3 on sfc is archived as vsw on sol level 3") {
    auto env = MultioTestEnvironment(plan);

    EXPECT_NO_THROW(env.process(fieldMessage(aifsField(41, "sfc"))));
    EXPECT_EQUAL(env.debugSink().size(), 1);

    auto h = decode(env.debugSink().front());
    EXPECT_EQUAL(getLong(h.get(), "paramId"), 260199);
    EXPECT_EQUAL(getString(h.get(), "mars.levtype"), "sol");
    EXPECT_EQUAL(getLong(h.get(), "mars.levelist"), 3);
}

CASE("100u on sfc is archived as 100u on sfc through the MTG2 switch") {
    auto env = MultioTestEnvironment(plan);

    EXPECT_NO_THROW(env.process(fieldMessage(aifsField(228246, "sfc"))));
    EXPECT_EQUAL(env.debugSink().size(), 1);

    auto h = decode(env.debugSink().front());

    // Header as written by mars2grib: u-component on a 103 surface at 100 m
    EXPECT_EQUAL(getLong(h.get(), "discipline"), 0);
    EXPECT_EQUAL(getLong(h.get(), "parameterCategory"), 2);
    EXPECT_EQUAL(getLong(h.get(), "parameterNumber"), 2);
    EXPECT_EQUAL(getString(h.get(), "typeOfLevel"), "heightAboveGround");
    EXPECT_EQUAL(getLong(h.get(), "level"), 100);

    // Keys that drive the eccodes MTG2 switch for AIFS-SINGLE-MSE v3.0
    EXPECT_EQUAL(getLong(h.get(), "backgroundProcess"), 3);
    EXPECT_EQUAL(getLong(h.get(), "generatingProcessIdentifier"), 6);
    EXPECT_EQUAL(getLong(h.get(), "MTG2Switch"), 0);

    // What FDB / MARS will see: the pre-MTG2 concepts
    EXPECT_EQUAL(getLong(h.get(), "paramId"), 228246);
    EXPECT_EQUAL(getString(h.get(), "mars.param"), "228246");
    EXPECT_EQUAL(getString(h.get(), "mars.levtype"), "sfc");
    EXPECT_EQUAL(codes_is_defined(h.get(), "mars.levelist"), 0);
}

CASE("100v on sfc is archived as 100v on sfc through the MTG2 switch") {
    auto env = MultioTestEnvironment(plan);

    EXPECT_NO_THROW(env.process(fieldMessage(aifsField(228247, "sfc"))));
    EXPECT_EQUAL(env.debugSink().size(), 1);

    auto h = decode(env.debugSink().front());
    EXPECT_EQUAL(getLong(h.get(), "paramId"), 228247);
    EXPECT_EQUAL(getString(h.get(), "mars.levtype"), "sfc");
    EXPECT_EQUAL(codes_is_defined(h.get(), "mars.levelist"), 0);
}

//----------------------------------------------------------------------------------------------------------------------

}  // namespace multio::test::mars2mars

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

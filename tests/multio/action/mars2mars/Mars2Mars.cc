/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "eckit/testing/Test.h"

#include "../../MultioTestEnvironment.h"

namespace multio::test::mars2mars {

using multio::message::Message;
using multio::message::Metadata;
using multio::test::MultioTestEnvironment;

namespace {

/// A plan that only converts the AIFS v3.0 special cases: soil temperature and soil moisture
/// layers passed on levtype=sfc, and the 100 m wind components.
const std::string aifsPlan = R"json({
    "name": "mars2mars_aifs_test",
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
        {
            "type": "debug-sink"
        }
    ]
})json";

/// A plan without selector: every field goes through all metkit rules.
const std::string unrestrictedPlan = R"json({
    "name": "mars2mars_unrestricted_test",
    "actions": [
        { "type": "mars2mars" },
        { "type": "debug-sink" }
    ]
})json";

/// Metadata of an AIFS single forecast field as the anemoi multio plugin sends it.
Metadata aifsField(std::int64_t param, const std::string& levtype) {
    return Metadata({
        {"class", "ai"},
        {"stream", "oper"},
        {"type", "fc"},
        {"expver", "0001"},
        {"model", "aifs-single"},
        {"date", 20260930},
        {"time", 0},
        {"step", 6},
        {"timespan", "none"},
        {"param", param},
        {"levtype", levtype},
        {"grid", "N320"},
        {"packing", "ccsds"},
        {"misc-generatingProcessIdentifier", 6},
        {"misc-precision", "double"},
    });
}

Message fieldMessage(Metadata md) {
    std::vector<double> values = {0.0, 1.0, 2.0};
    auto pl = eckit::Buffer(values.data(), values.size() * sizeof(double));
    return Message({Message::Tag::Field, {}, {}, std::move(md)}, std::move(pl));
}

}  // namespace

//----------------------------------------------------------------------------------------------------------------------

CASE("Soil temperature stl1 on sfc becomes sot on sol level 1") {
    auto env = MultioTestEnvironment(aifsPlan);

    EXPECT_NO_THROW(env.process(fieldMessage(aifsField(139, "sfc"))));
    EXPECT_EQUAL(env.debugSink().size(), 1);

    const auto& md = env.debugSink().front().metadata();
    EXPECT_EQUAL(md.get<std::int64_t>("param"), 260360);
    EXPECT_EQUAL(md.get<std::string>("levtype"), "sol");
    EXPECT_EQUAL(md.get<std::int64_t>("levelist"), 1);

    // Untouched keys keep their original representation
    EXPECT_EQUAL(md.get<std::string>("model"), "aifs-single");
    EXPECT_EQUAL(md.get<std::int64_t>("step"), 6);
    EXPECT_EQUAL(md.get<std::int64_t>("misc-generatingProcessIdentifier"), 6);
}

CASE("Soil temperature stl4 on sfc becomes sot on sol level 4") {
    auto env = MultioTestEnvironment(aifsPlan);

    EXPECT_NO_THROW(env.process(fieldMessage(aifsField(236, "sfc"))));
    EXPECT_EQUAL(env.debugSink().size(), 1);

    const auto& md = env.debugSink().front().metadata();
    EXPECT_EQUAL(md.get<std::int64_t>("param"), 260360);
    EXPECT_EQUAL(md.get<std::string>("levtype"), "sol");
    EXPECT_EQUAL(md.get<std::int64_t>("levelist"), 4);
}

CASE("Soil moisture swvl3 on sfc becomes vsw on sol level 3") {
    auto env = MultioTestEnvironment(aifsPlan);

    EXPECT_NO_THROW(env.process(fieldMessage(aifsField(41, "sfc"))));
    EXPECT_EQUAL(env.debugSink().size(), 1);

    const auto& md = env.debugSink().front().metadata();
    EXPECT_EQUAL(md.get<std::int64_t>("param"), 260199);
    EXPECT_EQUAL(md.get<std::string>("levtype"), "sol");
    EXPECT_EQUAL(md.get<std::int64_t>("levelist"), 3);
}

CASE("100 m wind 100u on sfc becomes u on hl level 100") {
    // mars2grib encodes this as 0/2/2 on a 103 surface at 100 m. With the AIFS v3.0 entries of
    // the eccodes MTG2 switch concept, eccodes reads the message back as param=228246, levtype=sfc,
    // which is what FDB indexes.
    auto env = MultioTestEnvironment(aifsPlan);

    EXPECT_NO_THROW(env.process(fieldMessage(aifsField(228246, "sfc"))));
    EXPECT_EQUAL(env.debugSink().size(), 1);

    const auto& md = env.debugSink().front().metadata();
    EXPECT_EQUAL(md.get<std::int64_t>("param"), 131);
    EXPECT_EQUAL(md.get<std::string>("levtype"), "hl");
    EXPECT_EQUAL(md.get<std::int64_t>("levelist"), 100);
}

CASE("100 m wind 100v on sfc becomes v on hl level 100") {
    auto env = MultioTestEnvironment(aifsPlan);

    EXPECT_NO_THROW(env.process(fieldMessage(aifsField(228247, "sfc"))));
    EXPECT_EQUAL(env.debugSink().size(), 1);

    const auto& md = env.debugSink().front().metadata();
    EXPECT_EQUAL(md.get<std::int64_t>("param"), 132);
    EXPECT_EQUAL(md.get<std::string>("levtype"), "hl");
    EXPECT_EQUAL(md.get<std::int64_t>("levelist"), 100);
}

CASE("Fields outside the selector pass through unchanged") {
    auto env = MultioTestEnvironment(aifsPlan);

    // Total precipitation has a local-to-WMO rule in metkit; the selector must keep it out.
    EXPECT_NO_THROW(env.process(fieldMessage(aifsField(228, "sfc"))));
    EXPECT_EQUAL(env.debugSink().size(), 1);

    const auto& md = env.debugSink().front().metadata();
    EXPECT_EQUAL(md.get<std::int64_t>("param"), 228);
    EXPECT_EQUAL(md.get<std::string>("levtype"), "sfc");
    EXPECT(md.find("levelist") == md.end());
}

CASE("Without a selector a rule that needs value scaling is an error") {
    auto env = MultioTestEnvironment(unrestrictedPlan);

    // 228 -> 228228 requires scaleValuesBy=1000 which this action cannot honour
    EXPECT_THROWS(env.process(fieldMessage(aifsField(228, "sfc"))));
    EXPECT_EQUAL(env.debugSink().size(), 0);
}

CASE("Non-field messages pass through") {
    auto env = MultioTestEnvironment(aifsPlan);

    EXPECT_NO_THROW(env.process(Message({Message::Tag::Flush, {}, {}, Metadata{}})));
    EXPECT_EQUAL(env.debugSink().size(), 1);
    EXPECT(env.debugSink().front().tag() == Message::Tag::Flush);
}

//----------------------------------------------------------------------------------------------------------------------

}  // namespace multio::test::mars2mars

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

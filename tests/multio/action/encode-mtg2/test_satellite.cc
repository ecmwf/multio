/*
 * (C) Copyright 2026- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// Encodes a simulated satellite field with the metadata ifs2mars emits for it: the MARS keys ident, instrument and
/// channel, plus the misc keys satelliteSeries, scaleFactorOfCentralWaveNumber and scaledValueOfCentralWaveNumber.


#include "eckit/testing/Test.h"

#include "multio/message/Message.h"
#include "multio/message/Metadata.h"

#include "../../MultioTestEnvironment.h"

namespace multio::test::encode_mtg2 {

using multio::message::Message;
using multio::message::Metadata;
using multio::test::MultioTestEnvironment;

namespace {

const std::string plan = R"json({
    "name": "satellite",
    "actions": [ { "type": "encode-mtg2" }, { "type": "debug-sink" } ]
})json";

// Cloudy brightness temperature, Meteosat-10 SEVIRI channel 9 (10.8 micron), as ifs2mars would emit it
Metadata satelliteMetadata(const std::string& levtype) {
    return Metadata{{{"class", "od"},
                     {"expver", "0001"},
                     {"type", "ssd"},
                     {"stream", "oper"},
                     {"repres", "gg"},
                     {"packing", "ccsds"},
                     {"grid", "N320"},
                     {"date", 20230901},
                     {"time", 0},
                     {"step", 6},
                     {"param", 260510},
                     {"levtype", levtype},
                     {"levelist", 3},  // ifs2mars sets levelist to the channel index of the field
                     {"ident", 57},
                     {"instrument", 207},
                     {"channel", 9},
                     {"misc-satelliteSeries", 333},
                     {"misc-scaleFactorOfCentralWaveNumber", 0},
                     {"misc-scaledValueOfCentralWaveNumber", 92593},
                     {"misc-precision", "double"}}};
}

void encodeOne(MultioTestEnvironment& env, Metadata md) {
    std::vector<double> payloadData(8192, 250.0);
    eckit::Buffer payload{payloadData.data(), sizeof(double) * payloadData.size()};
    env.process(Message{{Message::Tag::Field, {}, {}, std::move(md)}, std::move(payload)});
}

}  // namespace

CASE("satellite field encodes with the keys ifs2mars emits") {
    auto env = MultioTestEnvironment(plan);
    EXPECT_NO_THROW(encodeOne(env, satelliteMetadata("sfc")));
    EXPECT_EQUAL(env.debugSink().size(), 1);
}

}  // namespace multio::test::encode_mtg2

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

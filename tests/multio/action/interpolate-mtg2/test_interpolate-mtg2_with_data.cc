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

#include "multio/message/Parametrization.h"
#include "multio/util/FailureHandling.h"

#include "../../MultioTestEnvironment.h"


namespace multio::test::interpolate_mtg2 {


namespace dm = multio::datamod;

using multio::message::Message;
using multio::message::Metadata;
using multio::test::MultioTestEnvironment;


// TODO:
//   - explicitly test mapping that perform scaling and check if it has been done


Metadata climDtMD() {
    return {
        {"dataset", "climate-dt"},
        {"activity", "baseline"},
        {"experiment", "hist"},
        {"generation", "2"},
        {"model", "IFS-NEMO"},
        {"realization", "1"},
        {"resolution", "high"},
        {"misc-destineLocalVersion", 1},
        {"misc-format", "raw"},
        {"misc-globalSize", 28480},
        {"misc-laplacianScaleFactor", 0.5},
        {"misc-bitsPerValue", 16},
        {"misc-typeOfProcessedData", 1},
        {"misc-precision", "single"},
        {"misc-generatingProcessIdentifier", 160},
        {"misc-tablesVersion", 36},
        {"grid", "O80"},
        {"step", 0},
        {"time", 0},
        {"date", 19880101},
        {"levelist", 100000},
        {"levtype", "pl"},
        {"param", 131},
        {"packing", "ccsds"},
        {"origin", "ecmf"},
        {"expver", "j36u"},
        {"class", "d1"},
        {"type", "fc"},
        {"stream", "clte"},
    };
}


CASE("Test interpolate-mtg2 with O80 to H32_nested climateDt ATM") {
    const std::string plan = R"json({
            "name": "MULTIO_TEST",
            "actions": [
                {
                    "type": "interpolate-mtg2",
                    "outputs": [{
                      "grid": 'H32_nested',
                      "enable": '1',
                      "options": {"caching": true},
                      "additional-metadata": {"resolution": "standard"}
                    }]
                },
                {
                    "type": "debug-sink"
                }
            ]
        })json";

    auto env = MultioTestEnvironment(plan);
    EXPECT_EQUAL(env.debugSink().size(), 0);

    std::vector<float> payloadData(28480, 1.23);

    auto md = climDtMD();


    EXPECT_EQUAL(env.debugSink().size(), 0);
    eckit::Buffer payload{payloadData.data(), sizeof(float) * payloadData.size()};
    Message msg{{Message::Tag::Field, {}, {}, std::move(md)}, std::move(payload)};

    EXPECT_NO_THROW(env.process(std::move(msg)));

    EXPECT_EQUAL(env.debugSink().size(), 1);
    EXPECT(env.debugSink().front().tag() == Message::Tag::Field);
    EXPECT(env.debugSink().front().payload().size() == 98304);

    auto& mdout = env.debugSink().front().metadata();
    EXPECT_EQUAL(mdout.get<std::string>("grid"), "H32_nested");

    env.debugSink().pop();
}


CASE("Test interpolate-mtg2 with O80 to H32_nested climateDt ATM [wrong payload size]") {
    const std::string plan = R"json({
            "name": "MULTIO_TEST",
            "actions": [
                {
                    "type": "interpolate-mtg2",
                    "outputs": [{
                      "grid": 'H32_nested',
                      "enable": '1',
                      "options": {"caching": true},
                      "additional-metadata": {"resolution": "standard"}
                    }]
                },
                {
                    "type": "debug-sink"
                }
            ]
        })json";

    auto env = MultioTestEnvironment(plan);
    EXPECT_EQUAL(env.debugSink().size(), 0);

    std::vector<float> payloadData(12345, 1.23);

    auto md = climDtMD();

    EXPECT_EQUAL(env.debugSink().size(), 0);
    eckit::Buffer payload{payloadData.data(), sizeof(float) * payloadData.size()};
    Message msg{{Message::Tag::Field, {}, {}, std::move(md)}, std::move(payload)};

    EXPECT_THROWS(env.process(std::move(msg)));
}

}  // namespace multio::test::interpolate_mtg2

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

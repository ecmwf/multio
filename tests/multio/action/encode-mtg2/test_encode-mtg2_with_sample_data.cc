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
#include "multio/util/SampleMetadataGen.h"

#include "../../MultioTestEnvironment.h"


namespace multio::test::encode_mtg2 {


namespace dm = multio::datamod;

using multio::message::Message;
using multio::message::Metadata;
using multio::test::MultioTestEnvironment;


// TODO:
//   - explicitly test mapping that perform scaling and check if it has been done


CASE("Test encode-mtg2 with AIFS single ") {
    {
        const std::string plan = R"json({
            "name": "MULTIO_TEST",
            "actions": [
                {
                    "type": "encode-mtg2",
                    "geo-from-atlas": true
                },
                {
                    "type": "debug-sink"
                }
            ]
        })json";
        auto env = MultioTestEnvironment(plan);
        EXPECT_EQUAL(env.debugSink().size(), 0);

        std::vector<double> payloadData(8192, 1.23);

        for (auto md : multio::util::sample_gen::mkAifsSingleMd()) {
            try {
                md.set(dm::legacy::Precision, "double");

                EXPECT_EQUAL(env.debugSink().size(), 0);
                eckit::Buffer payload{payloadData.data(), sizeof(double) * payloadData.size()};
                Message msg{{Message::Tag::Field, {}, {}, std::move(md)}, std::move(payload)};
                EXPECT_NO_THROW(env.process(std::move(msg)));
                EXPECT_EQUAL(env.debugSink().size(), 1);
                EXPECT(env.debugSink().front().tag() == Message::Tag::Field);
                env.debugSink().pop();
            }
            catch (const eckit::Exception& e) {
                std::cout << "Error while generating & comparing message: " << md << std::endl;
                throw;
            }
        }
    }
}


CASE("Test encode-mtg2 with AIFS ens") {
    {
        const std::string plan = R"json({
            "name": "MULTIO_TEST",
            "actions": [
                {
                    "type": "encode-mtg2",
                    "geo-from-atlas": true
                },
                {
                    "type": "debug-sink"
                }
            ]
        })json";

        message::Parametrization::instance().clear();
        message::Parametrization::instance().update(  //
            message::Metadata{                        //
                              {"numberOfForecastsInEnsemble", 50},
                              {"typeOfEnsembleForecast", 2}});


        auto env = MultioTestEnvironment(plan);
        EXPECT_EQUAL(env.debugSink().size(), 0);

        std::vector<double> payloadData(8192, 1.23);

        for (auto md : multio::util::sample_gen::mkAifsSingleMd()) {
            try {
                md.set(dm::legacy::Precision, "double");

                EXPECT_EQUAL(env.debugSink().size(), 0);
                eckit::Buffer payload{payloadData.data(), sizeof(double) * payloadData.size()};
                Message msg{{Message::Tag::Field, {}, {}, std::move(md)}, std::move(payload)};
                EXPECT_NO_THROW(env.process(std::move(msg)));
                EXPECT_EQUAL(env.debugSink().size(), 1);
                EXPECT(env.debugSink().front().tag() == Message::Tag::Field);
                env.debugSink().pop();
            }
            catch (const eckit::Exception& e) {
                std::cout << "Error while generating & comparing message: " << md << std::endl;
                throw;
            }
        }
    }
}

}  // namespace multio::test::encode_mtg2

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

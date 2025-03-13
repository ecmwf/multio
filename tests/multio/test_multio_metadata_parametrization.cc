/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */


/// @author Philipp Geier

#include "eckit/testing/Test.h"
#include "multio/message/Metadata.h"
#include "multio/message/Parametrization.h"
#include "multio/util/VariantHelpers.h"


namespace multio::test {

using multio::message::BaseMetadata;
using multio::message::Metadata;
using multio::message::MetadataException;
using multio::message::MetadataKeyException;
using multio::message::MetadataMissingKeyException;
using multio::message::MetadataTypes;
using multio::message::MetadataValue;
using multio::message::MetadataWrongTypeException;
using multio::message::Null;
using multio::message::Parametrization;
using namespace multio::message::parametrization;


CASE("Test parse parametrization element type") {
    EXPECT(decodeElementType("byte") == ElementType::Byte);
    EXPECT(decodeElementType("int32") == ElementType::Int32);
    EXPECT(decodeElementType("int64") == ElementType::Int64);
    EXPECT(decodeElementType("real32") == ElementType::Real32);
    EXPECT(decodeElementType("real64") == ElementType::Real64);

    EXPECT(toString(ElementType::Byte) == "byte");
    EXPECT(toString(ElementType::Int32) == "int32");
    EXPECT(toString(ElementType::Int64) == "int64");
    EXPECT(toString(ElementType::Real32) == "real32");
    EXPECT(toString(ElementType::Real64) == "real64");

    EXPECT(toString(decodeElementType("byte")) == "byte");
    EXPECT(toString(decodeElementType("int32")) == "int32");
    EXPECT(toString(decodeElementType("int64")) == "int64");
    EXPECT(toString(decodeElementType("real32")) == "real32");
    EXPECT(toString(decodeElementType("real64")) == "real64");

    EXPECT(decodeElementType(toString(ElementType::Byte)) == ElementType::Byte);
    EXPECT(decodeElementType(toString(ElementType::Int32)) == ElementType::Int32);
    EXPECT(decodeElementType(toString(ElementType::Int64)) == ElementType::Int64);
    EXPECT(decodeElementType(toString(ElementType::Real32)) == ElementType::Real32);
    EXPECT(decodeElementType(toString(ElementType::Real64)) == ElementType::Real64);
};


CASE("Test update parametrization") {
    Parametrization::instance().clear();
    Parametrization::instance().update(Metadata{{"a", 1}, {"b", 1.0}});


    EXPECT_NO_THROW(Parametrization::instance().update(Metadata{{"a", 1}, {"b", 1.0}}));
    EXPECT_THROWS_AS(Parametrization::instance().update(Metadata{{"a", 2}}), MetadataException);


    const BaseMetadata& par = Parametrization::instance().get();
    EXPECT(par.get<std::int64_t>("a") == 1);
    EXPECT(par.get<double>("b") == 1.0);


    Metadata m;
    EXPECT(m.get<std::int64_t>("a") == 1);
    EXPECT(m.get<double>("b") == 1.0);


    unsigned char data[5] = {0x1, 0x2, 0x3, 0x4, 0x5};
    unsigned char data2[5] = {0x1, 0x0, 0x0, 0x0, 0x5};
    EXPECT_NO_THROW(Parametrization::instance().update("domain", "byte", data, 5));
    EXPECT_THROWS_AS(Parametrization::instance().update("domain", "byte", data2, 5), MetadataException);

    // Expect throw because payloadElementType is not given
    EXPECT_THROWS_AS(Parametrization::instance().update(message::Message({
                         {
                             message::Message::Tag::Parametrization,
                             message::Peer{},
                             message::Peer{},
                             Metadata{{"payloadKey", "domain"}},
                         },
                         message::SharedPayload{message::PayloadReference{data, 5}},
                     })),
                     MetadataException);

    // Expect throw because of empty payload although payloadKey is given
    EXPECT_THROWS_AS(Parametrization::instance().update(message::Message({
                         {
                             message::Message::Tag::Parametrization,
                             message::Peer{},
                             message::Peer{},
                             Metadata{{"payloadKey", "domain"}},
                         },
                     })),
                     MetadataException);


    // Try again with payloadElementType
    EXPECT_NO_THROW(Parametrization::instance().update(message::Message({
        {
            message::Message::Tag::Parametrization,
            message::Peer{},
            message::Peer{},
            Metadata{{"c", 3}, {"payloadKey", "domain"}, {"payloadElementType", "byte"}},
        },
        message::SharedPayload{message::PayloadReference{data, 5}},
    })));
    // Try again with different data - expect to fail because domain is already given
    EXPECT_THROWS_AS(Parametrization::instance().update(message::Message({
                         {
                             message::Message::Tag::Parametrization,
                             message::Peer{},
                             message::Peer{},
                             Metadata{{"payloadKey", "domain"}, {"payloadElementType", "byte"}},
                         },
                         message::SharedPayload{message::PayloadReference{data2, 5 * sizeof(unsigned char)}},
                     })),
                     MetadataException);

    EXPECT(par.find("payloadKey") == par.end());
    EXPECT(par.find("payloadElementType") == par.end());
    EXPECT(par.find("domain") != par.end());
    EXPECT(par.find("c") != par.end());
    EXPECT(par.get<std::int64_t>("c") == 3);
    const auto& domain = par.get<std::vector<unsigned char>>("domain");
    EXPECT(domain[0] == 0x1);
    EXPECT(domain[1] == 0x2);
    EXPECT(domain[2] == 0x3);
    EXPECT(domain[3] == 0x4);
    EXPECT(domain[4] == 0x5);


    std::int32_t plIn[5] = {1, 2, 3, 4, 5};
    EXPECT_NO_THROW(Parametrization::instance().update(message::Message({
        {
            message::Message::Tag::Parametrization,
            message::Peer{},
            message::Peer{},
            Metadata{{"payloadKey", "pl"}, {"payloadElementType", "int32"}},
        },
        message::SharedPayload{message::PayloadReference{plIn, 5 * sizeof(std::int32_t)}},
    })));
    EXPECT(par.find("pl") != par.end());
    // Metadata only supports int64...
    const auto& pl = par.get<std::vector<std::int64_t>>("pl");
    EXPECT(pl[0] == 1);
    EXPECT(pl[1] == 2);
    EXPECT(pl[2] == 3);
    EXPECT(pl[3] == 4);
    EXPECT(pl[4] == 5);


    float pvIn[5] = {0.1, 0.2, 0.3, 0.4, 0.5};
    EXPECT_NO_THROW(Parametrization::instance().update(message::Message({
        {
            message::Message::Tag::Parametrization,
            message::Peer{},
            message::Peer{},
            Metadata{{"payloadKey", "pv"}, {"payloadElementType", "real32"}},
        },
        message::SharedPayload{message::PayloadReference{pvIn, 5 * sizeof(float)}},
    })));
    EXPECT(par.find("pv") != par.end());
    const auto& pv = par.get<std::vector<float>>("pv");
    EXPECT(pv[0] == 0.1f);
    EXPECT(pv[1] == 0.2f);
    EXPECT(pv[2] == 0.3f);
    EXPECT(pv[3] == 0.4f);
    EXPECT(pv[4] == 0.5f);


    double pv2In[5] = {0.1, 0.2, 0.3, 0.4, 0.5};
    EXPECT_NO_THROW(Parametrization::instance().update(message::Message({
        {
            message::Message::Tag::Parametrization,
            message::Peer{},
            message::Peer{},
            Metadata{{"payloadKey", "pv2"}, {"payloadElementType", "real64"}},
        },
        message::SharedPayload{message::PayloadReference{pv2In, 5 * sizeof(double)}},
    })));
    EXPECT(par.find("pv2") != par.end());
    const auto& pv2 = par.get<std::vector<double>>("pv2");
    EXPECT(pv2[0] == 0.1);
    EXPECT(pv2[1] == 0.2);
    EXPECT(pv2[2] == 0.3);
    EXPECT(pv2[3] == 0.4);
    EXPECT(pv2[4] == 0.5);
};


}  // namespace multio::test

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

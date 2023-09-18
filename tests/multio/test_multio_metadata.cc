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
#include "multio/util/VariantHelpers.h"


namespace multio::test {

using multio::message::Metadata;
using multio::message::MetadataException;
using multio::message::MetadataKeyException;
using multio::message::MetadataMissingKeyException;
using multio::message::MetadataValue;
using multio::message::MetadataWrongTypeException;
using multio::message::Null;


CASE("Test default initialization of MetadataValue") {
    MetadataValue mv{};
    EXPECT_NO_THROW(mv.get<Null>());
};


Metadata createScalarMetadata() {
    return Metadata{
        {"paramId", 123L},
        {"null", {}},  // Default initialization should create a null value
        {"bool", true},
        {"byte", static_cast<std::int8_t>(8)},
        {"int16", static_cast<std::int16_t>(16)},
        {"int32", static_cast<std::int32_t>(32)},
        {"int64", static_cast<std::int64_t>(64)},
        {"string", std::string{"string"}},
    };
};

CASE("Test getting scala values") {
    Metadata m = createScalarMetadata();
    EXPECT_NO_THROW(m.get("null").get<Null>());
    EXPECT_NO_THROW(m.get<Null>("null"));
    EXPECT_NO_THROW(m.getOpt("null").value().get<Null>());
    EXPECT_NO_THROW(m.getOpt<Null>("null").value());

    EXPECT(m.get<std::int64_t>("paramId") == 123L);
    EXPECT(m.get("paramId").get<std::int64_t>() == 123L);
    EXPECT_THROWS_AS(m.get<std::string>("paramId"), MetadataException);  // TODO test nested MetadataWrongTypeException
    EXPECT_THROWS_AS(m.get("paramId").get<std::string>(),
                     MetadataException);  // TODO test nested MetadataWrongTypeException

    EXPECT(m.get<bool>("bool") == true);
    EXPECT(m.get("bool").get<bool>() == true);
    EXPECT_THROWS_AS(m.get<std::int8_t>("bool"), MetadataException);  // TODO test nested MetadataWrongTypeException
    EXPECT_THROWS_AS(m.get("bool").get<std::int8_t>(),
                     MetadataException);  // TODO test nested MetadataWrongTypeException

    EXPECT(m.get<std::int8_t>("byte") == 8);
    EXPECT(m.get("byte").get<std::int8_t>() == 8);
    EXPECT_THROWS_AS(m.get<std::int16_t>("byte"), MetadataException);  // TODO test nested MetadataWrongTypeException
    EXPECT_THROWS_AS(m.get("byte").get<std::int16_t>(),
                     MetadataException);  // TODO test nested MetadataWrongTypeException

    EXPECT(m.get<std::int16_t>("int16") == 16);
    EXPECT(m.get("int16").get<std::int16_t>() == 16);
    EXPECT_THROWS_AS(m.get<std::int32_t>("int16"), MetadataException);  // TODO test nested MetadataWrongTypeException
    EXPECT_THROWS_AS(m.get("int16").get<std::int32_t>(),
                     MetadataException);  // TODO test nested MetadataWrongTypeException

    EXPECT(m.get<std::int32_t>("int32") == 32);
    EXPECT(m.get("int32").get<std::int32_t>() == 32);
    EXPECT_THROWS_AS(m.get<std::int64_t>("int32"), MetadataException);  // TODO test nested MetadataWrongTypeException
    EXPECT_THROWS_AS(m.get("int32").get<std::int64_t>(),
                     MetadataException);  // TODO test nested MetadataWrongTypeException

    EXPECT(m.get<std::int64_t>("int64") == 64);
    EXPECT(m.get("int64").get<std::int64_t>() == 64);
    EXPECT_THROWS_AS(m.get<Null>("int64"), MetadataException);        // TODO test nested MetadataWrongTypeException
    EXPECT_THROWS_AS(m.get("int64").get<Null>(), MetadataException);  // TODO test nested MetadataWrongTypeException

    EXPECT_THROWS_AS(m.get<Null>("imagine"), MetadataException);  // TODO test nested MetadataMissingKeyException
    EXPECT_THROWS_AS(m.get("imagine"), MetadataException);        // TODO test nested MetadataMissingKeyException
};


CASE("Test modifying values") {
    Metadata m = createScalarMetadata();
    auto& n = m.get("null");

    EXPECT_NO_THROW(n.get<Null>());
    // Set value by ref
    n = "null string";
    EXPECT(m.get<std::string>("null") == "null string");


    // Set value by overwrite
    m.set("null", static_cast<std::int64_t>(0));
    EXPECT(m.get<std::int64_t>("null") == 0);

    // Try setting value and lookup
    auto [itToNull, hasBeenSet] = m.trySet("null", Null{});

    EXPECT(!hasBeenSet);
    EXPECT(itToNull != m.end());
    EXPECT(itToNull->second.get<std::int64_t>() == 0);
    EXPECT(m.get<std::int64_t>("null") == 0);


    auto [itToNewKey, newKeyHasBeenSet] = m.trySet("newKey", Null{});

    EXPECT(newKeyHasBeenSet);
    EXPECT_NO_THROW(itToNewKey->second.get<Null>());

    itToNewKey->second = "newValue";
    EXPECT(m.get<std::string>("newKey") == "newValue");

    // Try get move
    auto val = std::move(m).get("newKey");
    EXPECT(val.get<std::string>() == "newValue");
    EXPECT(itToNewKey->second.get<std::string>() == "");
    EXPECT(m.get<std::string>("newKey") == "");
};


CASE("Test setting and getting vector values") {
    Metadata m;

    m.set("pl", std::vector<std::int64_t>{0, 1, 2, 3, 5, 6});

    {  // Block for testing purpose
        auto searchPL = m.find("pl");
        EXPECT(searchPL != m.end());
    }


    // Possible real code
    if (auto searchPL = m.find("pl"); searchPL != m.end()) {
        EXPECT_NO_THROW(searchPL->second.get<std::vector<std::int64_t>>());
        auto& vec = searchPL->second.get<std::vector<std::int64_t>>();

        EXPECT(vec.size() == 6);
    }
};

CASE("Test setting, getting and merging nested metadata") {
    Metadata m;

    // Example action in pipeline adds encoderOVerwrite to pass down to encoder
    m.set("encoderOverwrites", Metadata{
                                   {"typeOfLevel", "oceanModel"},
                                   {"localDefinitionNumber", 14},
                               });

    // Another action (e.g. metadata-mapping) wants to add encoderOverwrites if not already given
    {  // Block for testing purpose
        auto searchEncOv = m.find("encoderOverwrites");
        EXPECT(searchEncOv != m.end());
    }

    // Possible real code
    if (auto searchEncOv = m.find("encoderOverwrites"); searchEncOv != m.end()) {
        EXPECT_NO_THROW(searchEncOv->second.get<Metadata>());
        auto& encOv = searchEncOv->second.get<Metadata>();

        // Merging allows extracts and moves values from an existing metadata only if they are not already contained
        Metadata mergeByRef{{"encodeBitsPerValue", 12L}, {"typeOfLevel", "unimportantValue"}};
        encOv.merge(mergeByRef);

        EXPECT(mergeByRef.size() == 1);
        EXPECT(mergeByRef.get<std::string>("typeOfLevel") == "unimportantValue");
        EXPECT(encOv.size() == 3);
        EXPECT(encOv.get<std::int64_t>("encodeBitsPerValue") == 12L);
        EXPECT(encOv.get<std::string>("typeOfLevel") == "oceanModel");

        // Merging also works on rvalues
        encOv.merge(
            Metadata{{"encodeBitsPerValue", 122L}, {"typeOfLevel", "dump me"}, {"gridType", "unstructured_grid"}});
        EXPECT(encOv.size() == 4);
        EXPECT(encOv.get<std::int64_t>("encodeBitsPerValue") == 12L);
        EXPECT(encOv.get<std::string>("typeOfLevel") == "oceanModel");
        EXPECT(encOv.get<std::string>("gridType") == "unstructured_grid");


        // Updating (overwriting) is possible without changing the passed metadata.
        // This may invalidate existing references
        encOv.update(Metadata{
            {"encodeBitsPerValue", 123L}, {"typeOfLevel", "oceanModelLayer"}, {"gridType", "unstructured_grid"}});
        EXPECT(encOv.size() == 4);
        EXPECT(encOv.get<std::int64_t>("encodeBitsPerValue") == 123L);
        EXPECT(encOv.get<std::string>("typeOfLevel") == "oceanModelLayer");
        EXPECT(encOv.get<std::string>("gridType") == "unstructured_grid");
    }


    // Finally the encoder action can iterator all values and set it somewhere else
    if (auto searchEncOv = m.find("encoderOverwrites"); searchEncOv != m.end()) {
        EXPECT_NO_THROW(searchEncOv->second.get<Metadata>());
        auto encOv = std::move(searchEncOv->second.get<Metadata>());

        int countStringVals = 0;
        int countIntegerVals = 0;
        int countEveryThingElse = 0;
        for (auto&& keyVal : std::move(encOv)) {
            keyVal.second.visit(eckit::Overloaded{
                [&](auto&& val) -> util::IfTypeOf<decltype(val), message::MetadataStringTypes> { ++countStringVals; },
                [&](auto&& val) -> util::IfTypeOf<decltype(val), message::MetadataIntegerTypes> { ++countIntegerVals; },
                [&](auto&& val)
                    -> util::IfTypeNotOf<decltype(val), util::MergeTypeList_t<message::MetadataStringTypes,
                                                                              message::MetadataIntegerTypes>> {
                    ++countEveryThingElse;
                },
            });
        }
        EXPECT(countStringVals == 2);
        EXPECT(countIntegerVals == 2);
        EXPECT(countEveryThingElse == 0);
    };
}


}  // namespace multio::test

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

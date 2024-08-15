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

using multio::message::BaseMetadata;
using multio::message::Metadata;
using multio::message::MetadataException;
using multio::message::MetadataKeyException;
using multio::message::MetadataMissingKeyException;
using multio::message::MetadataTypes;
using multio::message::MetadataValue;
using multio::message::MetadataWrongTypeException;
using multio::message::Null;


CASE("Test default initialization of MetadataValue") {
    MetadataValue mv{};
    EXPECT_NO_THROW(mv.get<Null>());
};


Metadata createScalarMetadata() {
    return Metadata{
        {"paramId", 123L}, {"null", {}},  // Default initialization should create a null value
        {"bool", true},    {"double", 0.123}, {"int64", 64}, {"string", "string"},
    };
};

CASE("Test getting scala values") {
    Metadata m = createScalarMetadata();
    const Metadata mc = createScalarMetadata();

    EXPECT_NO_THROW(m.get("null").get<Null>());
    EXPECT_NO_THROW(m.get<Null>("null"));
    EXPECT_NO_THROW(m.getOpt("null").value().get<Null>());
    EXPECT_NO_THROW(m.getOpt<Null>("null").value());
    EXPECT_NO_THROW(mc.get("null").get<Null>());
    EXPECT_NO_THROW(mc.get<Null>("null"));
    EXPECT_NO_THROW(mc.getOpt("null").value().get<Null>());
    EXPECT_NO_THROW(mc.getOpt<Null>("null").value());

    EXPECT(m.get<std::int64_t>("paramId") == 123L);
    EXPECT(m.get("paramId").get<std::int64_t>() == 123L);
    EXPECT(mc.get<std::int64_t>("paramId") == 123L);
    EXPECT(mc.get("paramId").get<std::int64_t>() == 123L);
    EXPECT(createScalarMetadata().get<std::int64_t>("paramId") == 123L);        // Test rvalue get access
    EXPECT(createScalarMetadata().get("paramId").get<std::int64_t>() == 123L);  // Test rvalue get access
    EXPECT_THROWS_AS(m.get<std::string>("paramId"), MetadataException);  // TODO test nested MetadataWrongTypeException
    EXPECT_THROWS_AS(m.get("paramId").get<std::string>(),
                     MetadataException);  // TODO test nested MetadataWrongTypeException

    EXPECT(m.get<double>("double") == 0.123);
    EXPECT(m.get("double").get<double>() == 0.123);
    EXPECT(mc.get<double>("double") == 0.123);
    EXPECT(mc.get("double").get<double>() == 0.123);
    EXPECT(createScalarMetadata().get<double>("double") == 0.123);        // Test rvalue get access
    EXPECT(createScalarMetadata().get("double").get<double>() == 0.123);  // Test rvalue get access
    EXPECT_THROWS_AS(m.get<std::string>("double"), MetadataException);    // TODO test nested MetadataWrongTypeException
    EXPECT_THROWS_AS(m.get("double").get<std::string>(),
                     MetadataException);  // TODO test nested MetadataWrongTypeException

    EXPECT(m.get<std::string>("string") == std::string("string"));
    EXPECT(m.get("string").get<std::string>() == std::string("string"));
    EXPECT(mc.get<std::string>("string") == std::string("string"));
    EXPECT(mc.get("string").get<std::string>() == std::string("string"));
    EXPECT(createScalarMetadata().get<std::string>("string") == std::string("string"));        // Test rvalue get access
    EXPECT(createScalarMetadata().get("string").get<std::string>() == std::string("string"));  // Test rvalue get access
    EXPECT_THROWS_AS(m.get<std::int64_t>("string"), MetadataException);  // TODO test nested MetadataWrongTypeException
    EXPECT_THROWS_AS(m.get("string").get<std::int64_t>(),
                     MetadataException);  // TODO test nested MetadataWrongTypeException

    EXPECT(m.get<bool>("bool") == true);
    EXPECT(m.get("bool").get<bool>() == true);
    EXPECT(mc.get<bool>("bool") == true);
    EXPECT(mc.get("bool").get<bool>() == true);
    EXPECT(createScalarMetadata().get<bool>("bool") == true);          // Test rvalue get access
    EXPECT(createScalarMetadata().get("bool").get<bool>() == true);    // Test rvalue get access
    EXPECT_THROWS_AS(m.get<std::int64_t>("bool"), MetadataException);  // TODO test nested MetadataWrongTypeException
    EXPECT_THROWS_AS(m.get("bool").get<std::int64_t>(),
                     MetadataException);  // TODO test nested MetadataWrongTypeException


    EXPECT(m.get<std::int64_t>("int64") == 64);
    EXPECT(m.get("int64").get<std::int64_t>() == 64);
    EXPECT(mc.get<std::int64_t>("int64") == 64);
    EXPECT(mc.get("int64").get<std::int64_t>() == 64);
    EXPECT(createScalarMetadata().get<std::int64_t>("int64") == 64);        // Test rvalue get access
    EXPECT(createScalarMetadata().get("int64").get<std::int64_t>() == 64);  // Test rvalue get access
    EXPECT_THROWS_AS(m.get<Null>("int64"), MetadataException);        // TODO test nested MetadataWrongTypeException
    EXPECT_THROWS_AS(m.get("int64").get<Null>(), MetadataException);  // TODO test nested MetadataWrongTypeException

    EXPECT_THROWS_AS(m.get<Null>("imagine"), MetadataException);   // TODO test nested MetadataMissingKeyException
    EXPECT_THROWS_AS(m.get("imagine"), MetadataException);         // TODO test nested MetadataMissingKeyException
    EXPECT_THROWS_AS(mc.get<Null>("imagine"), MetadataException);  // TODO test nested MetadataMissingKeyException
    EXPECT_THROWS_AS(mc.get("imagine"), MetadataException);        // TODO test nested MetadataMissingKeyException
};


CASE("Test modifying values") {
    Metadata m = createScalarMetadata();
    auto& n = m.get("null");

    EXPECT_NO_THROW(n.get<Null>());

    // Set value by ref
    {
        n = "null string";
        EXPECT(m.get<std::string>("null") == "null string");
    }


    // Set value by overwrite
    {
        m.set("null", 0);
        EXPECT(m.get<std::int64_t>("null") == 0);
    }

    // Try setting value and lookup
    {
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
    }


    // Try changing type (double to int)
    {
        auto& d = m.get("double");

        EXPECT_NO_THROW(d.get<double>());
        // Set value by ref
        d = 123;
        EXPECT(m.get<std::int64_t>("double") == 123);
    }

    // Try changing type (int to double)
    {
        auto& i = m.get("paramId");

        EXPECT_NO_THROW(i.get<std::int64_t>());
        // Set value by ref
        i = 0.123;
        EXPECT(m.get<double>("paramId") == 0.123);
    }
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
    m.set("encoder-overwrites", Metadata{
                                    {"typeOfLevel", "oceanModel"},
                                    {"localDefinitionNumber", 14},
                                });

    auto [itToNull, hasBeenSet] = m.trySet("encoder-overwrites", Metadata{{"i will never", "be set"}});

    // Or set via assignment
    m["encoder-overwrites"] = Metadata{
        {"typeOfLevel", "oceanModel"},
        {"localDefinitionNumber", 14},
    };

    // Another action (e.g. metadata-mapping) wants to add encoder-overwrites if not already given
    {  // Block for testing purpose
        auto searchEncOv = m.find("encoder-overwrites");
        EXPECT(searchEncOv != m.end());
    }

    // Possible real code
    if (auto searchEncOv = m.find("encoder-overwrites"); searchEncOv != m.end()) {
        EXPECT_NO_THROW(searchEncOv->second.get<BaseMetadata>());
        auto& encOv = searchEncOv->second.get<BaseMetadata>();

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
        {
            Metadata updateFrom{
                {"encodeBitsPerValue", 123L}, {"typeOfLevel", "oceanModelLayer"}, {"gridType", "healpix"}};


            encOv.updateNoOverwrite(updateFrom);
            EXPECT(encOv.size() == 4);
            EXPECT(encOv.get<std::int64_t>("encodeBitsPerValue") == 12L);
            EXPECT(encOv.get<std::string>("typeOfLevel") == "oceanModel");
            EXPECT(encOv.get<std::string>("gridType") == "unstructured_grid");
            EXPECT(updateFrom.size() == 3);
            EXPECT(updateFrom.get<std::int64_t>("encodeBitsPerValue") == 123L);
            EXPECT(updateFrom.get<std::string>("typeOfLevel") == "oceanModelLayer");
            EXPECT(updateFrom.get<std::string>("gridType") == "healpix");
        }


        {
            Metadata updateFrom{{"encodeBitsPerValue", 123L},
                                {"typeOfLevel", "oceanModelLayer"},
                                {"gridType", "healpix"},
                                {"new_test_key", "test_value"}};

            encOv.updateNoOverwrite(std::move(updateFrom));
            EXPECT(encOv.size() == 5);
            EXPECT(encOv.get<std::int64_t>("encodeBitsPerValue") == 12L);
            EXPECT(encOv.get<std::string>("typeOfLevel") == "oceanModel");
            EXPECT(encOv.get<std::string>("gridType") == "unstructured_grid");
            EXPECT(encOv.get<std::string>("new_test_key") == "test_value");
        }


        {
            Metadata updateFrom{
                {"encodeBitsPerValue", 123L}, {"typeOfLevel", "oceanModelLayer"}, {"gridType", "healpix"}};

            encOv.updateOverwrite(updateFrom);
            EXPECT(encOv.size() == 5);
            EXPECT(encOv.get<std::int64_t>("encodeBitsPerValue") == 123L);
            EXPECT(encOv.get<std::string>("typeOfLevel") == "oceanModelLayer");
            EXPECT(encOv.get<std::string>("gridType") == "healpix");
            EXPECT(encOv.get<std::string>("new_test_key") == "test_value");
        }

        {
            Metadata updateFrom{{"encodeBitsPerValue", 123L},
                                {"typeOfLevel", "oceanModelLayer"},
                                {"gridType", "healpix"},
                                {"new_test_key", "test_value2"}};

            encOv.updateOverwrite(std::move(updateFrom));
            EXPECT(encOv.size() == 5);
            EXPECT(encOv.get<std::int64_t>("encodeBitsPerValue") == 123L);
            EXPECT(encOv.get<std::string>("typeOfLevel") == "oceanModelLayer");
            EXPECT(encOv.get<std::string>("gridType") == "healpix");
            EXPECT(encOv.get<std::string>("new_test_key") == "test_value2");
        }
    }

    Metadata m2{m};  // copy m for later

    // Finally the encoder action can iterator all values and set it somewhere else
    if (auto searchEncOv = m.find("encoder-overwrites"); searchEncOv != m.end()) {
        EXPECT_NO_THROW(searchEncOv->second.get<BaseMetadata>());
        auto encOv = std::move(searchEncOv->second.get<BaseMetadata>());

        {
            int countStringVals1 = 0;
            int countIntegerVals1 = 0;
            int countEveryThingElse1 = 0;
            // Iterate and visit with const ref
            for (const auto& keyVal : encOv) {
                keyVal.second.visit(eckit::Overloaded{
                    [&](const auto& val) -> util::IfTypeOf<decltype(val), MetadataTypes::Strings> {
                        ++countStringVals1;
                    },
                    [&](const auto& val) -> util::IfTypeOf<decltype(val), MetadataTypes::Integers> {
                        ++countIntegerVals1;
                    },
                    [&](const auto& val)
                        -> util::IfTypeNotOf<decltype(val),
                                             util::MergeTypeList_t<MetadataTypes::Strings, MetadataTypes::Integers>> {
                        ++countEveryThingElse1;
                    },
                });
            }
            EXPECT_EQUAL(countStringVals1, 3);
            EXPECT_EQUAL(countIntegerVals1, 2);
            EXPECT_EQUAL(countEveryThingElse1, 0);

            EXPECT_EQUAL(encOv.size(), 5);
        }


        // Alternative to the block given above
        {
            int countStringVals2 = 0;
            int countIntegerVals2 = 0;
            int countEveryThingElse2 = 0;
            // Iterate and visit with rvalue
            for (auto&& keyVal : encOv) {
                keyVal.second.visit([&](auto&& val) -> void {
                    if constexpr (util::TypeListContains_v<std::decay_t<decltype(val)>, MetadataTypes::Strings>) {
                        ++countStringVals2;
                    }
                    else if constexpr (util::TypeListContains_v<std::decay_t<decltype(val)>, MetadataTypes::Integers>) {
                        ++countIntegerVals2;
                    }
                    else {
                        ++countEveryThingElse2;
                    }
                });
            }
            EXPECT_EQUAL(countStringVals2, 3);
            EXPECT_EQUAL(countIntegerVals2, 2);
            EXPECT_EQUAL(countEveryThingElse2, 0);

            // Should be not empty as movement did not happen at all
            EXPECT_EQUAL(encOv.size(), 5);
        }

        // Test extraction with visit & movement
        {
            std::vector<std::string> stringValues;
            std::vector<std::int64_t> integerValues;

            for (auto&& keyVal : encOv) {
                keyVal.second.visit([&](auto&& val) -> void {
                    if constexpr (util::TypeListContains_v<std::decay_t<decltype(val)>, MetadataTypes::Strings>) {
                        stringValues.emplace_back(std::move(val));
                    }
                    else if constexpr (util::TypeListContains_v<std::decay_t<decltype(val)>, MetadataTypes::Integers>) {
                        integerValues.emplace_back(std::move(val));
                    }
                });
            }

            EXPECT_EQUAL(stringValues.size(), 3);
            EXPECT_EQUAL(integerValues.size(), 2);


            // Visit again and expect strings to be empty (through movement)
            for (const auto& keyVal : encOv) {
                keyVal.second.visit([&](const auto& val) -> void {
                    if constexpr (util::TypeListContains_v<std::decay_t<decltype(val)>, MetadataTypes::Strings>) {
                        EXPECT_EQUAL(val, "");
                    }
                });
            }
        }
    };


    // Test equality values for nested rvalue access
    EXPECT(std::move(m2).get<BaseMetadata>("encoder-overwrites").get<std::int64_t>("encodeBitsPerValue") == 123L);
    EXPECT(std::move(m2).get<BaseMetadata>("encoder-overwrites").get<std::string>("typeOfLevel") == "oceanModelLayer");
    EXPECT(std::move(m2).get<BaseMetadata>("encoder-overwrites").get<std::string>("gridType") == "healpix");
}

CASE("Test visit with unwrapped unique ptr") {
    {
        MetadataValue v{BaseMetadata{{"key1", "value1"}}};

        static_cast<MetadataValue&>(v).visit(
            eckit::Overloaded{[](BaseMetadata&) { EXPECT(true); }, [](auto& other) { EXPECT(false); }});

        static_cast<const MetadataValue&>(v).visit(
            eckit::Overloaded{[](const BaseMetadata&) { EXPECT(true); }, [](const auto& other) { EXPECT(false); }});

        static_cast<MetadataValue&&>(v).visit(
            eckit::Overloaded{[](BaseMetadata&&) { EXPECT(true); }, [](auto&& other) { EXPECT(false); }});
    }

    {
        MetadataValue v{BaseMetadata{{"key1", "value1"}}};

        // This is rather a static complication test than a dynamic test
        BaseMetadata& ref = static_cast<MetadataValue&>(v).get<BaseMetadata>();
        const BaseMetadata& cref = static_cast<const MetadataValue&>(v).get<BaseMetadata>();
        BaseMetadata&& tref = static_cast<MetadataValue&&>(v).get<BaseMetadata>();

        // All references should be valid
        EXPECT_EQUAL(ref.size(), 1);
        EXPECT_EQUAL(cref.size(), 1);
        EXPECT_EQUAL(tref.size(), 1);

        BaseMetadata moved{std::move(tref)};
        // After movement references should point to an empty metadata
        EXPECT_EQUAL(ref.size(), 0);
        EXPECT_EQUAL(cref.size(), 0);
    }
}


}  // namespace multio::test

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

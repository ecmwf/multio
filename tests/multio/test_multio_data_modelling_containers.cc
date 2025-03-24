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

#include "eckit/config/LocalConfiguration.h"
#include "eckit/config/YAMLConfiguration.h"
#include "eckit/testing/Test.h"
#include "multio/datamod/ContainerInterop.h"
#include "multio/message/Metadata.h"
#include "multio/message/Parametrization.h"


// Both `enum` or `enum class` work
enum class TestKeys : std::uint64_t
{
    Key1,
    Key2,
    Key3,
    Key4,
    Key5,
    Key6,
};
// enum TestKeys
// {
//     Key1,
//     Key2,
//     Key3,
//     Key4,
//     Key5,
//     Key6,
// };


namespace multio::datamod {
MULTIO_KEY_SET_DESCRIPTION(TestKeys,                                                                             //
                           "test",                                                                               //
                                                                                                                 //
                           KeyDef<TestKeys::Key1, std::string>{"key1"}, KeyDef<TestKeys::Key2, double>{"key2"},  //
                           KeyDef<TestKeys::Key3, std::int64_t>{"key3"},                                         //
                           KeyDef<TestKeys::Key4, std::int64_t>{"key4"}.tagOptional(),                           //
                           KeyDef<TestKeys::Key5, bool>{"key5"},                                                 //
                           KeyDef<TestKeys::Key6, std::vector<double>>{"key6"}
                               .tagOptional());  // Using arrays with local configuration can be very dangerous...
}


namespace multio::test {

using multio::message::Metadata;

// Index of the KeyValue variants used to compare
constexpr std::size_t VARIANT_VAL_INDEX = 1;
constexpr std::size_t VARIANT_REF_INDEX = 2;


CASE("Test metadata by value from global parametrization") {
    Metadata localMd{{"key1", "val1"}, {"key2", 2.0}, {"key3", 3}, {"key5", true}};

    {
        using namespace datamod;
        // Should ref all values
        auto testKeysRef = read(keySet<TestKeys>(), localMd);

        EXPECT_EQUAL(key<TestKeys::Key1>(testKeysRef).get(), "val1");
        EXPECT_EQUAL(key<TestKeys::Key2>(testKeysRef).get(), 2.0);
        EXPECT_EQUAL(key<TestKeys::Key3>(testKeysRef).get(), 3);
        EXPECT_EQUAL(key<TestKeys::Key4>(testKeysRef).isMissing(), true);
        EXPECT_EQUAL(key<TestKeys::Key5>(testKeysRef).get(), true);
        EXPECT_EQUAL(key<TestKeys::Key6>(testKeysRef).isMissing(), true);

        EXPECT_EQUAL(key<TestKeys::Key1>(testKeysRef).value.index(), VARIANT_REF_INDEX);
        EXPECT_EQUAL(key<TestKeys::Key2>(testKeysRef).value.index(), VARIANT_REF_INDEX);
        EXPECT_EQUAL(key<TestKeys::Key3>(testKeysRef).value.index(), VARIANT_REF_INDEX);
        EXPECT_EQUAL(key<TestKeys::Key5>(testKeysRef).value.index(), VARIANT_REF_INDEX);
    }

    {
        using namespace datamod;
        // Should copy all values
        auto testKeysValue = readByValue(keySet<TestKeys>(), localMd);

        EXPECT_EQUAL(key<TestKeys::Key1>(testKeysValue).get(), "val1");
        EXPECT_EQUAL(key<TestKeys::Key2>(testKeysValue).get(), 2.0);
        EXPECT_EQUAL(key<TestKeys::Key3>(testKeysValue).get(), 3);
        EXPECT_EQUAL(key<TestKeys::Key4>(testKeysValue).isMissing(), true);
        EXPECT_EQUAL(key<TestKeys::Key5>(testKeysValue).get(), true);
        EXPECT_EQUAL(key<TestKeys::Key6>(testKeysValue).isMissing(), true);

        EXPECT_EQUAL(key<TestKeys::Key1>(testKeysValue).value.index(), VARIANT_VAL_INDEX);
        EXPECT_EQUAL(key<TestKeys::Key2>(testKeysValue).value.index(), VARIANT_VAL_INDEX);
        EXPECT_EQUAL(key<TestKeys::Key3>(testKeysValue).value.index(), VARIANT_VAL_INDEX);
        EXPECT_EQUAL(key<TestKeys::Key5>(testKeysValue).value.index(), VARIANT_VAL_INDEX);
    }

    // Now setting global metadata
    message::Parametrization::instance().update(Metadata{{"key6", std::vector<double>{{1.0, 2.0, 3.0}}}});

    {
        using namespace datamod;
        // Should ref all values
        auto testKeysRef = read(keySet<TestKeys>(), localMd);

        EXPECT_EQUAL(key<TestKeys::Key1>(testKeysRef).get(), "val1");
        EXPECT_EQUAL(key<TestKeys::Key2>(testKeysRef).get(), 2.0);
        EXPECT_EQUAL(key<TestKeys::Key3>(testKeysRef).get(), 3);
        EXPECT_EQUAL(key<TestKeys::Key4>(testKeysRef).isMissing(), true);
        EXPECT_EQUAL(key<TestKeys::Key5>(testKeysRef).get(), true);
        EXPECT_EQUAL(key<TestKeys::Key6>(testKeysRef).has(), true);
        EXPECT_EQUAL(key<TestKeys::Key6>(testKeysRef).get(), (std::vector<double>{{1.0, 2.0, 3.0}}));

        EXPECT_EQUAL(key<TestKeys::Key1>(testKeysRef).value.index(), VARIANT_REF_INDEX);
        EXPECT_EQUAL(key<TestKeys::Key2>(testKeysRef).value.index(), VARIANT_REF_INDEX);
        EXPECT_EQUAL(key<TestKeys::Key3>(testKeysRef).value.index(), VARIANT_REF_INDEX);
        EXPECT_EQUAL(key<TestKeys::Key5>(testKeysRef).value.index(), VARIANT_REF_INDEX);
        EXPECT_EQUAL(key<TestKeys::Key6>(testKeysRef).value.index(), VARIANT_REF_INDEX);
    }

    {
        using namespace datamod;
        // Should copy all values
        auto testKeysValue = readByValue(keySet<TestKeys>(), localMd);

        EXPECT_EQUAL(key<TestKeys::Key1>(testKeysValue).get(), "val1");
        EXPECT_EQUAL(key<TestKeys::Key2>(testKeysValue).get(), 2.0);
        EXPECT_EQUAL(key<TestKeys::Key3>(testKeysValue).get(), 3);
        EXPECT_EQUAL(key<TestKeys::Key4>(testKeysValue).isMissing(), true);
        EXPECT_EQUAL(key<TestKeys::Key5>(testKeysValue).get(), true);
        EXPECT_EQUAL(key<TestKeys::Key6>(testKeysValue).has(), true);
        EXPECT_EQUAL(key<TestKeys::Key6>(testKeysValue).get(), (std::vector<double>{{1.0, 2.0, 3.0}}));

        EXPECT_EQUAL(key<TestKeys::Key1>(testKeysValue).value.index(), VARIANT_VAL_INDEX);
        EXPECT_EQUAL(key<TestKeys::Key2>(testKeysValue).value.index(), VARIANT_VAL_INDEX);
        EXPECT_EQUAL(key<TestKeys::Key3>(testKeysValue).value.index(), VARIANT_VAL_INDEX);
        EXPECT_EQUAL(key<TestKeys::Key5>(testKeysValue).value.index(), VARIANT_VAL_INDEX);
        EXPECT_EQUAL(key<TestKeys::Key6>(testKeysValue).value.index(), VARIANT_REF_INDEX);
    }
};


CASE("Test parse simple config 1") {
    const std::string exampleJson(
        R"json({
            "key1": "val1",
            "key2": 2.0,
            "key3": 3,
            "key5": true
        })json");

    eckit::LocalConfiguration conf{eckit::YAMLConfiguration(exampleJson)};

    using namespace multio::datamod;
    auto testKeys = read(keySet<TestKeys>(), conf);

    EXPECT_EQUAL(key<TestKeys::Key1>(testKeys).get(), "val1");
    EXPECT_EQUAL(key<TestKeys::Key2>(testKeys).get(), 2.0);
    EXPECT_EQUAL(key<TestKeys::Key3>(testKeys).get(), 3);
    EXPECT_EQUAL(key<TestKeys::Key4>(testKeys).isMissing(), true);
    EXPECT_EQUAL(key<TestKeys::Key5>(testKeys).get(), true);
    EXPECT_EQUAL(key<TestKeys::Key6>(testKeys).isMissing(), true);
};


CASE("Test parse simple config 2") {
    const std::string exampleJson(
        R"json({
            "key1": "val1",
            "key2": 2.0,
            "key3": 3,
            "key4": 4,
            "key5": true
        })json");

    eckit::LocalConfiguration conf{eckit::YAMLConfiguration(exampleJson)};

    using namespace multio::datamod;
    auto testKeys = read(keySet<TestKeys>(), conf);

    EXPECT_EQUAL(key<TestKeys::Key1>(testKeys).get(), "val1");
    EXPECT_EQUAL(key<TestKeys::Key2>(testKeys).get(), 2.0);
    EXPECT_EQUAL(key<TestKeys::Key3>(testKeys).get(), 3);
    EXPECT_EQUAL(key<TestKeys::Key4>(testKeys).get(), 4);
    EXPECT_EQUAL(key<TestKeys::Key5>(testKeys).get(), true);
    EXPECT_EQUAL(key<TestKeys::Key6>(testKeys).isMissing(), true);
};


CASE("Test parse simple config 3") {
    const std::string exampleJson(
        R"json({
            "key1": "val1",
            "key2": 2.0,
            "key3": 3,
            "key4": null,
            "key5": true
        })json");

    eckit::LocalConfiguration conf{eckit::YAMLConfiguration(exampleJson)};

    using namespace multio::datamod;
    auto testKeys = read(keySet<TestKeys>(), conf);

    EXPECT_EQUAL(key<TestKeys::Key1>(testKeys).get(), "val1");
    EXPECT_EQUAL(key<TestKeys::Key2>(testKeys).get(), 2.0);
    EXPECT_EQUAL(key<TestKeys::Key3>(testKeys).get(), 3);
    EXPECT_EQUAL(key<TestKeys::Key4>(testKeys).isMissing(), true);
    EXPECT_EQUAL(key<TestKeys::Key5>(testKeys).get(), true);
    EXPECT_EQUAL(key<TestKeys::Key6>(testKeys).isMissing(), true);
};


CASE("Test parse simple config with unspupported values 1") {
    const std::string exampleJson(
        R"json({
            "key1": null,
            "key2": 2.0,
            "key3": 3,
            "key4": 4,
            "key5": true
        })json");

    eckit::LocalConfiguration conf{eckit::YAMLConfiguration(exampleJson)};

    using namespace multio::datamod;
    EXPECT_THROWS(read(keySet<TestKeys>(), conf));
};

CASE("Test parse simple config with unspupported values 2") {
    const std::string exampleJson(
        R"json({
            "key1": {},
            "key2": 2.0,
            "key3": 3,
            "key4": 4,
            "key5": true
        })json");

    eckit::LocalConfiguration conf{eckit::YAMLConfiguration(exampleJson)};

    using namespace multio::datamod;
    EXPECT_THROWS(read(keySet<TestKeys>(), conf));
};


CASE("Test parse config with int arr") {
    const std::string exampleJson(
        R"json({
            "key1": "val1",
            "key2": 2.0,
            "key3": 3,
            "key5": true,
            "key6": [1, 2, 3]
        })json");

    eckit::LocalConfiguration conf{eckit::YAMLConfiguration(exampleJson)};

    using namespace multio::datamod;
    auto testKeys = read(keySet<TestKeys>(), conf);

    EXPECT_EQUAL(key<TestKeys::Key1>(testKeys).get(), "val1");
    EXPECT_EQUAL(key<TestKeys::Key2>(testKeys).get(), 2.0);
    EXPECT_EQUAL(key<TestKeys::Key3>(testKeys).get(), 3);
    EXPECT_EQUAL(key<TestKeys::Key4>(testKeys).isMissing(), true);
    EXPECT_EQUAL(key<TestKeys::Key5>(testKeys).get(), true);

    auto& k6 = key<TestKeys::Key6>(testKeys);
    EXPECT_EQUAL(k6.isMissing(), false);
    EXPECT_EQUAL((k6.get()[0]), 1.0);
    EXPECT_EQUAL((k6.get()[1]), 2.0);
    EXPECT_EQUAL((k6.get()[2]), 3.0);
};

}  // namespace multio::test

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

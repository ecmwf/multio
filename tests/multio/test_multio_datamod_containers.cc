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
#include "multio/datamod/core/EntryParser.h"
#include "multio/message/Metadata.h"
#include "multio/message/Parametrization.h"

namespace multio::test {

namespace dm = multio::datamod;

struct TestKeys {
    dm::Entry<std::string> key1;
    dm::Entry<double> key2;
    dm::Entry<std::int64_t> key3;
    dm::Entry<std::int64_t> key4;
    dm::Entry<bool> key5;
    dm::Entry<std::vector<double>> key6;

    static constexpr std::string_view record_name_ = "test";
    static constexpr auto record_entries_
        = std::make_tuple(dm::entryDef("key1", &TestKeys::key1), dm::entryDef("key2", &TestKeys::key2),
                          dm::entryDef("key3", &TestKeys::key3), dm::entryDef("key4", &TestKeys::key4).tagOptional(),
                          dm::entryDef("key5", &TestKeys::key5), dm::entryDef("key6", &TestKeys::key6).tagOptional());
};


using multio::message::Metadata;

// Index of the KeyValue variants used to compare
constexpr std::size_t VARIANT_VAL_INDEX = 1;
constexpr std::size_t VARIANT_REF_INDEX = 2;


CASE("Test metadata by value from global parametrization") {
    Metadata localMd{{"key1", "val1"}, {"key2", 2.0}, {"key3", 3}, {"key5", true}};
    message::Parametrization::instance().clear();

    {
        using namespace datamod;
        // Should ref all values
        auto testKeysRef = dm::readRecord<TestKeys>(localMd);

        EXPECT_EQUAL(testKeysRef.key1.get(), "val1");
        EXPECT_EQUAL(testKeysRef.key2.get(), 2.0);
        EXPECT_EQUAL(testKeysRef.key3.get(), 3);
        EXPECT_EQUAL(testKeysRef.key4.isSet(), false);
        EXPECT_EQUAL(testKeysRef.key5.get(), true);
        EXPECT_EQUAL(testKeysRef.key6.isSet(), false);

        EXPECT_EQUAL(testKeysRef.key1.value.index(), VARIANT_REF_INDEX);
        EXPECT_EQUAL(testKeysRef.key2.value.index(), VARIANT_REF_INDEX);
        EXPECT_EQUAL(testKeysRef.key3.value.index(), VARIANT_REF_INDEX);
        EXPECT_EQUAL(testKeysRef.key5.value.index(), VARIANT_REF_INDEX);
    }

    {
        using namespace datamod;
        // Should copy all values
        auto testKeysValue = dm::readRecordByValue<TestKeys>(localMd);

        EXPECT_EQUAL(testKeysValue.key1.get(), "val1");
        EXPECT_EQUAL(testKeysValue.key2.get(), 2.0);
        EXPECT_EQUAL(testKeysValue.key3.get(), 3);
        EXPECT_EQUAL(testKeysValue.key4.isSet(), false);
        EXPECT_EQUAL(testKeysValue.key5.get(), true);
        EXPECT_EQUAL(testKeysValue.key6.isSet(), false);

        EXPECT_EQUAL(testKeysValue.key1.value.index(), VARIANT_VAL_INDEX);
        EXPECT_EQUAL(testKeysValue.key2.value.index(), VARIANT_VAL_INDEX);
        EXPECT_EQUAL(testKeysValue.key3.value.index(), VARIANT_VAL_INDEX);
        EXPECT_EQUAL(testKeysValue.key5.value.index(), VARIANT_VAL_INDEX);
    }

    // Now setting global metadata
    message::Parametrization::instance().update(Metadata{{"key6", std::vector<double>{{1.0, 2.0, 3.0}}}});

    {
        using namespace datamod;
        // Should ref all values
        auto testKeysRef = dm::readRecord<TestKeys>(localMd);

        EXPECT_EQUAL(testKeysRef.key1.get(), "val1");
        EXPECT_EQUAL(testKeysRef.key2.get(), 2.0);
        EXPECT_EQUAL(testKeysRef.key3.get(), 3);
        EXPECT_EQUAL(testKeysRef.key4.isSet(), false);
        EXPECT_EQUAL(testKeysRef.key5.get(), true);
        EXPECT_EQUAL(testKeysRef.key6.isSet(), true);
        EXPECT_EQUAL(testKeysRef.key6.get(), (std::vector<double>{{1.0, 2.0, 3.0}}));

        EXPECT_EQUAL(testKeysRef.key1.value.index(), VARIANT_REF_INDEX);
        EXPECT_EQUAL(testKeysRef.key2.value.index(), VARIANT_REF_INDEX);
        EXPECT_EQUAL(testKeysRef.key3.value.index(), VARIANT_REF_INDEX);
        EXPECT_EQUAL(testKeysRef.key5.value.index(), VARIANT_REF_INDEX);
        EXPECT_EQUAL(testKeysRef.key6.value.index(), VARIANT_REF_INDEX);
    }

    {
        using namespace datamod;
        // Should copy all values
        auto testKeysValue = dm::readRecordByValue<TestKeys>(localMd);

        EXPECT_EQUAL(testKeysValue.key1.get(), "val1");
        EXPECT_EQUAL(testKeysValue.key2.get(), 2.0);
        EXPECT_EQUAL(testKeysValue.key3.get(), 3);
        EXPECT_EQUAL(testKeysValue.key4.isSet(), false);
        EXPECT_EQUAL(testKeysValue.key5.get(), true);
        EXPECT_EQUAL(testKeysValue.key6.isSet(), true);
        EXPECT_EQUAL(testKeysValue.key6.get(), (std::vector<double>{{1.0, 2.0, 3.0}}));

        EXPECT_EQUAL(testKeysValue.key1.value.index(), VARIANT_VAL_INDEX);
        EXPECT_EQUAL(testKeysValue.key2.value.index(), VARIANT_VAL_INDEX);
        EXPECT_EQUAL(testKeysValue.key3.value.index(), VARIANT_VAL_INDEX);
        EXPECT_EQUAL(testKeysValue.key5.value.index(), VARIANT_VAL_INDEX);
        EXPECT_EQUAL(testKeysValue.key6.value.index(), VARIANT_REF_INDEX);
    }
};


CASE("Test metadata by value from global parametrization (scoped)") {
    Metadata localMd{{"test-key1", "val1"}, {"test-key2", 2.0}, {"test-key3", 3}, {"test-key5", true}};
    message::Parametrization::instance().clear();

    {
        using namespace datamod;
        // Should ref all values
        auto scopedKeys = scopeRecord(TestKeys{});
        readRecord(scopedKeys, localMd);
        auto testKeysRef = unscopeRecord(std::move(scopedKeys));

        EXPECT_EQUAL(testKeysRef.key1.get(), "val1");
        EXPECT_EQUAL(testKeysRef.key2.get(), 2.0);
        EXPECT_EQUAL(testKeysRef.key3.get(), 3);
        EXPECT_EQUAL(testKeysRef.key4.isSet(), false);
        EXPECT_EQUAL(testKeysRef.key5.get(), true);
        EXPECT_EQUAL(testKeysRef.key6.isSet(), false);

        EXPECT_EQUAL(testKeysRef.key1.value.index(), VARIANT_REF_INDEX);
        EXPECT_EQUAL(testKeysRef.key2.value.index(), VARIANT_REF_INDEX);
        EXPECT_EQUAL(testKeysRef.key3.value.index(), VARIANT_REF_INDEX);
        EXPECT_EQUAL(testKeysRef.key5.value.index(), VARIANT_REF_INDEX);
    }

    {
        using namespace datamod;
        // Should copy all values
        auto scopedKeys = scopeRecord(TestKeys{});
        readRecordByValue(scopedKeys, localMd);
        auto testKeysValue = unscopeRecord(std::move(scopedKeys));

        EXPECT_EQUAL(testKeysValue.key1.get(), "val1");
        EXPECT_EQUAL(testKeysValue.key2.get(), 2.0);
        EXPECT_EQUAL(testKeysValue.key3.get(), 3);
        EXPECT_EQUAL(testKeysValue.key4.isSet(), false);
        EXPECT_EQUAL(testKeysValue.key5.get(), true);
        EXPECT_EQUAL(testKeysValue.key6.isSet(), false);

        EXPECT_EQUAL(testKeysValue.key1.value.index(), VARIANT_VAL_INDEX);
        EXPECT_EQUAL(testKeysValue.key2.value.index(), VARIANT_VAL_INDEX);
        EXPECT_EQUAL(testKeysValue.key3.value.index(), VARIANT_VAL_INDEX);
        EXPECT_EQUAL(testKeysValue.key5.value.index(), VARIANT_VAL_INDEX);
    }

    // Now setting global metadata
    message::Parametrization::instance().update(Metadata{{"test-key6", std::vector<double>{{1.0, 2.0, 3.0}}}});

    {
        using namespace datamod;
        // Should ref all values
        auto scopedKeys = scopeRecord(TestKeys{});
        readRecord(scopedKeys, localMd);
        auto testKeysRef = unscopeRecord(std::move(scopedKeys));

        EXPECT_EQUAL(testKeysRef.key1.get(), "val1");
        EXPECT_EQUAL(testKeysRef.key2.get(), 2.0);
        EXPECT_EQUAL(testKeysRef.key3.get(), 3);
        EXPECT_EQUAL(testKeysRef.key4.isSet(), false);
        EXPECT_EQUAL(testKeysRef.key5.get(), true);
        EXPECT_EQUAL(testKeysRef.key6.isSet(), true);
        EXPECT_EQUAL(testKeysRef.key6.get(), (std::vector<double>{{1.0, 2.0, 3.0}}));

        EXPECT_EQUAL(testKeysRef.key1.value.index(), VARIANT_REF_INDEX);
        EXPECT_EQUAL(testKeysRef.key2.value.index(), VARIANT_REF_INDEX);
        EXPECT_EQUAL(testKeysRef.key3.value.index(), VARIANT_REF_INDEX);
        EXPECT_EQUAL(testKeysRef.key5.value.index(), VARIANT_REF_INDEX);
        EXPECT_EQUAL(testKeysRef.key6.value.index(), VARIANT_REF_INDEX);
    }

    {
        using namespace datamod;
        // Should copy all values
        auto scopedKeys = scopeRecord(TestKeys{});
        readRecordByValue(scopedKeys, localMd);
        auto testKeysValue = unscopeRecord(std::move(scopedKeys));

        EXPECT_EQUAL(testKeysValue.key1.get(), "val1");
        EXPECT_EQUAL(testKeysValue.key2.get(), 2.0);
        EXPECT_EQUAL(testKeysValue.key3.get(), 3);
        EXPECT_EQUAL(testKeysValue.key4.isSet(), false);
        EXPECT_EQUAL(testKeysValue.key5.get(), true);
        EXPECT_EQUAL(testKeysValue.key6.isSet(), true);
        EXPECT_EQUAL(testKeysValue.key6.get(), (std::vector<double>{{1.0, 2.0, 3.0}}));

        EXPECT_EQUAL(testKeysValue.key1.value.index(), VARIANT_VAL_INDEX);
        EXPECT_EQUAL(testKeysValue.key2.value.index(), VARIANT_VAL_INDEX);
        EXPECT_EQUAL(testKeysValue.key3.value.index(), VARIANT_VAL_INDEX);
        EXPECT_EQUAL(testKeysValue.key5.value.index(), VARIANT_VAL_INDEX);
        EXPECT_EQUAL(testKeysValue.key6.value.index(), VARIANT_REF_INDEX);
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

    auto testKeys = dm::readRecord<TestKeys>(conf);

    EXPECT_EQUAL(testKeys.key1.get(), "val1");
    EXPECT_EQUAL(testKeys.key2.get(), 2.0);
    EXPECT_EQUAL(testKeys.key3.get(), 3);
    EXPECT_EQUAL(testKeys.key4.isSet(), false);
    EXPECT_EQUAL(testKeys.key5.get(), true);
    EXPECT_EQUAL(testKeys.key6.isSet(), false);
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

    auto testKeys = dm::readRecord<TestKeys>(conf);

    EXPECT_EQUAL(testKeys.key1.get(), "val1");
    EXPECT_EQUAL(testKeys.key2.get(), 2.0);
    EXPECT_EQUAL(testKeys.key3.get(), 3);
    EXPECT_EQUAL(testKeys.key4.get(), 4);
    EXPECT_EQUAL(testKeys.key5.get(), true);
    EXPECT_EQUAL(testKeys.key6.isSet(), false);
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

    auto testKeys = dm::readRecord<TestKeys>(conf);

    EXPECT_EQUAL(testKeys.key1.get(), "val1");
    EXPECT_EQUAL(testKeys.key2.get(), 2.0);
    EXPECT_EQUAL(testKeys.key3.get(), 3);
    EXPECT_EQUAL(testKeys.key4.isSet(), false);
    EXPECT_EQUAL(testKeys.key5.get(), true);
    EXPECT_EQUAL(testKeys.key6.isSet(), false);
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

    EXPECT_THROWS(dm::readRecord<TestKeys>(conf));
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

    EXPECT_THROWS(dm::readRecord<TestKeys>(conf));
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

    auto testKeys = dm::readRecord<TestKeys>(conf);

    EXPECT_EQUAL(testKeys.key1.get(), "val1");
    EXPECT_EQUAL(testKeys.key2.get(), 2.0);
    EXPECT_EQUAL(testKeys.key3.get(), 3);
    EXPECT_EQUAL(testKeys.key4.isSet(), false);
    EXPECT_EQUAL(testKeys.key5.get(), true);

    auto& k6 = testKeys.key6;
    EXPECT_EQUAL(k6.isSet(), true);
    EXPECT_EQUAL((k6.get()[0]), 1.0);
    EXPECT_EQUAL((k6.get()[1]), 2.0);
    EXPECT_EQUAL((k6.get()[2]), 3.0);
};


CASE("Test parse simple config with unexpected additional keys") {
    const std::string exampleJson(
        R"json({
            "key1": "val1",
            "key2": 2.0,
            "key3": 3,
            "key4": null,
            "key5": true,
            "too_much": "yes",
            "not_needed": "so true"
        })json");

    eckit::LocalConfiguration conf{eckit::YAMLConfiguration(exampleJson)};

    /// TODO(pgeier) With C++20 designators are more useful for inline creation of structs:
    /// ParsedOptions{.allowAdditionalKeys=false}
    datamod::ParseOptions opts;
    opts.allowAdditionalKeys = false;
    EXPECT_THROWS(dm::readRecord<TestKeys>(conf, opts));
};

}  // namespace multio::test

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

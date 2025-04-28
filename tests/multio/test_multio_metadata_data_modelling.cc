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
#include "multio/message/DataModelling.h"


enum class TestKeys : std::uint64_t
{
    Key1,
    Key2,
    Key3,
    Key4,
    Key5,
};

template <>
struct multio::message::KeySetDescription<TestKeys> {
    static constexpr std::string_view name = "test";

    static const auto& keys() {
        using namespace multio::message;
        static const auto keys = std::make_tuple(
            describeKeyValue<TestKeys::Key1, std::string>("key1"), describeKeyValue<TestKeys::Key2, double>("key2"),
            describeKeyValue<TestKeys::Key3, std::int64_t>("key3"),
            describeKeyValue<TestKeys::Key4, std::int64_t, KVTag::Optional>("key4"),
            describeKeyValue<TestKeys::Key5, bool>("key5"));
        return keys;
    }
};


enum class TestKeys2 : std::uint64_t
{
    Key1,
    Key2,
};

template <>
struct multio::message::KeySetDescription<TestKeys2> {
    static constexpr std::string_view name = "test2";

    static const auto& keys() {
        using namespace multio::message;
        static const auto keys = std::make_tuple(describeKeyValue<TestKeys2::Key1, std::string>("key1"),
                                                 describeKeyValue<TestKeys2::Key2, double>("key2"));
        return keys;
    }
};


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

// Index of the KeyValue variants used to compare
constexpr std::size_t VARIANT_VAL_INDEX = 1;
constexpr std::size_t VARIANT_REF_INDEX = 2;


CASE("Test reify single description") {
    using namespace multio::message;

    auto key1 = toMissingKeyValue(key<TestKeys::Key1>());
    EXPECT_THROWS(validate(key1));
    auto key2 = toMissingKeyValue(key<TestKeys::Key2>());
    EXPECT_THROWS(validate(key2));
    auto key3 = toMissingKeyValue(key<TestKeys::Key3>());
    EXPECT_THROWS(validate(key3));

    // Optional should not throw
    auto key4 = toMissingKeyValue(key<TestKeys::Key4>());
    EXPECT_NO_THROW(validate(key4));

    //
    auto key5 = toMissingKeyValue(key<TestKeys::Key5>());
    EXPECT_THROWS(validate(key5));
};

CASE("Test reify description") {
    using namespace multio::message;

    auto testKeys = reify(keySet<TestKeys>());
    EXPECT_THROWS(validate(testKeys));
    EXPECT_THROWS(validate(key<TestKeys::Key1>(testKeys)));
    EXPECT_THROWS(validate(key<TestKeys::Key2>(testKeys)));
    EXPECT_THROWS(validate(key<TestKeys::Key3>(testKeys)));
    EXPECT_NO_THROW(validate(key<TestKeys::Key4>(testKeys)));
    EXPECT_THROWS(validate(key<TestKeys::Key5>(testKeys)));
};


CASE("Test reify description from metadata [1]") {
    using namespace multio::message;

    // Create an object from an rvalue
    // Should capture all values
    auto testKeys
        = read(keySet<TestKeys>(), Metadata{{"key1", "val1"}, {"key2", 2.0}, {"key3", 3}, {"key4", 4}, {"key5", true}});
    EXPECT_NO_THROW(validate(testKeys));

    EXPECT_EQUAL(key<TestKeys::Key1>(testKeys).get(), "val1");
    EXPECT_EQUAL(key<TestKeys::Key2>(testKeys).get(), 2.0);
    EXPECT_EQUAL(key<TestKeys::Key3>(testKeys).get(), 3);
    EXPECT_EQUAL(key<TestKeys::Key4>(testKeys).get(), 4);
    EXPECT_EQUAL(key<TestKeys::Key5>(testKeys).get(), true);

    util::forEach(
        [&](auto& kv) {
            // Implementation detail: variant index 1 is value
            EXPECT_EQUAL(kv.value.index(), VARIANT_VAL_INDEX);
        },
        testKeys.values);
};

CASE("Test reify description from metadata [1,1]") {
    using namespace multio::message;

    Metadata md{{"key1", "val1"}, {"key2", 2.0}, {"key3", 3}, {"key4", 4}, {"key5", true}};

    // Create an object from an rvalue
    // Should reference all values
    auto testKeys = read(keySet<TestKeys>(), md);
    EXPECT_NO_THROW(validate(testKeys));

    EXPECT_EQUAL(key<TestKeys::Key1>(testKeys).get(), "val1");
    EXPECT_EQUAL(key<TestKeys::Key2>(testKeys).get(), 2.0);
    EXPECT_EQUAL(key<TestKeys::Key3>(testKeys).get(), 3);
    EXPECT_EQUAL(key<TestKeys::Key4>(testKeys).get(), 4);
    EXPECT_EQUAL(key<TestKeys::Key5>(testKeys).get(), true);

    util::forEach(
        [&](auto& kv) {
            // Implementation detail: variant index 2 is reference
            EXPECT_EQUAL(kv.value.index(), VARIANT_REF_INDEX);
        },
        testKeys.values);


    // Now acquire to test if references are gone
    acquire(testKeys);

    EXPECT_EQUAL(key<TestKeys::Key1>(testKeys).get(), "val1");
    EXPECT_EQUAL(key<TestKeys::Key2>(testKeys).get(), 2.0);
    EXPECT_EQUAL(key<TestKeys::Key3>(testKeys).get(), 3);
    EXPECT_EQUAL(key<TestKeys::Key4>(testKeys).get(), 4);
    EXPECT_EQUAL(key<TestKeys::Key5>(testKeys).get(), true);

    util::forEach(
        [&](auto& kv) {
            // Implementation detail: variant index 1 is value
            EXPECT_EQUAL(kv.value.index(), VARIANT_VAL_INDEX);
        },
        testKeys.values);
};


CASE("Test reify description from metadata [2]") {
    using namespace multio::message;

    auto testKeys = read(keySet<TestKeys>(), Metadata{{"key1", "val1"}, {"key2", 2.0}, {"key3", 3}, {"key5", true}});
    EXPECT_NO_THROW(validate(testKeys));

    EXPECT_EQUAL(key<TestKeys::Key1>(testKeys).get(), "val1");
    EXPECT_EQUAL(key<TestKeys::Key2>(testKeys).get(), 2.0);
    EXPECT_EQUAL(key<TestKeys::Key3>(testKeys).get(), 3);
    EXPECT_EQUAL(key<TestKeys::Key4>(testKeys).isMissing(), true);
    EXPECT_EQUAL(key<TestKeys::Key5>(testKeys).get(), true);
};


CASE("Test reify description from metadata [3]") {
    using namespace multio::message;

    // TBD think about changing behaviour that verification is ALWAYS a separate step
    // Can already throw here if required metadata is not contained
    EXPECT_THROWS(read(keySet<TestKeys>(), Metadata{{"key2", 2.0}, {"key3", 3}, {"key5", true}}));
    // auto testKeys = reify(keySet<TestKeys>(), Metadata{{"key2", 2.0}, {"key3", 3}, {"key5", true}});
    // EXPECT_THROWS(validate(testKeys));

    // EXPECT_EQUAL(key<TestKeys::Key1>(testKeys).isMissing(), true);
    // EXPECT_THROWS(key<TestKeys::Key1>(testKeys).get());
    // EXPECT_EQUAL(key<TestKeys::Key2>(testKeys).get(), 2.0);
    // EXPECT_EQUAL(key<TestKeys::Key3>(testKeys).get(), 3);
    // EXPECT_EQUAL(key<TestKeys::Key4>(testKeys).isMissing(), true);
    // EXPECT_EQUAL(key<TestKeys::Key5>(testKeys).get(), true);
};


CASE("Test reify description and write to metadata [1]") {
    using namespace multio::message;

    // Create an object from an rvalue
    // Should capture all values
    auto testKeys = reify(keySet<TestKeys>());
    EXPECT_THROWS(validate(testKeys));

    EXPECT_EQUAL(key<TestKeys::Key1>(testKeys).isMissing(), true);
    EXPECT_EQUAL(key<TestKeys::Key2>(testKeys).isMissing(), true);
    EXPECT_EQUAL(key<TestKeys::Key3>(testKeys).isMissing(), true);
    EXPECT_EQUAL(key<TestKeys::Key4>(testKeys).isMissing(), true);
    EXPECT_EQUAL(key<TestKeys::Key5>(testKeys).isMissing(), true);

    key<TestKeys::Key1>(testKeys).set("val1");
    key<TestKeys::Key2>(testKeys).set(2.0);
    key<TestKeys::Key3>(testKeys).set(3);
    key<TestKeys::Key4>(testKeys).set(4);
    key<TestKeys::Key5>(testKeys).set(true);

    EXPECT_EQUAL(key<TestKeys::Key1>(testKeys).get(), "val1");
    EXPECT_EQUAL(key<TestKeys::Key2>(testKeys).get(), 2.0);
    EXPECT_EQUAL(key<TestKeys::Key3>(testKeys).get(), 3);
    EXPECT_EQUAL(key<TestKeys::Key4>(testKeys).get(), 4);
    EXPECT_EQUAL(key<TestKeys::Key5>(testKeys).get(), true);

    util::forEach(
        [&](auto& kv) {
            // Implementation detail: variant index 1 is value
            EXPECT_EQUAL(kv.value.index(), VARIANT_VAL_INDEX);
        },
        testKeys.values);

    {
        Metadata md{};
        write(testKeys, md);
        EXPECT_EQUAL(md.get<std::string>("key1"), "val1");
        EXPECT_EQUAL(md.get<double>("key2"), 2.0);
        EXPECT_EQUAL(md.get<std::int64_t>("key3"), 3);
        EXPECT_EQUAL(md.get<std::int64_t>("key4"), 4);
        EXPECT_EQUAL(md.get<bool>("key5"), true);
    }

    {
        auto md = write<Metadata>(testKeys);
        EXPECT_EQUAL(md.get<std::string>("key1"), "val1");
        EXPECT_EQUAL(md.get<double>("key2"), 2.0);
        EXPECT_EQUAL(md.get<std::int64_t>("key3"), 3);
        EXPECT_EQUAL(md.get<std::int64_t>("key4"), 4);
        EXPECT_EQUAL(md.get<bool>("key5"), true);
    }

    {
        testKeys.keySet.scoped();
        auto md = write<Metadata>(testKeys);
        EXPECT_EQUAL(md.get<std::string>("test-key1"), "val1");
        EXPECT_EQUAL(md.get<double>("test-key2"), 2.0);
        EXPECT_EQUAL(md.get<std::int64_t>("test-key3"), 3);
        EXPECT_EQUAL(md.get<std::int64_t>("test-key4"), 4);
        EXPECT_EQUAL(md.get<bool>("test-key5"), true);
    }

    {
        testKeys.keySet.scoped("myscope");
        auto md = write<Metadata>(testKeys);
        EXPECT_EQUAL(md.get<std::string>("myscope-key1"), "val1");
        EXPECT_EQUAL(md.get<double>("myscope-key2"), 2.0);
        EXPECT_EQUAL(md.get<std::int64_t>("myscope-key3"), 3);
        EXPECT_EQUAL(md.get<std::int64_t>("myscope-key4"), 4);
        EXPECT_EQUAL(md.get<bool>("myscope-key5"), true);
    }
};


CASE("Test reify subset description [1]") {
    using namespace multio::message;

    auto subKeySet = std::make_tuple(key<TestKeys::Key1>(), key<TestKeys::Key4>());

    auto subKeys = read(subKeySet, Metadata{{"key1", "val1"}});

    EXPECT_NO_THROW(validate(subKeys));

    EXPECT_EQUAL(key<TestKeys::Key1>(subKeys).isMissing(), false);
    EXPECT_EQUAL(key<TestKeys::Key1>(subKeys).get(), "val1");
    EXPECT_EQUAL(key<TestKeys::Key1>(subKeys).value.index(), VARIANT_VAL_INDEX);
    EXPECT_EQUAL(key<TestKeys::Key4>(subKeys).isMissing(), true);

    key<TestKeys::Key4>(subKeys).set(4);
    EXPECT_EQUAL(key<TestKeys::Key4>(subKeys).get(), 4);

    {
        auto md = write<Metadata>(subKeys);
        EXPECT_EQUAL(md.get<std::string>("key1"), "val1");
        EXPECT_EQUAL(md.get<std::int64_t>("key4"), 4);
    }
};


CASE("Test reify subset description from keyvalues tuple [1]") {
    using namespace multio::message;

    // Create an object from an rvalue
    // Should capture all values
    auto testKeys
        = read(keySet<TestKeys>(), Metadata{{"key1", "val1"}, {"key2", 2.0}, {"key3", 3}, {"key4", 4}, {"key5", true}});
    EXPECT_NO_THROW(validate(testKeys));

    util::forEach(
        [&](auto& kv) {
            // Implementation detail: variant index 1 is value
            EXPECT_EQUAL(kv.value.index(), VARIANT_VAL_INDEX);
        },
        testKeys.values);

    auto subKeySet = std::make_tuple(key<TestKeys::Key1>(), key<TestKeys::Key4>());
    auto subKeys = read(subKeySet, testKeys);

    EXPECT_EQUAL(key<TestKeys::Key1>(subKeys).get(), "val1");
    EXPECT_EQUAL(key<TestKeys::Key4>(subKeys).get(), 4);

    util::forEach(
        [&](auto& kv) {
            // Implementation detail: variant index 1 is value
            EXPECT_EQUAL(kv.value.index(), VARIANT_VAL_INDEX);
        },
        subKeys);
};

CASE("Test reify subset description from keyvalues tuple [1,1]") {
    using namespace multio::message;

    Metadata md{{"key1", "val1"}, {"key2", 2.0}, {"key3", 3}, {"key4", 4}, {"key5", true}};

    // Create an object from an rvalue
    // Should reference all values
    auto testKeys = read(keySet<TestKeys>(), md);
    EXPECT_NO_THROW(validate(testKeys));

    util::forEach(
        [&](auto& kv) {
            // Implementation detail: variant index 2 is reference
            EXPECT_EQUAL(kv.value.index(), VARIANT_REF_INDEX);
        },
        testKeys.values);

    auto subKeySet = std::make_tuple(key<TestKeys::Key1>(), key<TestKeys::Key4>());
    auto subKeys = read(subKeySet, testKeys);

    EXPECT_EQUAL(key<TestKeys::Key1>(subKeys).get(), "val1");
    EXPECT_EQUAL(key<TestKeys::Key4>(subKeys).get(), 4);

    util::forEach(
        [&](auto& kv) {
            // Implementation detail: variant index 1 is value
            EXPECT_EQUAL(kv.value.index(), VARIANT_REF_INDEX);
        },
        subKeys);
};


CASE("Test reify custom scoped description from metadata [1]") {
    using namespace multio::message;

    auto testKeys = read(keySet<TestKeys::Key1, TestKeys2::Key1>().scoped(),
                         Metadata{{"test-key1", "val1"}, {"test2-key1", "val2"}});
    EXPECT_NO_THROW(validate(testKeys));

    EXPECT_EQUAL(key<TestKeys::Key1>(testKeys).get(), "val1");
    EXPECT_EQUAL(key<TestKeys2::Key1>(testKeys).get(), "val2");
};


CASE("Test reify custom scoped description from metadata [2]") {
    using namespace multio::message;

    auto testKeys = read(keySet<TestKeys::Key1, TestKeys2::Key1>().makeScoped(),
                         Metadata{{"test-key1", "val1"}, {"test2-key1", "val2"}});
    EXPECT_NO_THROW(validate(testKeys));

    EXPECT_EQUAL(key<TestKeys::Key1>(testKeys).get(), "val1");
    EXPECT_EQUAL(key<TestKeys2::Key1>(testKeys).get(), "val2");
};


// CASE("Test reify custom scoped description from existing KeyValueSet [1]") {
//     using namespace multio::message;

//     auto alltestKeys = read(keySet<TestKeys>(), Metadata{{"key1", "val1"}, {"key2", 2.0}, {"key3", 3}, {"key4", 4},
//     {"key5", true}});

//     auto testKeys = read(keySet<TestKeys::Key1, TestKeys2::Key1>().scoped(), Metadata{{"test-key1", "val1"},
//     {"test2-key1", "val2"}}); EXPECT_NO_THROW(validate(testKeys));

//     EXPECT_EQUAL(key<TestKeys::Key1>(testKeys).get(), "val1");
//     EXPECT_EQUAL(key<TestKeys2::Key1>(testKeys).get(), "val2");
// };


CASE("Test hashing a keyset") {
    using namespace multio::message;

    auto testKeys = read(keySet<TestKeys::Key1, TestKeys2::Key1>().makeScoped(),
                         Metadata{{"test-key1", "val1"}, {"test2-key1", "val2"}});

    EXPECT(util::hash(testKeys) != 0);
};

// TODO Extend and test read/write with differente scope

// TODO rename reify to read for KeyValues??? allow reading a bigger KeyValueSet from a subset

// TODO Extend and test custom validation/init


}  // namespace multio::test

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

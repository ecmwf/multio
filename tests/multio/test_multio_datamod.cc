/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "eckit/testing/Test.h"
#include "multio/datamod/ContainerInterop.h"
#include "multio/datamod/core/EntryDef.h"
#include "multio/datamod/core/Hash.h"
#include "multio/datamod/core/Record.h"

namespace multio::test {

namespace dm = multio::datamod;

// TODO test ScopedEntryRef properly (validation, applyDefaults...)

constexpr auto KEY1 =                  //
    dm::EntryDef<std::string>{"key1"}  //
        .withAccessor([](auto&& v) { return &v.key1; });
constexpr auto KEY2 =             //
    dm::EntryDef<double>{"key2"}  //
        .withAccessor([](auto&& v) { return &v.key2; });
constexpr auto KEY3 =                   //
    dm::EntryDef<std::int64_t>{"key3"}  //
        .withAccessor([](auto&& v) { return &v.key3; });
constexpr auto KEY4 =  //
    dm::EntryDef<std::int64_t>{"key4"}
        .tagOptional()
        .withDescription(
            "Key4 describes the "
            "answer to the ultimate question of life, the universe, and everything. "
            "It is optional, because we assume that you can not provide it. If you could, "
            "all this code would not be need and hence you would not read this, "
            "right?")  //
        .withAccessor([](auto&& v) { return &v.key4; });
constexpr auto KEY5 =           //
    dm::EntryDef<bool>{"key5"}  //
        .withAccessor([](auto&& v) { return &v.key5; });

struct TestKeys {
    dm::EntryType_t<decltype(KEY1)> key1;
    dm::EntryType_t<decltype(KEY2)> key2;
    dm::EntryType_t<decltype(KEY3)> key3;
    dm::EntryType_t<decltype(KEY4)> key4;
    dm::EntryType_t<decltype(KEY5)> key5;

    static constexpr std::string_view record_name_ = "test";
    static constexpr auto record_entries_ = std::make_tuple(KEY1, KEY2, KEY3, KEY4, KEY5);
};

}  // namespace multio::test

namespace std {
template <>
struct hash<multio::test::TestKeys> : multio::datamod::HashRecord {};
}  // namespace std

namespace multio::test {

struct SubKeys {
    dm::EntryType_t<decltype(KEY1)> key1;
    dm::EntryType_t<decltype(KEY4)> key4;

    static constexpr std::string_view record_name_ = "subtest";
    static constexpr auto record_entries_ = std::make_tuple(KEY1, KEY4);
};


constexpr auto TKEY1 =                 //
    dm::EntryDef<std::string>{"key1"}  //
        .withAccessor([](auto&& v) { return &v.key1; });
constexpr auto TKEY2 =            //
    dm::EntryDef<double>{"key2"}  //
        .withAccessor([](auto&& v) { return &v.key2; });


struct TestKeys2 {
    dm::EntryType_t<decltype(TKEY1)> key1;
    dm::EntryType_t<decltype(TKEY2)> key2;

    static constexpr std::string_view record_name_ = "test";
    static constexpr auto record_entries_ = std::make_tuple(TKEY1, TKEY2);
};


// Explicit optional testing

constexpr auto REQKEY =                   //
    dm::EntryDef<std::string>{"req-key"}  //
        .withAccessor([](auto&& v) { return &v.reqKey; });
constexpr auto DEFKEY1 =  //
    dm::EntryDef<std::string>{"def-key1"}
        .withDefault("default-key1-value")  //
        .withAccessor([](auto&& v) { return &v.defKey1; });
constexpr auto DEFKEY2 =  //
    dm::EntryDef<std::string>{"def-key2"}
        .tagDefaulted()  //
        .withAccessor([](auto&& v) { return &v.defKey2; });
constexpr auto OPTKEY =  //
    dm::EntryDef<std::string>{"opt-key"}
        .tagOptional()  //
        .withAccessor([](auto&& v) { return &v.optKey; });

struct TestOpts {
    dm::EntryType_t<decltype(REQKEY)> reqKey;
    dm::EntryType_t<decltype(DEFKEY1)> defKey1;
    dm::EntryType_t<decltype(DEFKEY2)> defKey2;
    dm::EntryType_t<decltype(OPTKEY)> optKey;

    static constexpr std::string_view record_name_ = "test-opts";
    static constexpr auto record_entries_ = std::make_tuple(REQKEY, DEFKEY1, DEFKEY2, OPTKEY);

    static void applyDefaults(test::TestOpts& opts) {
        // Conditionally set default of def-key2 to value of def-key1
        opts.defKey2.ensureInit([&]() { return opts.defKey1.get(); });
    }
};


using namespace multio::datamod;
using multio::message::Metadata;

// Index of the KeyValue variants used to compare
constexpr std::size_t VARIANT_VAL_INDEX = 1;
constexpr std::size_t VARIANT_REF_INDEX = 2;

CASE("Test static asserts") {
    // Test some static asserts
    using namespace datamod;
    static_assert(HasParse_v<TypeParser<std::string, DefaultMapper>, std::string>);
    static_assert(HasParse_v<TypeParser<std::string, DefaultMapper>, std::string&>);
    static_assert(HasParse_v<TypeParser<std::string, DefaultMapper>, const std::string&>);
    static_assert(HasParse_v<TypeParser<std::string, DefaultMapper>, std::string&&>);

    static_assert(HasParse_v<TypeParser<bool, DefaultMapper>, bool>);
    static_assert(HasParse_v<TypeParser<bool, DefaultMapper>, bool&>);
    static_assert(HasParse_v<TypeParser<bool, DefaultMapper>, const bool&>);
    static_assert(HasParse_v<TypeParser<bool, DefaultMapper>, bool&&>);

    static_assert(HasParse_v<TypeParser<double, DefaultMapper>, double>);
    static_assert(HasParse_v<TypeParser<double, DefaultMapper>, double&>);
    static_assert(HasParse_v<TypeParser<double, DefaultMapper>, const double&>);
    static_assert(HasParse_v<TypeParser<double, DefaultMapper>, double&&>);

    static_assert(HasParse_v<TypeParser<std::int64_t, DefaultMapper>, std::int64_t>);
    static_assert(HasParse_v<TypeParser<std::int64_t, DefaultMapper>, std::int64_t&>);
    static_assert(HasParse_v<TypeParser<std::int64_t, DefaultMapper>, std::int64_t&>);
    static_assert(HasParse_v<TypeParser<std::int64_t, DefaultMapper>, std::int64_t&&>);
};


CASE("Test make single entry") {
    using namespace multio::datamod;

    auto key1 = KEY1.makeEntry();
    EXPECT_THROWS(KEY1.validate(key1));
    auto key2 = KEY2.makeEntry();
    EXPECT_THROWS(KEY2.validate(key2));
    auto key3 = KEY3.makeEntry();
    EXPECT_THROWS(KEY3.validate(key3));
    auto key4 = KEY4.makeEntry();
    EXPECT_NO_THROW(KEY4.validate(key4));
    auto key5 = KEY5.makeEntry();
    EXPECT_THROWS(KEY5.validate(key5));
};

CASE("Test make ercord") {
    using namespace multio::datamod;

    TestKeys testKeys;
    EXPECT_THROWS(validateRecord(testKeys));
    EXPECT_THROWS(KEY1.validate(testKeys.key1));
    EXPECT_THROWS(KEY2.validate(testKeys.key2));
    EXPECT_THROWS(KEY3.validate(testKeys.key3));
    EXPECT_NO_THROW(KEY4.validate(testKeys.key4));
    EXPECT_THROWS(KEY5.validate(testKeys.key5));
};


CASE("Test parse record from metadata [1]") {
    using namespace multio::datamod;

    // Create an object from an rvalue
    // Should capture all values
    auto testKeys
        = readRecord<TestKeys>(Metadata{{"key1", "val1"}, {"key2", 2.0}, {"key3", 3}, {"key4", 4}, {"key5", true}});
    EXPECT_NO_THROW(validateRecord(testKeys));

    EXPECT_EQUAL(testKeys.key1.get(), "val1");
    EXPECT_EQUAL(testKeys.key2.get(), 2.0);
    EXPECT_EQUAL(testKeys.key3.get(), 3);
    EXPECT_EQUAL(testKeys.key4.get(), 4);
    EXPECT_EQUAL(testKeys.key5.get(), true);

    util::forEach(
        [&](const auto& entryDef) {
            // Implementation detail: variant index 1 is value
            EXPECT_EQUAL(entryDef.get(testKeys).value.index(), VARIANT_VAL_INDEX);
        },
        recordEntries(testKeys));


    auto testKeysCpy = testKeys;
    util::forEach(
        [&](const auto& entryDef) {
            // Implementation detail: variant index 1 is value
            EXPECT_EQUAL(entryDef.get(testKeys).value.index(), VARIANT_VAL_INDEX);
        },
        recordEntries(testKeysCpy));
};

CASE("Test parse record from metadata [1,1]") {
    using namespace multio::datamod;

    Metadata md{{"key1", "val1"}, {"key2", 2.0}, {"key3", 3}, {"key4", 4}, {"key5", true}};

    // Create an object from an rvalue
    // Should reference all values
    auto testKeys = readRecord<TestKeys>(md);
    EXPECT_NO_THROW(validateRecord(testKeys));

    EXPECT_EQUAL(testKeys.key1.get(), "val1");
    EXPECT_EQUAL(testKeys.key2.get(), 2.0);
    EXPECT_EQUAL(testKeys.key3.get(), 3);
    EXPECT_EQUAL(testKeys.key4.get(), 4);
    EXPECT_EQUAL(testKeys.key5.get(), true);

    util::forEach(
        [&](const auto& entryDef) {
            // Implementation detail: variant index 2 is reference
            EXPECT_EQUAL(entryDef.get(testKeys).value.index(), VARIANT_REF_INDEX);
        },
        recordEntries(testKeys));


    // Now acquire to test if references are gone
    acquireRecord(testKeys);

    EXPECT_EQUAL(testKeys.key1.get(), "val1");
    EXPECT_EQUAL(testKeys.key2.get(), 2.0);
    EXPECT_EQUAL(testKeys.key3.get(), 3);
    EXPECT_EQUAL(testKeys.key4.get(), 4);
    EXPECT_EQUAL(testKeys.key5.get(), true);

    util::forEach(
        [&](const auto& entryDef) {
            // Implementation detail: variant index 2 is reference
            EXPECT_EQUAL(entryDef.get(testKeys).value.index(), VARIANT_VAL_INDEX);
        },
        recordEntries(testKeys));
};


CASE("Test parse record from metadata [2]") {
    using namespace multio::datamod;

    auto testKeys = readRecord<TestKeys>(Metadata{{"key1", "val1"}, {"key2", 2.0}, {"key3", 3}, {"key5", true}});
    EXPECT_NO_THROW(validateRecord(testKeys));

    EXPECT_EQUAL(testKeys.key1.get(), "val1");
    EXPECT_EQUAL(testKeys.key2.get(), 2.0);
    EXPECT_EQUAL(testKeys.key3.get(), 3);
    EXPECT_EQUAL(testKeys.key4.isSet(), false);
    EXPECT_EQUAL(testKeys.key5.get(), true);
};

CASE("Test parse record from metadata [2, scoped]") {
    using namespace multio::datamod;

    auto scopedKeys = scopeRecord(TestKeys{});
    readRecord(scopedKeys, Metadata{{"test-key1", "val1"}, {"test-key2", 2.0}, {"test-key3", 3}, {"test-key5", true}});
    auto testKeys = unscopeRecord(std::move(scopedKeys));
    EXPECT_NO_THROW(validateRecord(testKeys));

    EXPECT_EQUAL(testKeys.key1.get(), "val1");
    EXPECT_EQUAL(testKeys.key2.get(), 2.0);
    EXPECT_EQUAL(testKeys.key3.get(), 3);
    EXPECT_EQUAL(testKeys.key4.isSet(), false);
    EXPECT_EQUAL(testKeys.key5.get(), true);
};


CASE("Test parse record from metadata [3]") {
    using namespace multio::datamod;

    EXPECT_THROWS(readRecord<TestKeys>(Metadata{{"key2", 2.0}, {"key3", 3}, {"key5", true}}));
};


CASE("Test parse record and dump to metadata [1]") {
    using namespace multio::datamod;

    // Create an object from an rvalue
    // Should capture all values
    TestKeys testKeys;
    EXPECT_THROWS(validateRecord(testKeys));

    EXPECT_EQUAL(testKeys.key1.isSet(), false);
    EXPECT_EQUAL(testKeys.key2.isSet(), false);
    EXPECT_EQUAL(testKeys.key3.isSet(), false);
    EXPECT_EQUAL(testKeys.key4.isSet(), false);
    EXPECT_EQUAL(testKeys.key5.isSet(), false);

    testKeys.key1.set("val1");
    testKeys.key2.set(2.0);
    testKeys.key3.set(3);
    testKeys.key4.set(4);
    testKeys.key5.set(true);

    EXPECT_EQUAL(testKeys.key1.get(), "val1");
    EXPECT_EQUAL(testKeys.key2.get(), 2.0);
    EXPECT_EQUAL(testKeys.key3.get(), 3);
    EXPECT_EQUAL(testKeys.key4.get(), 4);
    EXPECT_EQUAL(testKeys.key5.get(), true);

    util::forEach(
        [&](const auto& entryDef) {
            // Implementation detail: variant index 2 is reference
            EXPECT_EQUAL(entryDef.get(testKeys).value.index(), VARIANT_VAL_INDEX);
        },
        recordEntries(testKeys));

    {
        Metadata md{};
        dumpRecord(testKeys, md);
        EXPECT_EQUAL(md.get<std::string>("key1"), "val1");
        EXPECT_EQUAL(md.get<double>("key2"), 2.0);
        EXPECT_EQUAL(md.get<std::int64_t>("key3"), 3);
        EXPECT_EQUAL(md.get<std::int64_t>("key4"), 4);
        EXPECT_EQUAL(md.get<bool>("key5"), true);
    }

    {
        auto md = dumpRecord<Metadata>(testKeys);
        EXPECT_EQUAL(md.get<std::string>("key1"), "val1");
        EXPECT_EQUAL(md.get<double>("key2"), 2.0);
        EXPECT_EQUAL(md.get<std::int64_t>("key3"), 3);
        EXPECT_EQUAL(md.get<std::int64_t>("key4"), 4);
        EXPECT_EQUAL(md.get<bool>("key5"), true);
    }

    {
        auto scopedTestKeys = dm::scopeRecord(testKeys);
        auto md = dumpRecord<Metadata>(scopedTestKeys);
        EXPECT_EQUAL(md.get<std::string>("test-key1"), "val1");
        EXPECT_EQUAL(md.get<double>("test-key2"), 2.0);
        EXPECT_EQUAL(md.get<std::int64_t>("test-key3"), 3);
        EXPECT_EQUAL(md.get<std::int64_t>("test-key4"), 4);
        EXPECT_EQUAL(md.get<bool>("test-key5"), true);
    }

    {
        auto scopedTestKeys = dm::scopeRecord(testKeys, "myscope");
        auto md = dumpRecord<Metadata>(scopedTestKeys);
        EXPECT_EQUAL(md.get<std::string>("myscope-key1"), "val1");
        EXPECT_EQUAL(md.get<double>("myscope-key2"), 2.0);
        EXPECT_EQUAL(md.get<std::int64_t>("myscope-key3"), 3);
        EXPECT_EQUAL(md.get<std::int64_t>("myscope-key4"), 4);
        EXPECT_EQUAL(md.get<bool>("myscope-key5"), true);
    }
};


// This is trivial
CASE("Test parse subset record[1]") {
    using namespace multio::datamod;


    auto subKeys = readRecord<SubKeys>(Metadata{{"key1", "val1"}});

    EXPECT_NO_THROW(validateRecord(subKeys));

    EXPECT_EQUAL(subKeys.key1.isSet(), true);
    EXPECT_EQUAL(subKeys.key1.get(), "val1");
    EXPECT_EQUAL(subKeys.key1.value.index(), VARIANT_VAL_INDEX);
    EXPECT_EQUAL(subKeys.key4.isSet(), false);

    subKeys.key4.set(4);
    EXPECT_EQUAL(subKeys.key4.get(), 4);

    {
        auto md = dumpRecord<Metadata>(subKeys);
        EXPECT_EQUAL(md.get<std::string>("key1"), "val1");
        EXPECT_EQUAL(md.get<std::int64_t>("key4"), 4);
    }
};


CASE("Test parse subset record from record [1]") {
    using namespace multio::datamod;

    // Create an object from an rvalue
    // Should capture all values
    auto testKeys
        = readRecord<TestKeys>(Metadata{{"key1", "val1"}, {"key2", 2.0}, {"key3", 3}, {"key4", 4}, {"key5", true}});
    EXPECT_NO_THROW(validateRecord(testKeys));

    util::forEach(
        [&](const auto& entryDef) {
            // Implementation detail: variant index 2 is reference
            EXPECT_EQUAL(entryDef.get(testKeys).value.index(), VARIANT_VAL_INDEX);
        },
        recordEntries(testKeys));


    auto subKeys = readRecord<SubKeys>(testKeys);

    EXPECT_EQUAL(subKeys.key1.get(), "val1");
    EXPECT_EQUAL(subKeys.key4.get(), 4);

    util::forEach(
        [&](const auto& entryDef) {
            // Implementation detail: variant index 2 is reference
            EXPECT_EQUAL(entryDef.get(testKeys).value.index(), VARIANT_VAL_INDEX);
        },
        recordEntries(subKeys));
};

CASE("Test parse subset record from keyvalues tuple [1,1]") {
    using namespace multio::datamod;

    Metadata md{{"key1", "val1"}, {"key2", 2.0}, {"key3", 3}, {"key4", 4}, {"key5", true}};

    // Create an object from an rvalue
    // Should reference all values
    auto testKeys = readRecord<TestKeys>(md);
    EXPECT_NO_THROW(validateRecord(testKeys));

    util::forEach(
        [&](const auto& entryDef) {
            // Implementation detail: variant index 2 is reference
            EXPECT_EQUAL(entryDef.get(testKeys).value.index(), VARIANT_REF_INDEX);
        },
        recordEntries(testKeys));

    auto subKeys = readRecord<SubKeys>(testKeys);

    EXPECT_EQUAL(subKeys.key1.get(), "val1");
    EXPECT_EQUAL(subKeys.key4.get(), 4);

    util::forEach(
        [&](const auto& entryDef) {
            // Implementation detail: variant index 2 is reference
            EXPECT_EQUAL(entryDef.get(testKeys).value.index(), VARIANT_REF_INDEX);
        },
        recordEntries(subKeys));
};


CASE("Test setting defaults") {
    using namespace multio::datamod;
    EXPECT_THROWS(readRecord<TestOpts>(Metadata{}));
    EXPECT_NO_THROW(readRecord<TestOpts>(Metadata{{"req-key", "val"}}));

    {
        auto opts = readRecord<TestOpts>(Metadata{{"req-key", "val"}});
        EXPECT_EQUAL(opts.defKey1.isSet(), true);
        EXPECT_EQUAL(opts.defKey1.get(), "default-key1-value");
        EXPECT_EQUAL(opts.defKey2.isSet(), true);
        EXPECT_EQUAL(opts.defKey2.get(), "default-key1-value");
    }

    {
        auto opts = readRecord<TestOpts>(Metadata{{"req-key", "val"}, {"def-key1", "val1"}});
        EXPECT_EQUAL(opts.defKey1.isSet(), true);
        EXPECT_EQUAL(opts.defKey1.get(), "val1");
        EXPECT_EQUAL(opts.defKey2.isSet(), true);
        EXPECT_EQUAL(opts.defKey2.get(), "val1");
    }

    {
        auto opts = readRecord<TestOpts>(Metadata{{"req-key", "val"}, {"def-key2", "val2"}});
        EXPECT_EQUAL(opts.defKey1.isSet(), true);
        EXPECT_EQUAL(opts.defKey1.get(), "default-key1-value");
        EXPECT_EQUAL(opts.defKey2.isSet(), true);
        EXPECT_EQUAL(opts.defKey2.get(), "val2");
    }
}


CASE("Test hashing a keyset") {
    using namespace multio::datamod;
    auto testKeys
        = readRecord<TestKeys>(Metadata{{"key1", "val1"}, {"key2", 2.0}, {"key3", 3}, {"key4", 4}, {"key5", true}});

    EXPECT(util::hash(testKeys) != 0);
};


}  // namespace multio::test

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

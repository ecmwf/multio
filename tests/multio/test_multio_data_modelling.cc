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
#include "multio/datamod/ContainerInterop.h"
#include "multio/datamod/DataModelling.h"
#include "multio/datamod/ReaderWriter.h"


enum class TestKeys : std::uint64_t
{
    Key1,
    Key2,
    Key3,
    Key4,
    Key5,
};

namespace multio::datamod {
MULTIO_KEY_SET_DESCRIPTION(TestKeys,                                      //
                           "test",                                        //
                                                                          //
                           KeyDef<TestKeys::Key1, std::string>{"key1"},   //
                           KeyDef<TestKeys::Key2, double>{"key2"},        //
                           KeyDef<TestKeys::Key3, std::int64_t>{"key3"},  //
                           KeyDef<TestKeys::Key4, std::int64_t>{"key4"}.tagOptional().withDescription(
                               "Key4 describes the "
                               "answer to the ultimate question of life, the universe, and everything. "
                               "It is optional, because we assume that you can not provide it. If you could, "
                               "all this code would not be need and hence you would not read this, "
                               "right?"),                          //
                           KeyDef<TestKeys::Key5, bool>{"key5"});  //
}


// Defined without macro
enum class TestKeys2 : std::uint64_t
{
    Key1,
    Key2,
};

template <>
struct multio::datamod::KeySetDefinition<TestKeys2> {
    static constexpr std::string_view name = "test2";

    static constexpr auto keyDefs
        = std::make_tuple(KeyDef<TestKeys2::Key1, std::string>{"key1"}, KeyDef<TestKeys2::Key2, double>{"key2"});
};


// Explicit optional testing

enum class TestOpts : std::uint64_t
{
    ReqKey,
    DefKey1,
    DefKey2,
    OptKey,
};

namespace multio::datamod {
MULTIO_KEY_SET_DESCRIPTION(TestOpts,                                                                              //
                           "test-opts",                                                                           //
                                                                                                                  //
                           KeyDef<TestOpts::ReqKey, std::string>{"req-key"},                                      //
                           KeyDef<TestOpts::DefKey1, std::string>{"def-key1"}.withDefault("default-key1-value"),  //
                           KeyDef<TestOpts::DefKey2, std::string>{"def-key2"}.tagDefaulted(),                     //
                           KeyDef<TestOpts::OptKey, std::string>{"opt-key"}.tagOptional());


template <>
struct KeySetAlter<KeySet<TestOpts>> {
    static void alter(KeyValueSet<KeySet<TestOpts>>& opts) {
        // Conditionally set default of def-key2 to value of def-key1
        auto k2 = key<TestOpts::DefKey1>(opts);
        key<TestOpts::DefKey2>(opts).withDefault([&]() { return k2.get(); });
    }
};

}  // namespace multio::datamod


namespace multio::test {
// Test constexpr
using namespace multio::datamod;
constexpr auto reqKey = KeyDef<TestOpts::ReqKey, std::string>{"req-key"};
constexpr auto reqKeyWithDescr = KeyDef<TestOpts::ReqKey, std::string>{"req-key"}.withDescription(
    "A key that has been just introduced to show how it is described with this text.");
constexpr auto defKey1 = KeyDef<TestOpts::DefKey1, std::string>{"def-key1"}.withDefault("default-key1-value");
constexpr auto defKey1WithDescr = KeyDef<TestOpts::DefKey1, std::string>{"def-key1"}
                                      .withDefault("default-key1-value")
                                      .withDescription(
                                          "long "
                                          "multiline"
                                          "description");

constexpr auto keys = std::make_tuple(KeyDef<TestOpts::ReqKey, std::string>{"req-key"},
                                      KeyDef<TestOpts::DefKey1, std::string>{"def-key1"}
                                          .withDefault("default-key1-value")
                                          .withDescription("long "
                                                           "multiline"
                                                           "description"));

// static constexpr KeyDef<TestOpts::ReqKey, std::string, KVTag::Required> reqKey{"req-key"};
// constexpr KeyDef<TestOpts::DefKey1, std::string, KVTag::Defaulted> defKey1
// {"def-key1"}.withDefault("default-key1-value");
}  // namespace multio::test


namespace multio::test {

using multio::message::Metadata;

// Index of the KeyValue variants used to compare
constexpr std::size_t VARIANT_VAL_INDEX = 1;
constexpr std::size_t VARIANT_REF_INDEX = 2;

CASE("Test static asserts") {
    // Test some static asserts
    using namespace datamod;
    static_assert(HasRead_v<Reader<std::string, DefaultMapper>, std::string>);
    static_assert(HasRead_v<Reader<std::string, DefaultMapper>, std::string&>);
    static_assert(HasRead_v<Reader<std::string, DefaultMapper>, const std::string&>);
    static_assert(HasRead_v<Reader<std::string, DefaultMapper>, std::string&&>);

    static_assert(HasRead_v<Reader<bool, DefaultMapper>, bool>);
    static_assert(HasRead_v<Reader<bool, DefaultMapper>, bool&>);
    static_assert(HasRead_v<Reader<bool, DefaultMapper>, const bool&>);
    static_assert(HasRead_v<Reader<bool, DefaultMapper>, bool&&>);

    static_assert(HasRead_v<Reader<double, DefaultMapper>, double>);
    static_assert(HasRead_v<Reader<double, DefaultMapper>, double&>);
    static_assert(HasRead_v<Reader<double, DefaultMapper>, const double&>);
    static_assert(HasRead_v<Reader<double, DefaultMapper>, double&&>);

    static_assert(HasRead_v<Reader<std::int64_t, DefaultMapper>, std::int64_t>);
    static_assert(HasRead_v<Reader<std::int64_t, DefaultMapper>, std::int64_t&>);
    static_assert(HasRead_v<Reader<std::int64_t, DefaultMapper>, std::int64_t&>);
    static_assert(HasRead_v<Reader<std::int64_t, DefaultMapper>, std::int64_t&&>);
};


CASE("Test reify single description") {
    using namespace multio::datamod;

    KeyValue<TestKeys::Key1> key1{};
    EXPECT_THROWS(validate(key1));
    KeyValue<TestKeys::Key2> key2{};
    EXPECT_THROWS(validate(key2));
    KeyValue<TestKeys::Key3> key3{};
    EXPECT_THROWS(validate(key3));

    // Optional should not throw
    KeyValue<TestKeys::Key4> key4{};
    EXPECT_NO_THROW(validate(key4));

    KeyValue<TestKeys::Key5> key5{};
    EXPECT_THROWS(validate(key5));
};

CASE("Test reify description") {
    using namespace multio::datamod;

    auto testKeys = reify(keySet<TestKeys>());
    EXPECT_THROWS(validate(testKeys));
    EXPECT_THROWS(validate(key<TestKeys::Key1>(testKeys)));
    EXPECT_THROWS(validate(key<TestKeys::Key2>(testKeys)));
    EXPECT_THROWS(validate(key<TestKeys::Key3>(testKeys)));
    EXPECT_NO_THROW(validate(key<TestKeys::Key4>(testKeys)));
    EXPECT_THROWS(validate(key<TestKeys::Key5>(testKeys)));
};


CASE("Test reify description from metadata [1]") {
    using namespace multio::datamod;

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
    using namespace multio::datamod;

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
    using namespace multio::datamod;

    auto testKeys = read(keySet<TestKeys>(), Metadata{{"key1", "val1"}, {"key2", 2.0}, {"key3", 3}, {"key5", true}});
    EXPECT_NO_THROW(validate(testKeys));

    EXPECT_EQUAL(key<TestKeys::Key1>(testKeys).get(), "val1");
    EXPECT_EQUAL(key<TestKeys::Key2>(testKeys).get(), 2.0);
    EXPECT_EQUAL(key<TestKeys::Key3>(testKeys).get(), 3);
    EXPECT_EQUAL(key<TestKeys::Key4>(testKeys).isMissing(), true);
    EXPECT_EQUAL(key<TestKeys::Key5>(testKeys).get(), true);
};


CASE("Test reify description from metadata [3]") {
    using namespace multio::datamod;

    EXPECT_THROWS(read(keySet<TestKeys>(), Metadata{{"key2", 2.0}, {"key3", 3}, {"key5", true}}));
};


CASE("Test reify description and write to metadata [1]") {
    using namespace multio::datamod;

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
    using namespace multio::datamod;

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
    using namespace multio::datamod;

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
    using namespace multio::datamod;

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
    using namespace multio::datamod;

    auto testKeys = read(keySet<TestKeys::Key1, TestKeys2::Key1>().scoped(),
                         Metadata{{"test-key1", "val1"}, {"test2-key1", "val2"}});
    EXPECT_NO_THROW(validate(testKeys));

    EXPECT_EQUAL(key<TestKeys::Key1>(testKeys).get(), "val1");
    EXPECT_EQUAL(key<TestKeys2::Key1>(testKeys).get(), "val2");
};


CASE("Test reify custom scoped description from metadata [2]") {
    using namespace multio::datamod;

    auto testKeys = read(keySet<TestKeys::Key1, TestKeys2::Key1>().makeScoped(),
                         Metadata{{"test-key1", "val1"}, {"test2-key1", "val2"}});
    EXPECT_NO_THROW(validate(testKeys));

    EXPECT_EQUAL(key<TestKeys::Key1>(testKeys).get(), "val1");
    EXPECT_EQUAL(key<TestKeys2::Key1>(testKeys).get(), "val2");
};


// CASE("Test reify custom scoped description from existing KeyValueSet [1]") {
//     using namespace multio::datamod;

//     auto alltestKeys = read(keySet<TestKeys>(), Metadata{{"key1", "val1"}, {"key2", 2.0}, {"key3", 3}, {"key4", 4},
//     {"key5", true}});

//     auto testKeys = read(keySet<TestKeys::Key1, TestKeys2::Key1>().scoped(), Metadata{{"test-key1", "val1"},
//     {"test2-key1", "val2"}}); EXPECT_NO_THROW(validate(testKeys));

//     EXPECT_EQUAL(key<TestKeys::Key1>(testKeys).get(), "val1");
//     EXPECT_EQUAL(key<TestKeys2::Key1>(testKeys).get(), "val2");
// };


CASE("Test setting defaults") {
    using namespace multio::datamod;
    EXPECT_THROWS(read(keySet<TestOpts>(), Metadata{}));
    EXPECT_NO_THROW(read(keySet<TestOpts>(), Metadata{{"req-key", "val"}}));

    {
        auto opts = read(keySet<TestOpts>(), Metadata{{"req-key", "val"}});
        EXPECT_EQUAL(key<TestOpts::DefKey1>(opts).isMissing(), false);
        EXPECT_EQUAL(key<TestOpts::DefKey1>(opts).get(), "default-key1-value");
        EXPECT_EQUAL(key<TestOpts::DefKey2>(opts).isMissing(), false);
        EXPECT_EQUAL(key<TestOpts::DefKey2>(opts).get(), "default-key1-value");
    }

    {
        auto opts = read(keySet<TestOpts>(), Metadata{{"req-key", "val"}, {"def-key1", "val1"}});
        EXPECT_EQUAL(key<TestOpts::DefKey1>(opts).isMissing(), false);
        EXPECT_EQUAL(key<TestOpts::DefKey1>(opts).get(), "val1");
        EXPECT_EQUAL(key<TestOpts::DefKey2>(opts).isMissing(), false);
        EXPECT_EQUAL(key<TestOpts::DefKey2>(opts).get(), "val1");
    }

    {
        auto opts = read(keySet<TestOpts>(), Metadata{{"req-key", "val"}, {"def-key2", "val2"}});
        EXPECT_EQUAL(key<TestOpts::DefKey1>(opts).isMissing(), false);
        EXPECT_EQUAL(key<TestOpts::DefKey1>(opts).get(), "default-key1-value");
        EXPECT_EQUAL(key<TestOpts::DefKey2>(opts).isMissing(), false);
        EXPECT_EQUAL(key<TestOpts::DefKey2>(opts).get(), "val2");
    }
}


CASE("Test hashing a keyset") {
    using namespace multio::datamod;

    auto testKeys = read(keySet<TestKeys::Key1, TestKeys2::Key1>().makeScoped(),
                         Metadata{{"test-key1", "val1"}, {"test2-key1", "val2"}});

    EXPECT(util::hash(testKeys) != 0);
};


}  // namespace multio::test

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

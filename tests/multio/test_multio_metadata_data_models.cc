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
#include "multio/message/Glossary.h"


namespace multio::test {

CASE("Test reading Param keys from metadata") {
    using namespace multio::message;

    auto keys = keySet<MarsKeys::PARAM>();

    EXPECT_EQUAL(key<MarsKeys::PARAM>(read(keys, Metadata{{"param", 3}})).get(), 3);
    EXPECT_EQUAL(key<MarsKeys::PARAM>(read(keys, Metadata{{"param", "123.456"}})).get(), 456123);
};

CASE("Test reading time durationkeys from metadata") {
    using namespace multio::message;

    auto keys = keySet<MarsKeys::STEP>();

    EXPECT((key<MarsKeys::STEP>(read(keys, Metadata{{"step", 3}})).get()) == (TimeDuration{std::chrono::hours{3}}));
    EXPECT((key<MarsKeys::STEP>(read(keys, Metadata{{"step", 3}})).get()) == (TimeDuration{std::chrono::hours{3}}));
    EXPECT((key<MarsKeys::STEP>(read(keys, Metadata{{"step", "3h"}})).get()) == (TimeDuration{std::chrono::hours{3}}));
    EXPECT((key<MarsKeys::STEP>(read(keys, Metadata{{"step", "3s"}})).get())
           == (TimeDuration{std::chrono::seconds{3}}));
    EXPECT_THROWS(key<MarsKeys::STEP>(read(keys, Metadata{{"step", "3m"}})).get());
};


multio::message::Metadata makeMarsMetadata() {
    return multio::message::Metadata{{"expver", "123"},      {"stream", "stream"}, {"type", "fc"},
                                     {"class", "od"},        {"origin", "ecmf"},   {"anoffset", 20240101},
                                     {"packing", "packing"}, {"number", 4},        {"ident", 5},
                                     {"instrument", 6},      {"channel", 7},       {"chem", 500},
                                     {"param", 3},           {"model", "model"},   {"levtype", "heightAboveGround"},
                                     {"levelist", 0},        {"direction", 1},     {"frequency", 2},
                                     {"date", 20220103},     {"time", 180000},     {"step", 4},
                                     {"timeproc", 1},        {"hdate", 20240101},  {"grid", "O320"},
                                     {"truncation", 399}};
}


CASE("Test reading MARS keys from metadata") {
    using namespace multio::message;

    auto marsKeys = read(keySet<MarsKeys>(), makeMarsMetadata());

    // TODO elaborate - should check inconsistences
    EXPECT_NO_THROW(validate(marsKeys));
};


CASE("Test encoder hashes") {
    using namespace multio::message;

    auto marsKeys = read(keySet<MarsKeys>(), makeMarsMetadata());
    acquire(marsKeys);

    // Create copies
    auto mk1 = marsKeys;
    auto mk2 = marsKeys;

    // Frequency & direction should not change hash
    key<MarsKeys::FREQUENCY>(mk2).set(11);
    key<MarsKeys::DIRECTION>(mk2).set(11);

    EXPECT_EQUAL(util::hash(getEncoderCacheKeys(mk1)), util::hash(getEncoderCacheKeys(mk2)));

    auto mk22 = mk2;
    key<MarsKeys::LEVELIST>(mk22).set(1);  // LEVEL can change hash ?
    EXPECT(util::hash(getEncoderCacheKeys(mk1)) != util::hash(getEncoderCacheKeys(mk22)));


    auto mk3 = marsKeys;
    key<MarsKeys::PARAM>(mk3).set(123);  // Change param because hash should be different to mk1 & 2
    EXPECT(util::hash(getEncoderCacheKeys(mk1)) != util::hash(getEncoderCacheKeys(mk3)));

    key<MarsKeys::LEVTYPE>(mk3).set("ml");
    auto mk4 = mk3;
    key<MarsKeys::LEVELIST>(mk3).set(9000);  // FOR ml levelist should not change hash
    EXPECT_EQUAL(util::hash(getEncoderCacheKeys(mk3)), util::hash(getEncoderCacheKeys(mk4)));

    // Example cache
    std::unordered_map<EncoderCacheMarsKeyValueSet, int> cache{};
    cache.insert_or_assign(getEncoderCacheKeys(mk1), 1);
    cache.insert_or_assign(getEncoderCacheKeys(mk2), 2);
    cache.insert_or_assign(getEncoderCacheKeys(mk22), 22);
    cache.insert_or_assign(getEncoderCacheKeys(mk3), 3);
    cache.insert_or_assign(getEncoderCacheKeys(mk4), 4);

    EXPECT_EQUAL(cache[getEncoderCacheKeys(mk1)], 2);
    EXPECT_EQUAL(cache[getEncoderCacheKeys(mk2)], 2);
    EXPECT_EQUAL(cache[getEncoderCacheKeys(mk22)], 22);
    EXPECT_EQUAL(cache[getEncoderCacheKeys(mk3)], 4);
    EXPECT_EQUAL(cache[getEncoderCacheKeys(mk4)], 4);
};


// TODO test geometry


}  // namespace multio::test

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

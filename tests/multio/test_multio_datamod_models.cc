/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */


#include "eckit/testing/Test.h"

#include "multio/datamod/ContainerInterop.h"
#include "multio/datamod/MarsMiscGeo.h"

#include "multio/mars2grib/MarsCachedKeys.h"

#include "multio/datamod/core/EntryParser.h"
#include "multio/message/Metadata.h"


namespace multio::test {
using multio::message::Metadata;

namespace dm = multio::datamod;

CASE("Test reading Param keys from metadata") {
    using namespace dm;

    EXPECT_EQUAL(parseEntry(PARAM, Metadata{{"param", 3}}).get(), 3);
    EXPECT_EQUAL(parseEntry(PARAM, Metadata{{"param", "123.456"}}).get(), 456123);
};

CASE("Test reading time durationkeys from metadata") {
    using namespace dm;

    EXPECT((parseEntry(STEP, Metadata{{"step", 3}}).get()) == (TimeDuration{std::chrono::hours{3}}));
    EXPECT((parseEntry(STEP, Metadata{{"step", 3}}).get()) == (TimeDuration{std::chrono::hours{3}}));
    EXPECT((parseEntry(STEP, Metadata{{"step", "3h"}}).get()) == (TimeDuration{std::chrono::hours{3}}));
    EXPECT((parseEntry(STEP, Metadata{{"step", "3s"}}).get()) == (TimeDuration{std::chrono::seconds{3}}));
    EXPECT_THROWS(parseEntry(STEP, Metadata{{"step", "3m"}}).get());
};


multio::message::Metadata makeMarsMetadata() {
    return multio::message::Metadata{
        {"expver", "123"},  {"stream", "stream"},   {"type", "fc"},         {"class", "od"},
        {"origin", "ecmf"}, {"anoffset", 20240101}, {"packing", "packing"}, {"number", 4},
        {"ident", 5},       {"instrument", 6},      {"channel", 7},         {"chem", 500},
        {"param", 3},       {"model", "model"},     {"levtype", "hl"},      {"levelist", 0},
        {"direction", 1},   {"frequency", 2},       {"date", 20220103},     {"time", 180000},
        {"step", 4},        {"timespan", 1},        {"hdate", 20240101},    {"grid", "O320"},
        {"truncation", 399}};
}

multio::message::Metadata makeValidMarsMetadata() {
    auto md = makeMarsMetadata();
    md.erase("truncation");
    return md;
}


CASE("Test reading MARS keys from metadata") {
    using namespace dm;

    {
        // Expect error because of having grid & truncation defined at the same time
        EXPECT_THROWS(readRecord<MarsRecord>(makeMarsMetadata()));
    }

    {
        auto md = makeMarsMetadata();
        md.erase("truncation");
        EXPECT_NO_THROW(readRecord<MarsRecord>(md));
    }

    {
        auto md = makeMarsMetadata();
        md.erase("grid");
        EXPECT_NO_THROW(readRecord<MarsRecord>(md));
    }

    {
        auto md = makeMarsMetadata();
        md.erase("truncation");
        md.erase("grid");
        EXPECT_THROWS(readRecord<MarsRecord>(md));
    }
};


CASE("Test encoder hashes") {
    using namespace dm;

    auto marsKeys = readRecordByValue<MarsRecord>(makeValidMarsMetadata());

    // Create copies
    auto mk1 = marsKeys;
    auto mk2 = marsKeys;

    // Frequency & direction should not change hash
    mk2.frequency.set(11);
    mk2.direction.set(11);

    EXPECT_EQUAL(util::hash(readRecord<mars2grib::MarsCacheRecord>(mk1)),
                 util::hash(readRecord<mars2grib::MarsCacheRecord>(mk2)));

    auto mk22 = mk2;
    mk22.levelist.set(1);  // LEVEL can change hash ?
    EXPECT(util::hash(readRecord<mars2grib::MarsCacheRecord>(mk1))
           != util::hash(readRecord<mars2grib::MarsCacheRecord>(mk22)));


    auto mk3 = marsKeys;
    mk3.param.set(123);  // Change param because hash should be different to mk1 & 2
    EXPECT(util::hash(readRecord<mars2grib::MarsCacheRecord>(mk1))
           != util::hash(readRecord<mars2grib::MarsCacheRecord>(mk3)));

    mk3.levtype.set("ml");
    auto mk4 = mk3;
    mk3.levelist.set(9000);  // FOR ml levelist should not change hash
    EXPECT_EQUAL(util::hash(readRecord<mars2grib::MarsCacheRecord>(mk3)),
                 util::hash(readRecord<mars2grib::MarsCacheRecord>(mk4)));

    // Example cache
    std::unordered_map<mars2grib::MarsCacheRecord, int> cache{};
    cache.insert_or_assign(readRecord<mars2grib::MarsCacheRecord>(mk1), 1);
    cache.insert_or_assign(readRecord<mars2grib::MarsCacheRecord>(mk2), 2);
    cache.insert_or_assign(readRecord<mars2grib::MarsCacheRecord>(mk22), 22);
    cache.insert_or_assign(readRecord<mars2grib::MarsCacheRecord>(mk3), 3);
    cache.insert_or_assign(readRecord<mars2grib::MarsCacheRecord>(mk4), 4);

    EXPECT_EQUAL(cache[readRecord<mars2grib::MarsCacheRecord>(mk1)], 2);
    EXPECT_EQUAL(cache[readRecord<mars2grib::MarsCacheRecord>(mk2)], 2);
    EXPECT_EQUAL(cache[readRecord<mars2grib::MarsCacheRecord>(mk22)], 22);
    EXPECT_EQUAL(cache[readRecord<mars2grib::MarsCacheRecord>(mk3)], 4);
    EXPECT_EQUAL(cache[readRecord<mars2grib::MarsCacheRecord>(mk4)], 4);
};


// TODO test geometry


}  // namespace multio::test

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

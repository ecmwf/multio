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
#include "multio/datamod/MarsCachedKeys.h"

#include "multio/datamod/types/LevType.h"
#include "multio/datamod/types/TypeOfStatisticalProcessing.h"

#include "multio/datamod/core/EntryParser.h"
#include "multio/message/Metadata.h"


namespace multio::test {
using multio::message::Metadata;

namespace dm = multio::datamod;

CASE("Test reading Param keys from metadata") {
    using namespace dm;

    EXPECT_EQUAL(parseEntry(PARAM, Metadata{{"param", 3}}).get().id(), 3);
    EXPECT_EQUAL(parseEntry(PARAM, Metadata{{"param", "123.456"}}).get().id(), 456123);
};

CASE("Test parsing/dumping levtype") {
    using namespace dm;

    for (auto levtype : dm::allLevTypes()) {
        EXPECT((ParseType<dm::LevType>::parse(DumpType<dm::LevType>::dump(levtype)) == levtype));
    }


    EXPECT_THROWS(ParseType<dm::LevType>::parse("no_valid_level"));
};

CASE("Test parsing/dumping stattype") {
    using namespace dm;

    EXPECT((DumpType<dm::StatType>::dump(ParseType<dm::StatType>::parse("moav")) == "moav"));
    EXPECT((DumpType<dm::StatType>::dump(ParseType<dm::StatType>::parse("momn")) == "momn"));
    EXPECT((DumpType<dm::StatType>::dump(ParseType<dm::StatType>::parse("momx")) == "momx"));
    EXPECT((DumpType<dm::StatType>::dump(ParseType<dm::StatType>::parse("mosd")) == "mosd"));

    EXPECT((DumpType<dm::StatType>::dump(ParseType<dm::StatType>::parse("daav")) == "daav"));
    EXPECT((DumpType<dm::StatType>::dump(ParseType<dm::StatType>::parse("damn")) == "damn"));
    EXPECT((DumpType<dm::StatType>::dump(ParseType<dm::StatType>::parse("damx")) == "damx"));
    EXPECT((DumpType<dm::StatType>::dump(ParseType<dm::StatType>::parse("dasd")) == "dasd"));

    EXPECT((DumpType<dm::StatType>::dump(ParseType<dm::StatType>::parse("moav_daav")) == "moav_daav"));
    EXPECT((DumpType<dm::StatType>::dump(ParseType<dm::StatType>::parse("momn_damn")) == "momn_damn"));
    EXPECT((DumpType<dm::StatType>::dump(ParseType<dm::StatType>::parse("momx_damx")) == "momx_damx"));
    EXPECT((DumpType<dm::StatType>::dump(ParseType<dm::StatType>::parse("mosd_dasd")) == "mosd_dasd"));

    // Test throw due to invalid combination (durations have an order)
    EXPECT_THROWS(ParseType<dm::StatType>::parse("mosd_moav"));
    EXPECT_THROWS(ParseType<dm::StatType>::parse("dasd_daav"));
    EXPECT_THROWS(ParseType<dm::StatType>::parse("dasd_moav"));

    // Wrong separation character
    EXPECT_THROWS(ParseType<dm::StatType>::parse("mosd-daav"));
};

CASE("Test parsing/dumping typeOfStatisticalProcessing") {
    using namespace dm;

    // clang-format off
    EXPECT((DumpType<dm::TypeOfStatisticalProcessing>::dump(ParseType<dm::TypeOfStatisticalProcessing>::parse("average")) == "average"));
    EXPECT((DumpType<dm::TypeOfStatisticalProcessing>::dump(ParseType<dm::TypeOfStatisticalProcessing>::parse("accumul")) == "accumul"));
    EXPECT((DumpType<dm::TypeOfStatisticalProcessing>::dump(ParseType<dm::TypeOfStatisticalProcessing>::parse("max")) == "max"));
    EXPECT((DumpType<dm::TypeOfStatisticalProcessing>::dump(ParseType<dm::TypeOfStatisticalProcessing>::parse("min")) == "min"));
    EXPECT((DumpType<dm::TypeOfStatisticalProcessing>::dump(ParseType<dm::TypeOfStatisticalProcessing>::parse("difference")) == "difference"));
    EXPECT((DumpType<dm::TypeOfStatisticalProcessing>::dump(ParseType<dm::TypeOfStatisticalProcessing>::parse("root-mean-square")) == "root-mean-square"));
    EXPECT((DumpType<dm::TypeOfStatisticalProcessing>::dump(ParseType<dm::TypeOfStatisticalProcessing>::parse("stddev")) == "stddev"));
    EXPECT((DumpType<dm::TypeOfStatisticalProcessing>::dump(ParseType<dm::TypeOfStatisticalProcessing>::parse("covariance")) == "covariance"));
    EXPECT((DumpType<dm::TypeOfStatisticalProcessing>::dump(ParseType<dm::TypeOfStatisticalProcessing>::parse("inverse-difference")) == "inverse-difference"));
    EXPECT((DumpType<dm::TypeOfStatisticalProcessing>::dump(ParseType<dm::TypeOfStatisticalProcessing>::parse("ratio")) == "ratio"));
    EXPECT((DumpType<dm::TypeOfStatisticalProcessing>::dump(ParseType<dm::TypeOfStatisticalProcessing>::parse("standardized-anomaly")) == "standardized-anomaly"));
    EXPECT((DumpType<dm::TypeOfStatisticalProcessing>::dump(ParseType<dm::TypeOfStatisticalProcessing>::parse("summation")) == "summation"));
    EXPECT((DumpType<dm::TypeOfStatisticalProcessing>::dump(ParseType<dm::TypeOfStatisticalProcessing>::parse("return-period")) == "return-period"));
    EXPECT((DumpType<dm::TypeOfStatisticalProcessing>::dump(ParseType<dm::TypeOfStatisticalProcessing>::parse("median")) == "median"));
    EXPECT((DumpType<dm::TypeOfStatisticalProcessing>::dump(ParseType<dm::TypeOfStatisticalProcessing>::parse("severity")) == "severity"));
    EXPECT((DumpType<dm::TypeOfStatisticalProcessing>::dump(ParseType<dm::TypeOfStatisticalProcessing>::parse("mode")) == "mode"));
    EXPECT((DumpType<dm::TypeOfStatisticalProcessing>::dump(ParseType<dm::TypeOfStatisticalProcessing>::parse("index-processing")) == "index-processing"));

    EXPECT((ParseType<dm::TypeOfStatisticalProcessing>::parse(DumpType<dm::TypeOfStatisticalProcessing>::dump(TypeOfStatisticalProcessing::Average)) == TypeOfStatisticalProcessing::Average));
    EXPECT((ParseType<dm::TypeOfStatisticalProcessing>::parse(DumpType<dm::TypeOfStatisticalProcessing>::dump(TypeOfStatisticalProcessing::Accumulation)) == TypeOfStatisticalProcessing::Accumulation));
    EXPECT((ParseType<dm::TypeOfStatisticalProcessing>::parse(DumpType<dm::TypeOfStatisticalProcessing>::dump(TypeOfStatisticalProcessing::Maximum)) == TypeOfStatisticalProcessing::Maximum));
    EXPECT((ParseType<dm::TypeOfStatisticalProcessing>::parse(DumpType<dm::TypeOfStatisticalProcessing>::dump(TypeOfStatisticalProcessing::Minimum)) == TypeOfStatisticalProcessing::Minimum));
    EXPECT((ParseType<dm::TypeOfStatisticalProcessing>::parse(DumpType<dm::TypeOfStatisticalProcessing>::dump(TypeOfStatisticalProcessing::Difference)) == TypeOfStatisticalProcessing::Difference));
    EXPECT((ParseType<dm::TypeOfStatisticalProcessing>::parse(DumpType<dm::TypeOfStatisticalProcessing>::dump(TypeOfStatisticalProcessing::RootMeanSquare)) == TypeOfStatisticalProcessing::RootMeanSquare));
    EXPECT((ParseType<dm::TypeOfStatisticalProcessing>::parse(DumpType<dm::TypeOfStatisticalProcessing>::dump(TypeOfStatisticalProcessing::StandardDeviation)) == TypeOfStatisticalProcessing::StandardDeviation));
    EXPECT((ParseType<dm::TypeOfStatisticalProcessing>::parse(DumpType<dm::TypeOfStatisticalProcessing>::dump(TypeOfStatisticalProcessing::Covariance)) == TypeOfStatisticalProcessing::Covariance));
    EXPECT((ParseType<dm::TypeOfStatisticalProcessing>::parse(DumpType<dm::TypeOfStatisticalProcessing>::dump(TypeOfStatisticalProcessing::InverseDifference)) == TypeOfStatisticalProcessing::InverseDifference));
    EXPECT((ParseType<dm::TypeOfStatisticalProcessing>::parse(DumpType<dm::TypeOfStatisticalProcessing>::dump(TypeOfStatisticalProcessing::Ratio)) == TypeOfStatisticalProcessing::Ratio));
    EXPECT((ParseType<dm::TypeOfStatisticalProcessing>::parse(DumpType<dm::TypeOfStatisticalProcessing>::dump(TypeOfStatisticalProcessing::StandardizedAnomaly)) == TypeOfStatisticalProcessing::StandardizedAnomaly));
    EXPECT((ParseType<dm::TypeOfStatisticalProcessing>::parse(DumpType<dm::TypeOfStatisticalProcessing>::dump(TypeOfStatisticalProcessing::Summation)) == TypeOfStatisticalProcessing::Summation));
    EXPECT((ParseType<dm::TypeOfStatisticalProcessing>::parse(DumpType<dm::TypeOfStatisticalProcessing>::dump(TypeOfStatisticalProcessing::ReturnPeriod)) == TypeOfStatisticalProcessing::ReturnPeriod));
    EXPECT((ParseType<dm::TypeOfStatisticalProcessing>::parse(DumpType<dm::TypeOfStatisticalProcessing>::dump(TypeOfStatisticalProcessing::Median)) == TypeOfStatisticalProcessing::Median));
    EXPECT((ParseType<dm::TypeOfStatisticalProcessing>::parse(DumpType<dm::TypeOfStatisticalProcessing>::dump(TypeOfStatisticalProcessing::Severity)) == TypeOfStatisticalProcessing::Severity));
    EXPECT((ParseType<dm::TypeOfStatisticalProcessing>::parse(DumpType<dm::TypeOfStatisticalProcessing>::dump(TypeOfStatisticalProcessing::Mode)) == TypeOfStatisticalProcessing::Mode));
    EXPECT((ParseType<dm::TypeOfStatisticalProcessing>::parse(DumpType<dm::TypeOfStatisticalProcessing>::dump(TypeOfStatisticalProcessing::IndexProcessing)) == TypeOfStatisticalProcessing::IndexProcessing));
    // clang-format on

    EXPECT_THROWS(ParseType<dm::TypeOfStatisticalProcessing>::parse("noop"));
}

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
        EXPECT_THROWS(readRecord<FullMarsRecord>(makeMarsMetadata()));
    }

    {
        auto md = makeMarsMetadata();
        md.erase("truncation");
        EXPECT_NO_THROW(readRecord<FullMarsRecord>(md));
    }

    {
        auto md = makeMarsMetadata();
        md.erase("grid");
        EXPECT_NO_THROW(readRecord<FullMarsRecord>(md));
    }

    {
        auto md = makeMarsMetadata();
        md.erase("truncation");
        md.erase("grid");
        EXPECT_THROWS(readRecord<FullMarsRecord>(md));
    }
};


CASE("Test encoder hashes") {
    using namespace dm;

    auto marsKeys = readRecordByValue<FullMarsRecord>(makeValidMarsMetadata());

    // Create copies
    auto mk1 = marsKeys;
    auto mk2 = marsKeys;

    // Frequency & direction should not change hash
    mk2.frequency.set(11);
    mk2.direction.set(11);

    EXPECT_EQUAL(util::hash(readRecord<dm::MarsCacheRecord>(mk1)),
                 util::hash(readRecord<dm::MarsCacheRecord>(mk2)));

    auto mk22 = mk2;
    mk22.levelist.set(1);  // LEVEL can change hash ?
    EXPECT(util::hash(readRecord<dm::MarsCacheRecord>(mk1))
           != util::hash(readRecord<dm::MarsCacheRecord>(mk22)));


    auto mk3 = marsKeys;
    mk3.param.set(123);  // Change param because hash should be different to mk1 & 2
    EXPECT(util::hash(readRecord<dm::MarsCacheRecord>(mk1))
           != util::hash(readRecord<dm::MarsCacheRecord>(mk3)));

    mk3.levtype.set("ml");
    auto mk4 = mk3;
    mk3.levelist.set(9000);  // FOR ml levelist should not change hash
    EXPECT_EQUAL(util::hash(readRecord<dm::MarsCacheRecord>(mk3)),
                 util::hash(readRecord<dm::MarsCacheRecord>(mk4)));

    // Example cache
    std::unordered_map<dm::MarsCacheRecord, int> cache{};
    cache.insert_or_assign(readRecord<dm::MarsCacheRecord>(mk1), 1);
    cache.insert_or_assign(readRecord<dm::MarsCacheRecord>(mk2), 2);
    cache.insert_or_assign(readRecord<dm::MarsCacheRecord>(mk22), 22);
    cache.insert_or_assign(readRecord<dm::MarsCacheRecord>(mk3), 3);
    cache.insert_or_assign(readRecord<dm::MarsCacheRecord>(mk4), 4);

    EXPECT_EQUAL(cache[readRecord<dm::MarsCacheRecord>(mk1)], 2);
    EXPECT_EQUAL(cache[readRecord<dm::MarsCacheRecord>(mk2)], 2);
    EXPECT_EQUAL(cache[readRecord<dm::MarsCacheRecord>(mk22)], 22);
    EXPECT_EQUAL(cache[readRecord<dm::MarsCacheRecord>(mk3)], 4);
    EXPECT_EQUAL(cache[readRecord<dm::MarsCacheRecord>(mk4)], 4);
};


// TODO test geometry


}  // namespace multio::test

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

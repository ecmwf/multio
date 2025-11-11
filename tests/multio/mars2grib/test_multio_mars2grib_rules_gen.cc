/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "eckit/config/LocalConfiguration.h"
#include "eckit/config/YAMLConfiguration.h"
#include "eckit/filesystem/PathName.h"
#include "eckit/testing/Test.h"

#include "multio/datamod/ContainerInterop.h"
#include "multio/datamod/core/EntryDef.h"
#include "multio/mars2grib/EncoderConf.h"
#include "multio/mars2grib/Rules.h"
#include "multio/mars2grib/generated/InferPDT.h"
#include "multio/mars2grib/rules/Matcher.h"
#include "multio/mars2grib/rules/Rule.h"

#include "multio/datamod/MarsMiscGeo.h"
#include "multio/mars2grib/sections/Level.h"
#include "multio/util/Print.h"
#include "multio/util/SampleMetadataGen.h"
#include "test_multio_mars2grib_helper.h"


namespace multio::test {

namespace dm = multio::datamod;

CASE("Test rules gen matchers") {
    using namespace multio::mars2grib::rules;
    using namespace multio::mars2grib::matcher;
    using namespace multio::mars2grib;

    static auto ruleSet = exclusiveRuleList("testRuleSet",
        // Branch for grids
        chainedRuleList(
            rule(all(Has{&dm::FullMarsRecord::grid}, NoneOf{&dm::FullMarsRecord::levtype, {dm::LevType::AL}})),
            rule(OneOf{&dm::FullMarsRecord::param, {1, 3, 4}}, Setter([](SectionsConf& c) {
                     c.product.ensureInit().modify().pdtCat.ensureInit().modify().timeExtent.set(
                         TimeExtent::PointInTime);
                     c.product.ensureInit().modify().level.ensureInit().modify().type.set(
                         dm::TypeOfLevel::HeightAboveGround);
                 }))),

        // Branch for spherical harmonics
        chainedRuleList(
            rule(all(Has{&dm::FullMarsRecord::truncation}, NoneOf{&dm::FullMarsRecord::levtype, {dm::LevType::AL}}))),

        // Branch for abstract level
        chainedRuleList(rule(OneOf{&dm::FullMarsRecord::levtype, {dm::LevType::AL}})));

    {
        dm::FullMarsRecord mars{};
        SectionsConf sections;

        // Nothing should match the outer rule, which is allowed to not match
        EXPECT(ruleSet(mars, sections) != true);
    }

    {
        auto md = util::sample_gen::mkMd();
        md.set("param", 1);

        auto mars = dm::readRecord<dm::FullMarsRecord>(md);
        SectionsConf sections;

        EXPECT(ruleSet(mars, sections));
        EXPECT((sections.product.get().level.get().type.get()) == dm::TypeOfLevel::HeightAboveGround);
        EXPECT(((sections.product.get().pdtCat.get().timeExtent.get()) == TimeExtent::PointInTime));
    }

    {
        auto md = util::sample_gen::mkMd();
        md.set("param", 42);  // Not included in rule

        auto mars = dm::readRecord<dm::FullMarsRecord>(md);
        SectionsConf sections;

        // First rule matches because grid is given, but then no param matches - the rule is not fully determined and
        // throws
        EXPECT_THROWS(ruleSet(mars, sections));
    }
};


// KeyValueSets can be comprade directly, but here
// we want to exclude comparision of specific sections in detail until fully migrated
// Moreover this mechanism shows more detailed errors
template <typename Rec>
void detailedCompare(const Rec& computed, const Rec& expected) {
    using namespace multio::mars2grib;
    try {

        util::forEach(
            [&](const auto& entryDef) {
                // Exclude comparison for specific fields
                if constexpr (std::is_same_v<dm::EntryValueType_t<decltype(entryDef)>, rules::PDTCat>) {
                    return;
                }
                if constexpr (std::is_same_v<Rec, sections::LevelConfigurator>) {
                    if (entryDef.key() == sections::FixedLevel.key()) {
                        return;
                    }
                }

                const auto& compEntry = entryDef.get(computed);
                const auto& exptEntry = entryDef.get(expected);
                if constexpr (dm::IsRecord_v<dm::EntryValueType_t<decltype(entryDef)>>) {
                    if (compEntry.isSet() && !exptEntry.isSet()) {
                        std::ostringstream oss;
                        util::PrintStream ps(oss);
                        ps << "Nested record " << entryDef.keyInfo()
                           << " is given but expected to be missing. Computed: ";
                        ps << compEntry;
                        oss << std::endl;
                        throw eckit::Exception(oss.str(), Here());
                    }
                    if (!compEntry.isSet() && exptEntry.isSet()) {
                        std::ostringstream oss;
                        util::PrintStream ps(oss);
                        ps << "Nested record " << entryDef.keyInfo()
                           << " is missing but expected to be given. Expected: " << std::endl;
                        ps << exptEntry << std::endl;
                        ps << std::endl;
                        throw eckit::Exception(oss.str(), Here());
                    }
                    if (compEntry.isSet() && exptEntry.isSet()) {
                        try {
                            detailedCompare(compEntry.get(), exptEntry.get());
                        }
                        catch (...) {
                            std::cout << "Error comparing entry " << entryDef.keyInfo() << std::endl;
                            throw;
                        }
                    }
                }
                else {
                    if (compEntry != exptEntry) {
                        std::ostringstream oss;
                        util::PrintStream ps(oss);
                        ps << "Entries for " << entryDef.keyInfo() << " do not compare - got: " << std::endl;
                        ps << compEntry << std::endl;
                        ps << " - expected: " << std::endl;
                        ps << exptEntry;
                        ps << std::endl;
                        throw eckit::Exception(oss.str(), Here());
                    }
                }
            },
            dm::recordEntries(computed));
    }
    catch (...) {
        util::PrintStream ps{std::cout};
        ps << "Error comparing computed: " << std::endl;
        {
            util::IndentGuard g{ps};
            ps << computed << std::endl;
        }
        ps << "with expected: " << std::endl;
        {
            util::IndentGuard g{ps};
            ps << expected << std::endl;
        }
        std::cout << std::flush;
        throw;
    }
}

CASE("Test real rules matchers with AIFS single keys") {
    using namespace multio::mars2grib::rules;
    using namespace multio::mars2grib;

    for (auto md : multio::util::sample_gen::mkAifsSingleMd()) {
        try {
            auto mars = dm::readRecord<dm::FullMarsRecord>(md);
            SectionsConf sections;


            EXPECT(allRules()(mars, sections));
            EXPECT_NO_THROW(dm::applyRecordDefaults(sections));
            EXPECT_NO_THROW(dm::validateRecord(sections));

            EXPECT((sections.product.get().templateNumber.isSet()));

            SectionsConf expectedSections = expectedAIFSSingleEncoderSections(mars);

            detailedCompare(sections, expectedSections);
        }
        catch (...) {
            std::cout << "Error while generating & comparing message: " << md << std::endl;
            throw;
        }
    }
};


CASE("Test real rules matchers with AIFS ens keys") {
    using namespace multio::mars2grib::rules;
    using namespace multio::mars2grib;

    for (auto md : multio::util::sample_gen::mkAifsEnsMd()) {
        try {
            auto mars = dm::readRecord<dm::FullMarsRecord>(md);
            SectionsConf sections;


            EXPECT(mars2grib::rules::allRules()(mars, sections));
            EXPECT_NO_THROW(dm::applyRecordDefaults(sections));
            EXPECT_NO_THROW(dm::validateRecord(sections));

            EXPECT((sections.product.get().templateNumber.isSet()));

            SectionsConf expectedSections = expectedAIFSEnsEncoderSections(mars);

            detailedCompare(sections, expectedSections);
        }
        catch (...) {
            std::cout << "Error while generating & comparing message: " << md << std::endl;
            throw;
        }
    }
};


CASE("Test real rules matchers with AIFS JSON rules") {
    using namespace multio::mars2grib::rules;
    using namespace multio::mars2grib;

    auto marsMessages
        = eckit::LocalConfiguration{eckit::YAMLConfiguration{eckit::PathName{"mars-encoder-conf-test-aifs.json"}}}
              .getSubConfigurations("rules");

    std::size_t i = 0;
    for (auto subConf : marsMessages) {
        ++i;
        eckit::LocalConfiguration marsConf = subConf.getSubConfiguration("message");
        try {
            auto mars = dm::readRecord<dm::FullMarsRecord>(marsConf);
            SectionsConf expectedSections = dm::readRecord<SectionsConf>(subConf.getSubConfiguration("rule"));

            SectionsConf computedSections;

            EXPECT(mars2grib::rules::allRules()(mars, computedSections));
            EXPECT_NO_THROW(dm::applyRecordDefaults(computedSections));
            EXPECT_NO_THROW(dm::validateRecord(computedSections));

            detailedCompare(computedSections, expectedSections);
        }
        catch (...) {
            // Print additional information and rethrow
            std::cout << i << "/" << marsMessages.size() << ": error for mars conf: ";
            util::print(std::cout, marsConf);
            std::cout << std::endl;
            util::print(std::cout, subConf);
            std::cout << std::endl;
            throw;
        };
    }
};


// TODO(pgeier) have commented because of changes in the rules. New json must be generated.
//              But after merge this code is not needed anymore
// CASE("Test real rules matchers with era6 5220 JSON rules") {
//     using namespace multio::mars2grib::rules;
//     using namespace multio::mars2grib;

//     auto marsMessages
//         =
//         eckit::LocalConfiguration{eckit::YAMLConfiguration{eckit::PathName{"mars-encoder-conf-test-era6-5220.json"}}}
//               .getSubConfigurations("rules");

//     std::size_t i = 0;
//     for (auto subConf : marsMessages) {
//         ++i;
//         eckit::LocalConfiguration marsConf = subConf.getSubConfiguration("message");
//         try {
//             auto mars = dm::readRecord<dm::FullMarsRecord>(marsConf);
//             SectionsConf expectedSections = dm::readRecord<SectionsConf>(subConf.getSubConfiguration("rule"));

//             SectionsConf computedSections;

//             EXPECT(mars2grib::rules::allRules()(mars, computedSections));
//             EXPECT_NO_THROW(dm::applyRecordDefaults(computedSections));
//             EXPECT_NO_THROW(dm::validateRecord(computedSections));

//             detailedCompare(computedSections, expectedSections);
//         }
//         catch (...) {
//             // Print additional information and rethrow
//             std::cout << i << "/" << marsMessages.size() << ": error for mars conf: ";
//             util::print(std::cout, marsConf);
//             std::cout << std::endl;
//             util::print(std::cout, subConf);
//             std::cout << std::endl;
//             throw;
//         };
//     }
// };

}  // namespace multio::test

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

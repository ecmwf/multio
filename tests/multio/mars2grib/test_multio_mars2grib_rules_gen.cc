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
#include "multio/datamod/DataModelling.h"
#include "multio/mars2grib/EncoderConf.h"
#include "multio/mars2grib/Rules.h"
#include "multio/mars2grib/generated/InferPDT.h"
#include "multio/mars2grib/rules/Rule.h"

#include "multio/datamod/MarsMiscGeo.h"
#include "multio/message/Metadata.h"

#include "multio/util/SampleMetadataGen.h"
#include "test_multio_mars2grib_helper.h"


namespace multio::test {

CASE("Test rules gen matchers") {
    using namespace multio::mars2grib::rules;
    using namespace multio::mars2grib;
    using namespace multio::datamod;

    static auto ruleSet = exclusiveRuleList(
        // Branch for grids
        chainedRuleList(
            rule(all(Has<MarsKeys::GRID>{}, NoneOf<MarsKeys::LEVTYPE>{{LevType::AL}})),
            rule(OneOf<MarsKeys::PARAM>{{1, 3, 4}},
                 setAll(setKey<PDTCatDef::TimeExtent, EncoderSectionsDef::Product, EncoderProductDef::PDTCat>(
                            {TimeExtent::PointInTime}),  //
                        setKey<LevelDef::Type, EncoderSectionsDef::Product, EncoderProductDef::Level>(
                            {TypeOfLevel::HeightAboveGround})  //
                        ))),

        // Branch for spherical harmonics
        chainedRuleList(rule(all(Has<MarsKeys::TRUNCATION>{}, NoneOf<MarsKeys::LEVTYPE>{{LevType::AL}}))),

        // Branch for abstract level
        chainedRuleList(rule(OneOf<MarsKeys::LEVTYPE>{{LevType::AL}})));

    {
        auto mars = MarsKeyValueSet{};
        EncoderSections sections;

        // Nothing should match the outer rule, which is allowed to not match
        EXPECT(ruleSet(mars, sections) != true);
    }

    {
        auto md = util::sample_gen::mkMd();
        md.set("param", 1);

        auto mars = read(MarsKeySet{}, md);
        EncoderSections sections;

        EXPECT(ruleSet(mars, sections));
        EXPECT((keyPath<EncoderSectionsDef::Product, EncoderProductDef::Level, LevelDef::Type>(sections).get())
               == TypeOfLevel::HeightAboveGround);
        EXPECT(((keyPath<EncoderSectionsDef::Product, EncoderProductDef::PDTCat, PDTCatDef::TimeExtent>(sections).get())
                == TimeExtent::PointInTime));
    }

    {
        auto md = util::sample_gen::mkMd();
        md.set("param", 42);  // Not included in rule

        auto mars = read(MarsKeySet{}, md);
        EncoderSections sections;

        // First rule matches because grid is given, but then no param matches - the rule is not fully determined and
        // throws
        EXPECT_THROWS(ruleSet(mars, sections));
    }
};


// KeyValueSets can be comprade directly, but here
// we want to exclude comparision of specific sections in detail until fully migrated
// Moreover this mechanism shows more detailed errors
template <typename KS>
void detailedCompare(const multio::datamod::KeyValueSet<KS>& computed,
                     const multio::datamod::KeyValueSet<KS>& expected) {
    using namespace multio::datamod;
    using namespace multio::mars2grib;

    util::forEach(
        [&](const auto& kvd) {
            constexpr auto ID = std::decay_t<decltype(kvd)>::id;
            // Exclude comparison for specific fields
            if constexpr (  //
                !std::is_same_v<KeyId<ID>, KeyId<EncoderProductDef::PDTCat>>
                && !std::is_same_v<KeyId<ID>, KeyId<LevelDef::FixedLevel>>  //
            ) {
                const auto& compEntry = key<ID>(computed);
                const auto& exptEntry = key<ID>(expected);
                if constexpr (IsKeyValueSet_v<KeyDefValueType_t<ID>>) {
                    if (compEntry.has() && exptEntry.isMissing()) {
                        std::ostringstream oss;
                        oss << "Nested key value set " << kvd.keyInfo()
                            << " is given but expected to be missing. Computed: ";
                        util::print(oss, compEntry);
                        oss << std::endl;
                        throw eckit::Exception(oss.str(), Here());
                    }
                    if (compEntry.isMissing() && exptEntry.has()) {
                        std::ostringstream oss;
                        oss << "Nested key value set " << kvd.keyInfo()
                            << " is missing but expected to be given. Expected: ";
                        util::print(oss, exptEntry);
                        oss << std::endl;
                        throw eckit::Exception(oss.str(), Here());
                    }
                    if (compEntry.has() && exptEntry.has()) {
                        detailedCompare(compEntry.get(), exptEntry.get());
                    }
                }
                else {
                    EXPECT((compEntry == exptEntry));
                }
            }
        },
        KS{});
}

CASE("Test real rules matchers with AIFS single keys") {
    using namespace multio::mars2grib::rules;
    using namespace multio::mars2grib;
    using namespace multio::datamod;

    for (auto md : multio::util::sample_gen::mkAifsSingleMd()) {
        try {
            auto mars = read(MarsKeySet{}, md);
            EncoderSections sections;


            EXPECT(mars2grib::rules::allRules()(mars, sections));
            EXPECT_NO_THROW(alterAndValidate(sections));

            EXPECT((keyPath<EncoderSectionsDef::Product, EncoderProductDef::TemplateNumber>(sections).has()));

            EncoderSections expectedSections = expectedAIFSSingleEncoderSections(mars);

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
    using namespace multio::datamod;

    for (auto md : multio::util::sample_gen::mkAifsEnsMd()) {
        try {
            auto mars = read(MarsKeySet{}, md);
            EncoderSections sections;


            EXPECT(mars2grib::rules::allRules()(mars, sections));
            EXPECT_NO_THROW(alterAndValidate(sections));

            EXPECT((keyPath<EncoderSectionsDef::Product, EncoderProductDef::TemplateNumber>(sections).has()));

            EncoderSections expectedSections = expectedAIFSEnsEncoderSections(mars);

            detailedCompare(sections, expectedSections);
        }
        catch (...) {
            std::cout << "Error while generating & comparing message: " << md << std::endl;
            throw;
        }
    }
};


CASE("Test real rules matchers with JSON rules") {
    using namespace multio::mars2grib::rules;
    using namespace multio::mars2grib;
    using namespace multio::datamod;

    auto marsMessages
        = eckit::LocalConfiguration{eckit::YAMLConfiguration{eckit::PathName{"mars-encoder-conf-test.json"}}}
              .getSubConfigurations("rules");

    std::size_t i = 0;
    for (auto subConf : marsMessages) {
        ++i;
        eckit::LocalConfiguration marsConf = subConf.getSubConfiguration("message");
        try {
            MarsKeyValueSet mars = read(MarsKeySet{}, marsConf);
            EncoderSections expectedSections = read(EncoderSectionsKeySet{}, subConf.getSubConfiguration("rule"));

            EncoderSections computedSections;

            EXPECT(mars2grib::rules::allRules()(mars, computedSections));
            EXPECT_NO_THROW(alterAndValidate(computedSections));

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

}  // namespace multio::test

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

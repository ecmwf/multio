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

#include "multio/mars2mars/Rules.h"

#include "multio/datamod/MarsRecord.h"

namespace multio::test {

CASE("Test fixParam35ToSol1") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    MarsRecord mars;
    MiscRecord misc;

    mars.param = Param{35};
    mars.levtype = LevType::SFC;

    auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

    EXPECT(res);
    EXPECT(mars.param.value() == Param{262024});
    EXPECT(mars.levelist.has_value());
    EXPECT(mars.levelist.value() == 1);
    EXPECT(mars.levtype.value() == LevType::SOL);
};


CASE("Test fixParam36ToSol2") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    MarsRecord mars;
    MiscRecord misc;

    mars.param = Param{36};
    mars.levtype = LevType::SFC;

    auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

    EXPECT(res);
    EXPECT(mars.param.value() == Param{262024});
    EXPECT(mars.levelist.has_value());
    EXPECT(mars.levelist.value() == 2);
    EXPECT(mars.levtype.value() == LevType::SOL);
};


CASE("Test fixParam37ToSol3") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    MarsRecord mars;
    MiscRecord misc;

    mars.param = Param{37};
    mars.levtype = LevType::SFC;

    auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

    EXPECT(res);
    EXPECT(mars.param.value() == Param{262024});
    EXPECT(mars.levelist.has_value());
    EXPECT(mars.levelist.value() == 3);
    EXPECT(mars.levtype.value() == LevType::SOL);
};

CASE("Test fixParam38ToSol4") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    MarsRecord mars;
    MiscRecord misc;

    mars.param = Param{38};
    mars.levtype = LevType::SFC;

    auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

    EXPECT(res);
    EXPECT(mars.param.value() == Param{262024});
    EXPECT(mars.levelist.has_value());
    EXPECT(mars.levelist.value() == 4);
    EXPECT(mars.levtype.value() == LevType::SOL);
};


CASE("Test fixParam39ToSol1") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    MarsRecord mars;
    MiscRecord misc;

    mars.param = Param{39};
    mars.levtype = LevType::SFC;

    auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

    EXPECT(res);
    EXPECT(mars.param.value() == Param{260199});
    EXPECT(mars.levelist.has_value());
    EXPECT(mars.levelist.value() == 1);
    EXPECT(mars.levtype.value() == LevType::SOL);
};


CASE("Test fixParam40ToSol2") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    MarsRecord mars;
    MiscRecord misc;

    mars.param = Param{40};
    mars.levtype = LevType::SFC;

    auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

    EXPECT(res);
    EXPECT(mars.param.value() == Param{260199});
    EXPECT(mars.levelist.has_value());
    EXPECT(mars.levelist.value() == 2);
    EXPECT(mars.levtype.value() == LevType::SOL);
};

CASE("Test fixParam41ToSol3") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    MarsRecord mars;
    MiscRecord misc;

    mars.param = Param{41};
    mars.levtype = LevType::SFC;

    auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

    EXPECT(res);
    EXPECT(mars.param.value() == Param{260199});
    EXPECT(mars.levelist.has_value());
    EXPECT(mars.levelist.value() == 3);
    EXPECT(mars.levtype.value() == LevType::SOL);
};

CASE("Test fixParam42ToSol4") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    MarsRecord mars;
    MiscRecord misc;

    mars.param = Param{42};
    mars.levtype = LevType::SFC;

    auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

    EXPECT(res);
    EXPECT(mars.param.value() == Param{260199});
    EXPECT(mars.levelist.has_value());
    EXPECT(mars.levelist.value() == 4);
    EXPECT(mars.levtype.value() == LevType::SOL);
};


CASE("Test fixParam139ToSol1") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    MarsRecord mars;
    MiscRecord misc;

    mars.param = Param{139};
    mars.levtype = LevType::SFC;

    auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

    EXPECT(res);
    EXPECT(mars.param.value() == Param{260360});
    EXPECT(mars.levelist.has_value());
    EXPECT(mars.levelist.value() == 1);
    EXPECT(mars.levtype.value() == LevType::SOL);
};

CASE("Test fixParam170ToSol2") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    MarsRecord mars;
    MiscRecord misc;

    mars.param = Param{170};
    mars.levtype = LevType::SFC;

    auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

    EXPECT(res);
    EXPECT(mars.param.value() == Param{260360});
    EXPECT(mars.levelist.has_value());
    EXPECT(mars.levelist.value() == 2);
    EXPECT(mars.levtype.value() == LevType::SOL);
};

CASE("Test fixParam183ToSol3") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    MarsRecord mars;
    MiscRecord misc;

    mars.param = Param{183};
    mars.levtype = LevType::SFC;

    auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

    EXPECT(res);
    EXPECT(mars.param.value() == Param{260360});
    EXPECT(mars.levelist.has_value());
    EXPECT(mars.levelist.value() == 3);
    EXPECT(mars.levtype.value() == LevType::SOL);
};


CASE("Test fixParam263ToSol4") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    MarsRecord mars;
    MiscRecord misc;

    mars.param = Param{236};
    mars.levtype = LevType::SFC;

    auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

    EXPECT(res);
    EXPECT(mars.param.value() == Param{260360});
    EXPECT(mars.levelist.has_value());
    EXPECT(mars.levelist.value() == 4);
    EXPECT(mars.levtype.value() == LevType::SOL);
};


CASE("Test fixTableVersion") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    std::vector<std::int64_t> paramIds{
        {228080, 228081, 228082, 228083, 228084, 228085, 233032, 233033, 233034, 233035, 235062, 235063, 235064}};

    for (auto paramId : paramIds) {
        MarsRecord mars;
        MiscRecord misc;

        mars.param = Param{paramId};

        auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

        EXPECT(res);
        EXPECT(misc.tablesVersion.has_value());
        EXPECT(misc.tablesVersion.value() == 30);
    }
};


CASE("Test fixCloudParam164") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    MarsRecord mars;
    MiscRecord misc;

    mars.param = Param{164};

    auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

    EXPECT(res);
    EXPECT(mars.param.value() == Param{228164});
    EXPECT(res->valuesScaleFactor);
    EXPECT(res->valuesScaleFactor.value() == 100.0);
};

CASE("Test fixCloudParam186") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    MarsRecord mars;
    MiscRecord misc;

    mars.param = Param{186};

    auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

    EXPECT(res);
    EXPECT(mars.param.value() == Param{3073});
    EXPECT(res->valuesScaleFactor);
    EXPECT(res->valuesScaleFactor.value() == 100.0);
};

CASE("Test fixCloudParam187") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    MarsRecord mars;
    MiscRecord misc;

    mars.param = Param{187};

    auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

    EXPECT(res);
    EXPECT(mars.param.value() == Param{3074});
    EXPECT(res->valuesScaleFactor);
    EXPECT(res->valuesScaleFactor.value() == 100.0);
};

CASE("Test fixCloudParam188") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    MarsRecord mars;
    MiscRecord misc;

    mars.param = Param{188};

    auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

    EXPECT(res);
    EXPECT(mars.param.value() == Param{3075});
    EXPECT(res->valuesScaleFactor);
    EXPECT(res->valuesScaleFactor.value() == 100.0);
};

CASE("Test fixConvectivePrecip143") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    MarsRecord mars;
    MiscRecord misc;

    mars.param = Param{143};

    auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

    EXPECT(res);
    EXPECT(mars.param.value() == Param{228143});
    EXPECT(res->valuesScaleFactor);
    EXPECT(res->valuesScaleFactor.value() == 1000.0);
};

CASE("Test fixTotalPrecip228") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    MarsRecord mars;
    MiscRecord misc;

    mars.param = Param{228};

    auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

    EXPECT(res);
    EXPECT(mars.param.value() == Param{228228});
    EXPECT(res->valuesScaleFactor);
    EXPECT(res->valuesScaleFactor.value() == 1000.0);
};


CASE("Test heightAboveGround2m") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    std::vector<std::int64_t> paramIds{{167, 168, 174096, 228037}};

    for (auto paramId : paramIds) {
        MarsRecord mars;
        MiscRecord misc;

        mars.param = Param{paramId};
        mars.levtype = LevType::SFC;
        mars.levelist = 0;

        auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

        EXPECT(res);
        EXPECT(mars.levelist.value() == 2);
    }
};

CASE("Test heightAboveGround10m") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    std::vector<std::int64_t> paramIds{{228029, 228131, 228132, 165, 166, 207}};

    for (auto paramId : paramIds) {
        MarsRecord mars;
        MiscRecord misc;

        mars.param = Param{paramId};
        mars.levtype = LevType::SFC;
        mars.levelist = 0;

        auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

        EXPECT(res);
        EXPECT(mars.levelist.value() == 10);
    }
};

CASE("Test heightAboveSea") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    std::vector<std::int64_t> paramIds{{140233, 140245, 140249}};

    for (auto paramId : paramIds) {
        MarsRecord mars;
        MiscRecord misc;

        mars.param = Param{paramId};

        auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

        EXPECT(res);
        EXPECT(mars.levelist.value() == 10);
        EXPECT(!misc.bitsPerValue.has_value());
    }
};


CASE("Test mapToSol") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    std::vector<std::int64_t> paramIds{{33, 238, 228038, 260360, 262000, 262024, 260199}};

    for (auto paramId : paramIds) {
        MarsRecord mars;
        MiscRecord misc;

        mars.param = Param{paramId};
        mars.levtype = LevType::SFC;
        mars.levelist = 123;

        auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

        EXPECT(res);
        EXPECT(mars.levtype.value() == LevType::SOL);
    }
};


CASE("Test fixRunOffWaterParam205") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;


    MarsRecord mars;
    MiscRecord misc;

    mars.param = Param{205};

    auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

    EXPECT(res);
    EXPECT(mars.param.value() == Param{231002});
    EXPECT(res->valuesScaleFactor);
    EXPECT(res->valuesScaleFactor.value() == 1000.0);
};

CASE("Test fixSnowfallWaterEquivParam") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    MarsRecord mars;
    MiscRecord misc;

    mars.param = Param{144};

    auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

    EXPECT(res);
    EXPECT(mars.param.value() == Param{228144});
    EXPECT(res->valuesScaleFactor);
    EXPECT(res->valuesScaleFactor.value() == 1000.0);
};

// -----------------------------------------------------------------------
// WMO unit mapping: Runoff and water cycle (m -> mm, scale 1000)
// -----------------------------------------------------------------------

CASE("Test fixSurfaceRunoff8") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    MarsRecord mars;
    MiscRecord misc;

    mars.param = Param{8};

    auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

    EXPECT(res);
    EXPECT(mars.param.value() == Param{231010});
    EXPECT(res->valuesScaleFactor);
    EXPECT(res->valuesScaleFactor.value() == 1000.0);
};

CASE("Test fixSubSurfaceRunoff9") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    MarsRecord mars;
    MiscRecord misc;

    mars.param = Param{9};

    auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

    EXPECT(res);
    EXPECT(mars.param.value() == Param{231012});
    EXPECT(res->valuesScaleFactor);
    EXPECT(res->valuesScaleFactor.value() == 1000.0);
};

CASE("Test fixSnowDepth141") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    MarsRecord mars;
    MiscRecord misc;

    mars.param = Param{141};

    auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

    EXPECT(res);
    EXPECT(mars.param.value() == Param{228141});
    EXPECT(res->valuesScaleFactor);
    EXPECT(res->valuesScaleFactor.value() == 1000.0);
};

CASE("Test fixLargeScalePrecip142") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    MarsRecord mars;
    MiscRecord misc;

    mars.param = Param{142};

    auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

    EXPECT(res);
    EXPECT(mars.param.value() == Param{3062});
    EXPECT(res->valuesScaleFactor);
    EXPECT(res->valuesScaleFactor.value() == 1000.0);
};

CASE("Test fixEvaporation182") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    MarsRecord mars;
    MiscRecord misc;

    mars.param = Param{182};

    auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

    EXPECT(res);
    EXPECT(mars.param.value() == Param{260259});
    EXPECT(res->valuesScaleFactor);
    EXPECT(res->valuesScaleFactor.value() == 1000.0);
};

CASE("Test fixSkinReservoirContent198") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    MarsRecord mars;
    MiscRecord misc;

    mars.param = Param{198};

    auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

    EXPECT(res);
    EXPECT(mars.param.value() == Param{160198});
    EXPECT(res->valuesScaleFactor);
    EXPECT(res->valuesScaleFactor.value() == 1000.0);
};

CASE("Test fixConvectiveSnowfall239") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    MarsRecord mars;
    MiscRecord misc;

    mars.param = Param{239};

    auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

    EXPECT(res);
    EXPECT(mars.param.value() == Param{231057});
    EXPECT(res->valuesScaleFactor);
    EXPECT(res->valuesScaleFactor.value() == 1000.0);
};

CASE("Test fixLargeScaleSnowfall240") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    MarsRecord mars;
    MiscRecord misc;

    mars.param = Param{240};

    auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

    EXPECT(res);
    EXPECT(mars.param.value() == Param{231058});
    EXPECT(res->valuesScaleFactor);
    EXPECT(res->valuesScaleFactor.value() == 1000.0);
};

CASE("Test fixFreezingRain228216") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    MarsRecord mars;
    MiscRecord misc;

    mars.param = Param{228216};

    auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

    EXPECT(res);
    EXPECT(mars.param.value() == Param{231001});
    EXPECT(res->valuesScaleFactor);
    EXPECT(res->valuesScaleFactor.value() == 1000.0);
};


// -----------------------------------------------------------------------
// WMO unit mapping: Albedo (fraction -> %, scale 100), required for ERA
// -----------------------------------------------------------------------

CASE("Test fixAlbedoUvDirect15") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    MarsRecord mars;
    MiscRecord misc;

    mars.param = Param{15};

    auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

    EXPECT(res);
    EXPECT(mars.param.value() == Param{210199});
    EXPECT(res->valuesScaleFactor);
    EXPECT(res->valuesScaleFactor.value() == 100.0);
};

CASE("Test fixAlbedoUvDiffuse16") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    MarsRecord mars;
    MiscRecord misc;

    mars.param = Param{16};

    auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

    EXPECT(res);
    EXPECT(mars.param.value() == Param{210198});
    EXPECT(res->valuesScaleFactor);
    EXPECT(res->valuesScaleFactor.value() == 100.0);
};

CASE("Test fixAlbedoNirDirect17") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    MarsRecord mars;
    MiscRecord misc;

    mars.param = Param{17};

    auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

    EXPECT(res);
    EXPECT(mars.param.value() == Param{210261});
    EXPECT(res->valuesScaleFactor);
    EXPECT(res->valuesScaleFactor.value() == 100.0);
};

CASE("Test fixAlbedoNirDiffuse18") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    MarsRecord mars;
    MiscRecord misc;

    mars.param = Param{18};

    auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

    EXPECT(res);
    EXPECT(mars.param.value() == Param{210260});
    EXPECT(res->valuesScaleFactor);
    EXPECT(res->valuesScaleFactor.value() == 100.0);
};

CASE("Test fixSnowAlbedo32") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    MarsRecord mars;
    MiscRecord misc;

    mars.param = Param{32};

    auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

    EXPECT(res);
    EXPECT(mars.param.value() == Param{228032});
    EXPECT(res->valuesScaleFactor);
    EXPECT(res->valuesScaleFactor.value() == 100.0);
};

CASE("Test fixForecastAlbedo243") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    MarsRecord mars;
    MiscRecord misc;

    mars.param = Param{243};

    auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

    EXPECT(res);
    EXPECT(mars.param.value() == Param{260509});
    EXPECT(res->valuesScaleFactor);
    EXPECT(res->valuesScaleFactor.value() == 100.0);
};

CASE("Test fixClimatologicalAlbedo174") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    FullMarsRecord mars;
    MiscRecord misc;

    mars.param.set(174);

    auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

    EXPECT(res);
    EXPECT(mars.param.get() == Param{260509});
    EXPECT(res->valuesScaleFactor);
    EXPECT(res->valuesScaleFactor.value() == 100.0);
};
CASE("Test fixAlbedoComponents") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    // UV direct: isotropic, volumetric, geometric
    // NIR direct: isotropic, volumetric, geometric
    struct AlbedoCase {
        std::int64_t paramIn;
        std::int64_t paramOut;
    };

    std::vector<AlbedoCase> cases{
        {210186, 210201},  // aluvpi -> aluvpi_p
        {210187, 210202},  // aluvpv -> aluvpv_p
        {210188, 210200},  // aluvpg -> aluvpg_p
        {210189, 210263},  // alnipi -> alnipi_p
        {210190, 210264},  // alnipv -> alnipv_p
        {210191, 210262},  // alnipg -> alnipg_p
    };

    for (const auto& tc : cases) {
        MarsRecord mars;
        MiscRecord misc;

        mars.param = Param{tc.paramIn};

        auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

        EXPECT(res);
        EXPECT(mars.param.value() == Param{tc.paramOut});
        EXPECT(res->valuesScaleFactor);
        EXPECT(res->valuesScaleFactor.value() == 100.0);
    }
};


// -----------------------------------------------------------------------
// WMO unit mapping: ERA other water-cycle parameters
// -----------------------------------------------------------------------

CASE("Test fixSnowEvaporation44") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    MarsRecord mars;
    MiscRecord misc;

    mars.param = Param{44};

    auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

    EXPECT(res);
    EXPECT(mars.param.value() == Param{231003});
    EXPECT(res->valuesScaleFactor);
    EXPECT(res->valuesScaleFactor.value() == 1000.0);
};

CASE("Test fixSnowmelt45") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    MarsRecord mars;
    MiscRecord misc;

    mars.param = Param{45};

    auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

    EXPECT(res);
    EXPECT(mars.param.value() == Param{3099});
    EXPECT(res->valuesScaleFactor);
    EXPECT(res->valuesScaleFactor.value() == 1000.0);
};

CASE("Test fixTotalColumnOzone206") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    MarsRecord mars;
    MiscRecord misc;

    mars.param = Param{206};

    auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

    EXPECT(res);
    EXPECT(mars.param.value() == Param{260132});
    EXPECT(res->valuesScaleFactor);
    EXPECT(res->valuesScaleFactor.value() == 46698.05);
};

CASE("Test fixPotentialEvaporation228251") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    MarsRecord mars;
    MiscRecord misc;

    mars.param = Param{228251};

    auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

    EXPECT(res);
    EXPECT(mars.param.value() == Param{231005});
    EXPECT(res->valuesScaleFactor);
    EXPECT(res->valuesScaleFactor.value() == 1000.0);
};


// -----------------------------------------------------------------------
// WMO unit mapping: Time-mean rate parameters (m/s -> mm/s, scale 1000)
// -----------------------------------------------------------------------

CASE("Test fixTimeMeanRateParams") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    struct TimeMeanCase {
        std::int64_t paramIn;
        std::int64_t paramOut;
    };

    std::vector<TimeMeanCase> cases{
        {172008, 235020},  // msror   -> avg_surfror
        {172009, 235021},  // mssror  -> avg_ssurfror
        {172044, 235023},  // esrate  -> avg_esrwe
        {172045, 235024},  //         -> avg_smr (snowmelt rate)
        {172142, 235029},  // mlsprt  -> avg_lsprate
        {172143, 235030},  // cprate  -> avg_cpr
        {172144, 235031},  // mtsfr   -> avg_tsrwe
        {172182, 235043},  // erate   -> avg_ie
        {172205, 235048},  // mrort   -> avg_rorwe
        {172228, 235055},  // tprate  -> avg_tprate
        {235141, 235078},  // avg_sd_m -> avg_sd
    };

    for (const auto& tc : cases) {
        MarsRecord mars;
        MiscRecord misc;

        mars.param = Param{tc.paramIn};

        auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

        EXPECT(res);
        EXPECT(mars.param.value() == Param{tc.paramOut});
        EXPECT(res->valuesScaleFactor);
        EXPECT(res->valuesScaleFactor.value() == 1000.0);
    }
};


// -----------------------------------------------------------------------
// WMO unit mapping: Time-mean cloud cover and albedo (fraction -> %, scale 100)
// New in ecCodes 2.38
// -----------------------------------------------------------------------

CASE("Test fixTimeMeanCloudCoverAndAlbedo") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    struct TimeMeanCloudCase {
        std::int64_t paramIn;
        std::int64_t paramOut;
    };

    std::vector<TimeMeanCloudCase> cases{
        {235186, 235108},  // avg_lcc_frac -> avg_lcc
        {235187, 235109},  // avg_mcc_frac -> avg_mcc
        {235188, 235110},  // avg_hcc_frac -> avg_hcc
        {235243, 235263},  // avg_fal_frac -> avg_al
    };

    for (const auto& tc : cases) {
        MarsRecord mars;
        MiscRecord misc;

        mars.param = Param{tc.paramIn};

        auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

        EXPECT(res);
        EXPECT(mars.param.value() == Param{tc.paramOut});
        EXPECT(res->valuesScaleFactor);
        EXPECT(res->valuesScaleFactor.value() == 100.0);
    }
};


CASE("Test fixTimespanMax2T") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    std::vector<std::int64_t> paramIds{{121, 228026, 201}};

    for (auto paramId : paramIds) {
        MarsRecord mars;
        MiscRecord misc;

        mars.param = Param{paramId};

        auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

        EXPECT(res);
        EXPECT(mars.param.value() == Param{237167});
    }
};

CASE("Test fixTimespanMaxCape") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;


    MarsRecord mars;
    MiscRecord misc;

    mars.param = Param{228035};

    auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

    EXPECT(res);
    EXPECT(mars.param.value() == Param{237117});
};

CASE("Test fixTimespanMaxMuCapes") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    MarsRecord mars;
    MiscRecord misc;

    mars.param = Param{228036};

    auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

    EXPECT(res);
    EXPECT(mars.param.value() == Param{237321});
};

CASE("Test fixTimespanMaxPrecipRate") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    std::vector<std::int64_t> paramIds{{228222, 228224, 228226}};

    for (auto paramId : paramIds) {
        MarsRecord mars;
        MiscRecord misc;

        mars.param = Param{paramId};

        auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

        EXPECT(res);
        EXPECT(mars.param.value() == Param{237055});
    }
};

CASE("Test fixTimespanMaxWindGust") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    std::vector<std::int64_t> paramIds{{123, 228028, 49}};

    for (auto paramId : paramIds) {
        MarsRecord mars;
        MiscRecord misc;

        mars.param = Param{paramId};

        auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

        EXPECT(res);
        EXPECT(mars.param.value() == Param{237318});
    }
};


CASE("Test fixTimespanMeanFlashDensity") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    std::vector<std::int64_t> paramIds{{228051, 228057, 228058}};

    for (auto paramId : paramIds) {
        MarsRecord mars;
        MiscRecord misc;

        mars.param = Param{paramId};

        auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

        EXPECT(res);
        EXPECT(mars.param.value() == Param{235326});
    }
};


CASE("Test fixTimespanMin2T") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    std::vector<std::int64_t> paramIds{{122, 228027, 202}};

    for (auto paramId : paramIds) {
        MarsRecord mars;
        MiscRecord misc;

        mars.param = Param{paramId};

        auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

        EXPECT(res);
        EXPECT(mars.param.value() == Param{238167});
    }
};


CASE("Test fixTimespanMinPrecipRate") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    std::vector<std::int64_t> paramIds{{228223, 228225, 228227}};

    for (auto paramId : paramIds) {
        MarsRecord mars;
        MiscRecord misc;

        mars.param = Param{paramId};

        auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

        EXPECT(res);
        EXPECT(mars.param.value() == Param{238055});
    }
};


CASE("Test fixTimespanModePrecip") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    std::vector<std::int64_t> paramIds{{260320, 260321, 260339}};

    for (auto paramId : paramIds) {
        MarsRecord mars;
        MiscRecord misc;

        mars.param = Param{paramId};

        auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

        EXPECT(res);
        EXPECT(mars.param.value() == Param{260683});
    }
};


CASE("Test fixTimespanSeverityPrecip") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    std::vector<std::int64_t> paramIds{{260318, 260319, 260338}};

    for (auto paramId : paramIds) {
        MarsRecord mars;
        MiscRecord misc;

        mars.param = Param{paramId};

        auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

        EXPECT(res);
        EXPECT(mars.param.value() == Param{260682});
    }
};


CASE("Test fixWindspeedU100m") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    MarsRecord mars;
    MiscRecord misc;

    mars.param = Param{228246};
    mars.levtype = LevType::SFC;

    auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

    EXPECT(res);
    EXPECT(mars.levtype.value() == LevType::HL);
    EXPECT(mars.levelist.value() == 100);
    EXPECT(mars.param.value() == Param{131});
};

CASE("Test fixWindspeedU200m") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    MarsRecord mars;
    MiscRecord misc;

    mars.param = Param{228239};
    mars.levtype = LevType::SFC;

    auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

    EXPECT(res);
    EXPECT(mars.levtype.value() == LevType::HL);
    EXPECT(mars.levelist.value() == 200);
    EXPECT(mars.param.value() == Param{131});
};

CASE("Test fixWindspeedV100m") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    MarsRecord mars;
    MiscRecord misc;

    mars.param = Param{228247};
    mars.levtype = LevType::SFC;

    auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

    EXPECT(res);
    EXPECT(mars.levtype.value() == LevType::HL);
    EXPECT(mars.levelist.value() == 100);
    EXPECT(mars.param.value() == Param{132});
};


CASE("Test fixWindspeedV200m") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    MarsRecord mars;
    MiscRecord misc;

    mars.param = Param{228240};
    mars.levtype = LevType::SFC;

    auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

    EXPECT(res);
    EXPECT(mars.levtype.value() == LevType::HL);
    EXPECT(mars.levelist.value() == 200);
    EXPECT(mars.param.value() == Param{132});
};


CASE("Test fixWindspeed100m") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    MarsRecord mars;
    MiscRecord misc;

    mars.param = Param{228249};
    mars.levtype = LevType::SFC;

    auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

    EXPECT(res);
    EXPECT(mars.levtype.value() == LevType::HL);
    EXPECT(mars.levelist.value() == 100);
    EXPECT(mars.param.value() == Param{10});
};


CASE("Test fixWindspeed200m") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    MarsRecord mars;
    MiscRecord misc;

    mars.param = Param{228241};
    mars.levtype = LevType::SFC;

    auto res = mars2mars::applyMappings(mars2mars::allRules(), mars, misc);

    EXPECT(res);
    EXPECT(mars.levtype.value() == LevType::HL);
    EXPECT(mars.levelist.value() == 200);
    EXPECT(mars.param.value() == Param{10});
};


CASE("Test ruleWaveBitsPerValueS1") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    MarsRecord mars;
    MiscRecord misc;

    mars.param = Param{22824};
    mars.step = 123;

    auto res = mars2mars::applyMappings(mars2mars::mapBitsPerValue(), mars, misc);

    EXPECT(res);
    EXPECT(misc.bitsPerValue.has_value());
    EXPECT(misc.bitsPerValue.value() == 24);
};

CASE("Test ruleWaveBitsPerValue") {
    using namespace multio::mars2mars::rules;
    using namespace multio::mars2mars;
    using namespace multio::datamod;

    std::vector<std::int64_t> paramIds;

    for (std::int64_t p = 140098; p <= 140105; ++p) {
        paramIds.push_back(p);
    }
    for (std::int64_t p = 140112; p <= 140129; ++p) {
        paramIds.push_back(p);
    }
    for (std::int64_t p = 140207; p <= 140209; ++p) {
        paramIds.push_back(p);
    }
    for (std::int64_t p = 140211; p <= 140212; ++p) {
        paramIds.push_back(p);
    }
    for (std::int64_t p = 140214; p <= 140232; ++p) {
        paramIds.push_back(p);
    }
    for (std::int64_t p = 140234; p <= 140239; ++p) {
        paramIds.push_back(p);
    }
    paramIds.push_back(140244);
    for (std::int64_t p = 140251; p <= 140254; ++p) {
        paramIds.push_back(p);
    }


    for (auto paramId : paramIds) {
        MarsRecord mars;
        MiscRecord misc;

        mars.param = Param{paramId};

        auto res = mars2mars::applyMappings(mars2mars::mapBitsPerValue(), mars, misc);

        EXPECT(res);
        EXPECT(misc.bitsPerValue.has_value());
        EXPECT(misc.bitsPerValue.value() == 24);
    }
};

}  // namespace multio::test

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

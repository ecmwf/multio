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

#include "multio/datamod/MarsMiscGeo.h"

namespace multio::test {

CASE("Test fixParam35ToSol1") {
    using namespace multio::mars2grib::rules;
    using namespace multio::mars2grib;
    using namespace multio::datamod;

    MarsKeyValueSet mars;
    MiscKeyValueSet misc;

    key<MarsKeys::PARAM>(mars).set(35);
    key<MarsKeys::LEVTYPE>(mars).set(LevType::SFC);

    auto res = mars2mars::applyMappings(mars, misc);

    EXPECT(res);
    EXPECT(key<MarsKeys::PARAM>(mars).get() == 262024);
    EXPECT(key<MarsKeys::LEVELIST>(mars).has());
    EXPECT(key<MarsKeys::LEVELIST>(mars).get() == 1);
    EXPECT(key<MarsKeys::LEVTYPE>(mars).get() == LevType::SOL);
};


CASE("Test fixParam36ToSol2") {
    using namespace multio::mars2grib::rules;
    using namespace multio::mars2grib;
    using namespace multio::datamod;

    MarsKeyValueSet mars;
    MiscKeyValueSet misc;

    key<MarsKeys::PARAM>(mars).set(36);
    key<MarsKeys::LEVTYPE>(mars).set(LevType::SFC);

    auto res = mars2mars::applyMappings(mars, misc);

    EXPECT(res);
    EXPECT(key<MarsKeys::PARAM>(mars).get() == 262024);
    EXPECT(key<MarsKeys::LEVELIST>(mars).has());
    EXPECT(key<MarsKeys::LEVELIST>(mars).get() == 2);
    EXPECT(key<MarsKeys::LEVTYPE>(mars).get() == LevType::SOL);
};


CASE("Test fixParam37ToSol3") {
    using namespace multio::mars2grib::rules;
    using namespace multio::mars2grib;
    using namespace multio::datamod;

    MarsKeyValueSet mars;
    MiscKeyValueSet misc;

    key<MarsKeys::PARAM>(mars).set(37);
    key<MarsKeys::LEVTYPE>(mars).set(LevType::SFC);

    auto res = mars2mars::applyMappings(mars, misc);

    EXPECT(res);
    EXPECT(key<MarsKeys::PARAM>(mars).get() == 262024);
    EXPECT(key<MarsKeys::LEVELIST>(mars).has());
    EXPECT(key<MarsKeys::LEVELIST>(mars).get() == 3);
    EXPECT(key<MarsKeys::LEVTYPE>(mars).get() == LevType::SOL);
};

CASE("Test fixParam38ToSol4") {
    using namespace multio::mars2grib::rules;
    using namespace multio::mars2grib;
    using namespace multio::datamod;

    MarsKeyValueSet mars;
    MiscKeyValueSet misc;

    key<MarsKeys::PARAM>(mars).set(38);
    key<MarsKeys::LEVTYPE>(mars).set(LevType::SFC);

    auto res = mars2mars::applyMappings(mars, misc);

    EXPECT(res);
    EXPECT(key<MarsKeys::PARAM>(mars).get() == 262024);
    EXPECT(key<MarsKeys::LEVELIST>(mars).has());
    EXPECT(key<MarsKeys::LEVELIST>(mars).get() == 4);
    EXPECT(key<MarsKeys::LEVTYPE>(mars).get() == LevType::SOL);
};


CASE("Test fixParam39ToSol1") {
    using namespace multio::mars2grib::rules;
    using namespace multio::mars2grib;
    using namespace multio::datamod;

    MarsKeyValueSet mars;
    MiscKeyValueSet misc;

    key<MarsKeys::PARAM>(mars).set(39);
    key<MarsKeys::LEVTYPE>(mars).set(LevType::SFC);

    auto res = mars2mars::applyMappings(mars, misc);

    EXPECT(res);
    EXPECT(key<MarsKeys::PARAM>(mars).get() == 262199);
    EXPECT(key<MarsKeys::LEVELIST>(mars).has());
    EXPECT(key<MarsKeys::LEVELIST>(mars).get() == 1);
    EXPECT(key<MarsKeys::LEVTYPE>(mars).get() == LevType::SOL);
};


CASE("Test fixParam40ToSol2") {
    using namespace multio::mars2grib::rules;
    using namespace multio::mars2grib;
    using namespace multio::datamod;

    MarsKeyValueSet mars;
    MiscKeyValueSet misc;

    key<MarsKeys::PARAM>(mars).set(40);
    key<MarsKeys::LEVTYPE>(mars).set(LevType::SFC);

    auto res = mars2mars::applyMappings(mars, misc);

    EXPECT(res);
    EXPECT(key<MarsKeys::PARAM>(mars).get() == 262199);
    EXPECT(key<MarsKeys::LEVELIST>(mars).has());
    EXPECT(key<MarsKeys::LEVELIST>(mars).get() == 2);
    EXPECT(key<MarsKeys::LEVTYPE>(mars).get() == LevType::SOL);
};

CASE("Test fixParam41ToSol3") {
    using namespace multio::mars2grib::rules;
    using namespace multio::mars2grib;
    using namespace multio::datamod;

    MarsKeyValueSet mars;
    MiscKeyValueSet misc;

    key<MarsKeys::PARAM>(mars).set(41);
    key<MarsKeys::LEVTYPE>(mars).set(LevType::SFC);

    auto res = mars2mars::applyMappings(mars, misc);

    EXPECT(res);
    EXPECT(key<MarsKeys::PARAM>(mars).get() == 262199);
    EXPECT(key<MarsKeys::LEVELIST>(mars).has());
    EXPECT(key<MarsKeys::LEVELIST>(mars).get() == 3);
    EXPECT(key<MarsKeys::LEVTYPE>(mars).get() == LevType::SOL);
};

CASE("Test fixParam42ToSol4") {
    using namespace multio::mars2grib::rules;
    using namespace multio::mars2grib;
    using namespace multio::datamod;

    MarsKeyValueSet mars;
    MiscKeyValueSet misc;

    key<MarsKeys::PARAM>(mars).set(42);
    key<MarsKeys::LEVTYPE>(mars).set(LevType::SFC);

    auto res = mars2mars::applyMappings(mars, misc);

    EXPECT(res);
    EXPECT(key<MarsKeys::PARAM>(mars).get() == 262199);
    EXPECT(key<MarsKeys::LEVELIST>(mars).has());
    EXPECT(key<MarsKeys::LEVELIST>(mars).get() == 4);
    EXPECT(key<MarsKeys::LEVTYPE>(mars).get() == LevType::SOL);
};


CASE("Test fixParam139ToSol1") {
    using namespace multio::mars2grib::rules;
    using namespace multio::mars2grib;
    using namespace multio::datamod;

    MarsKeyValueSet mars;
    MiscKeyValueSet misc;

    key<MarsKeys::PARAM>(mars).set(139);
    key<MarsKeys::LEVTYPE>(mars).set(LevType::SFC);

    auto res = mars2mars::applyMappings(mars, misc);

    EXPECT(res);
    EXPECT(key<MarsKeys::PARAM>(mars).get() == 262360);
    EXPECT(key<MarsKeys::LEVELIST>(mars).has());
    EXPECT(key<MarsKeys::LEVELIST>(mars).get() == 1);
    EXPECT(key<MarsKeys::LEVTYPE>(mars).get() == LevType::SOL);
};

CASE("Test fixParam170ToSol2") {
    using namespace multio::mars2grib::rules;
    using namespace multio::mars2grib;
    using namespace multio::datamod;

    MarsKeyValueSet mars;
    MiscKeyValueSet misc;

    key<MarsKeys::PARAM>(mars).set(170);
    key<MarsKeys::LEVTYPE>(mars).set(LevType::SFC);

    auto res = mars2mars::applyMappings(mars, misc);

    EXPECT(res);
    EXPECT(key<MarsKeys::PARAM>(mars).get() == 262360);
    EXPECT(key<MarsKeys::LEVELIST>(mars).has());
    EXPECT(key<MarsKeys::LEVELIST>(mars).get() == 2);
    EXPECT(key<MarsKeys::LEVTYPE>(mars).get() == LevType::SOL);
};

CASE("Test fixParam183ToSol3") {
    using namespace multio::mars2grib::rules;
    using namespace multio::mars2grib;
    using namespace multio::datamod;

    MarsKeyValueSet mars;
    MiscKeyValueSet misc;

    key<MarsKeys::PARAM>(mars).set(183);
    key<MarsKeys::LEVTYPE>(mars).set(LevType::SFC);

    auto res = mars2mars::applyMappings(mars, misc);

    EXPECT(res);
    EXPECT(key<MarsKeys::PARAM>(mars).get() == 262360);
    EXPECT(key<MarsKeys::LEVELIST>(mars).has());
    EXPECT(key<MarsKeys::LEVELIST>(mars).get() == 3);
    EXPECT(key<MarsKeys::LEVTYPE>(mars).get() == LevType::SOL);
};


CASE("Test fixParam263ToSol4") {
    using namespace multio::mars2grib::rules;
    using namespace multio::mars2grib;
    using namespace multio::datamod;

    MarsKeyValueSet mars;
    MiscKeyValueSet misc;

    key<MarsKeys::PARAM>(mars).set(236);
    key<MarsKeys::LEVTYPE>(mars).set(LevType::SFC);

    auto res = mars2mars::applyMappings(mars, misc);

    EXPECT(res);
    EXPECT(key<MarsKeys::PARAM>(mars).get() == 262360);
    EXPECT(key<MarsKeys::LEVELIST>(mars).has());
    EXPECT(key<MarsKeys::LEVELIST>(mars).get() == 4);
    EXPECT(key<MarsKeys::LEVTYPE>(mars).get() == LevType::SOL);
};


CASE("Test fixTableVersion") {
    using namespace multio::mars2grib::rules;
    using namespace multio::mars2grib;
    using namespace multio::datamod;

    std::vector<std::int64_t> paramIds{
        {228080, 228081, 228082, 228083, 228084, 228085, 233032, 233033, 233034, 233035, 235062, 235063, 235064}};

    for (auto paramId : paramIds) {
        MarsKeyValueSet mars;
        MiscKeyValueSet misc;

        key<MarsKeys::PARAM>(mars).set(paramId);

        auto res = mars2mars::applyMappings(mars, misc);

        EXPECT(res);
        EXPECT(key<MiscKeys::TablesVersion>(misc).has());
        EXPECT(key<MiscKeys::TablesVersion>(misc).get() == 30);
    }
};


CASE("Test fixCloudParam164") {
    using namespace multio::mars2grib::rules;
    using namespace multio::mars2grib;
    using namespace multio::datamod;

    MarsKeyValueSet mars;
    MiscKeyValueSet misc;

    key<MarsKeys::PARAM>(mars).set(164);

    auto res = mars2mars::applyMappings(mars, misc);

    EXPECT(res);
    EXPECT(key<MarsKeys::PARAM>(mars).get() == 228164);
    EXPECT(res->valuesScaleFactor);
    EXPECT(res->valuesScaleFactor.value() == 100.0);
};

CASE("Test fixCloudParam186") {
    using namespace multio::mars2grib::rules;
    using namespace multio::mars2grib;
    using namespace multio::datamod;

    MarsKeyValueSet mars;
    MiscKeyValueSet misc;

    key<MarsKeys::PARAM>(mars).set(186);

    auto res = mars2mars::applyMappings(mars, misc);

    EXPECT(res);
    EXPECT(key<MarsKeys::PARAM>(mars).get() == 3073);
    EXPECT(res->valuesScaleFactor);
    EXPECT(res->valuesScaleFactor.value() == 100.0);
};

CASE("Test fixCloudParam187") {
    using namespace multio::mars2grib::rules;
    using namespace multio::mars2grib;
    using namespace multio::datamod;

    MarsKeyValueSet mars;
    MiscKeyValueSet misc;

    key<MarsKeys::PARAM>(mars).set(187);

    auto res = mars2mars::applyMappings(mars, misc);

    EXPECT(res);
    EXPECT(key<MarsKeys::PARAM>(mars).get() == 3074);
    EXPECT(res->valuesScaleFactor);
    EXPECT(res->valuesScaleFactor.value() == 100.0);
};

CASE("Test fixCloudParam188") {
    using namespace multio::mars2grib::rules;
    using namespace multio::mars2grib;
    using namespace multio::datamod;

    MarsKeyValueSet mars;
    MiscKeyValueSet misc;

    key<MarsKeys::PARAM>(mars).set(188);

    auto res = mars2mars::applyMappings(mars, misc);

    EXPECT(res);
    EXPECT(key<MarsKeys::PARAM>(mars).get() == 3075);
    EXPECT(res->valuesScaleFactor);
    EXPECT(res->valuesScaleFactor.value() == 100.0);
};

CASE("Test fixConvectivePrecip143") {
    using namespace multio::mars2grib::rules;
    using namespace multio::mars2grib;
    using namespace multio::datamod;

    MarsKeyValueSet mars;
    MiscKeyValueSet misc;

    key<MarsKeys::PARAM>(mars).set(143);

    auto res = mars2mars::applyMappings(mars, misc);

    EXPECT(res);
    EXPECT(key<MarsKeys::PARAM>(mars).get() == 228143);
    EXPECT(res->valuesScaleFactor);
    EXPECT(res->valuesScaleFactor.value() == 1000.0);
};

CASE("Test fixTotalPrecip228") {
    using namespace multio::mars2grib::rules;
    using namespace multio::mars2grib;
    using namespace multio::datamod;

    MarsKeyValueSet mars;
    MiscKeyValueSet misc;

    key<MarsKeys::PARAM>(mars).set(228);

    auto res = mars2mars::applyMappings(mars, misc);

    EXPECT(res);
    EXPECT(key<MarsKeys::PARAM>(mars).get() == 228228);
    EXPECT(res->valuesScaleFactor);
    EXPECT(res->valuesScaleFactor.value() == 1000.0);
};


CASE("Test heightAboveGround2m") {
    using namespace multio::mars2grib::rules;
    using namespace multio::mars2grib;
    using namespace multio::datamod;

    std::vector<std::int64_t> paramIds{{167, 168, 174096, 228037}};

    for (auto paramId : paramIds) {
        MarsKeyValueSet mars;
        MiscKeyValueSet misc;

        key<MarsKeys::PARAM>(mars).set(paramId);
        key<MarsKeys::LEVTYPE>(mars).set(LevType::SFC);
        key<MarsKeys::LEVELIST>(mars).set(0);

        auto res = mars2mars::applyMappings(mars, misc);

        EXPECT(res);
        EXPECT(key<MarsKeys::LEVELIST>(mars).get() == 2);
    }
};

CASE("Test heightAboveGround10m") {
    using namespace multio::mars2grib::rules;
    using namespace multio::mars2grib;
    using namespace multio::datamod;

    std::vector<std::int64_t> paramIds{{228029, 228131, 228132, 165, 166, 207}};

    for (auto paramId : paramIds) {
        MarsKeyValueSet mars;
        MiscKeyValueSet misc;

        key<MarsKeys::PARAM>(mars).set(paramId);
        key<MarsKeys::LEVTYPE>(mars).set(LevType::SFC);
        key<MarsKeys::LEVELIST>(mars).set(0);

        auto res = mars2mars::applyMappings(mars, misc);

        EXPECT(res);
        EXPECT(key<MarsKeys::LEVELIST>(mars).get() == 10);
    }
};

CASE("Test heightAboveSea") {
    using namespace multio::mars2grib::rules;
    using namespace multio::mars2grib;
    using namespace multio::datamod;

    std::vector<std::int64_t> paramIds{{140233, 140245, 140249}};

    for (auto paramId : paramIds) {
        MarsKeyValueSet mars;
        MiscKeyValueSet misc;

        key<MarsKeys::PARAM>(mars).set(paramId);

        auto res = mars2mars::applyMappings(mars, misc);

        EXPECT(res);
        EXPECT(key<MarsKeys::LEVELIST>(mars).get() == 10);
        EXPECT(key<MiscKeys::BitsPerValue>(misc).has());
        EXPECT(key<MiscKeys::BitsPerValue>(misc).get() == 24);
    }
};


CASE("Test mapToSol") {
    using namespace multio::mars2grib::rules;
    using namespace multio::mars2grib;
    using namespace multio::datamod;

    std::vector<std::int64_t> paramIds{{33, 238, 228038, 228141, 260360, 262000, 262024, 260199}};

    for (auto paramId : paramIds) {
        MarsKeyValueSet mars;
        MiscKeyValueSet misc;

        key<MarsKeys::PARAM>(mars).set(paramId);
        key<MarsKeys::LEVTYPE>(mars).set(LevType::SFC);
        key<MarsKeys::LEVELIST>(mars).set(123);

        auto res = mars2mars::applyMappings(mars, misc);

        EXPECT(res);
        EXPECT(key<MarsKeys::LEVTYPE>(mars).get() == LevType::SOL);
    }
};


CASE("Test fixRunOffWaterParam205") {
    using namespace multio::mars2grib::rules;
    using namespace multio::mars2grib;
    using namespace multio::datamod;


    MarsKeyValueSet mars;
    MiscKeyValueSet misc;

    key<MarsKeys::PARAM>(mars).set(205);

    auto res = mars2mars::applyMappings(mars, misc);

    EXPECT(res);
    EXPECT(key<MarsKeys::PARAM>(mars).get() == 231002);
    EXPECT(res->valuesScaleFactor);
    EXPECT(res->valuesScaleFactor.value() == 1000.0);
};

CASE("Test fixSnowfallWaterEquivParam") {
    using namespace multio::mars2grib::rules;
    using namespace multio::mars2grib;
    using namespace multio::datamod;

    MarsKeyValueSet mars;
    MiscKeyValueSet misc;

    key<MarsKeys::PARAM>(mars).set(144);

    auto res = mars2mars::applyMappings(mars, misc);

    EXPECT(res);
    EXPECT(key<MarsKeys::PARAM>(mars).get() == 228114);
    EXPECT(res->valuesScaleFactor);
    EXPECT(res->valuesScaleFactor.value() == 1000.0);
};


CASE("Test fixTimespanMax2T") {
    using namespace multio::mars2grib::rules;
    using namespace multio::mars2grib;
    using namespace multio::datamod;

    std::vector<std::int64_t> paramIds{{122, 228026, 201}};

    for (auto paramId : paramIds) {
        MarsKeyValueSet mars;
        MiscKeyValueSet misc;

        key<MarsKeys::PARAM>(mars).set(paramId);

        auto res = mars2mars::applyMappings(mars, misc);

        EXPECT(res);
        EXPECT(key<MarsKeys::PARAM>(mars).get() == 237167);
    }
};

CASE("Test fixTimespanMaxCape") {
    using namespace multio::mars2grib::rules;
    using namespace multio::mars2grib;
    using namespace multio::datamod;


    MarsKeyValueSet mars;
    MiscKeyValueSet misc;

    key<MarsKeys::PARAM>(mars).set(228035);

    auto res = mars2mars::applyMappings(mars, misc);

    EXPECT(res);
    EXPECT(key<MarsKeys::PARAM>(mars).get() == 237117);
};

CASE("Test fixTimespanMaxMuCapes") {
    using namespace multio::mars2grib::rules;
    using namespace multio::mars2grib;
    using namespace multio::datamod;

    MarsKeyValueSet mars;
    MiscKeyValueSet misc;

    key<MarsKeys::PARAM>(mars).set(228036);

    auto res = mars2mars::applyMappings(mars, misc);

    EXPECT(res);
    EXPECT(key<MarsKeys::PARAM>(mars).get() == 237321);
};

CASE("Test fixTimespanMaxPrecipRate") {
    using namespace multio::mars2grib::rules;
    using namespace multio::mars2grib;
    using namespace multio::datamod;

    std::vector<std::int64_t> paramIds{{228222, 228224, 228226}};

    for (auto paramId : paramIds) {
        MarsKeyValueSet mars;
        MiscKeyValueSet misc;

        key<MarsKeys::PARAM>(mars).set(paramId);

        auto res = mars2mars::applyMappings(mars, misc);

        EXPECT(res);
        EXPECT(key<MarsKeys::PARAM>(mars).get() == 237055);
    }
};

CASE("Test fixTimespanMaxWindGust") {
    using namespace multio::mars2grib::rules;
    using namespace multio::mars2grib;
    using namespace multio::datamod;

    std::vector<std::int64_t> paramIds{{123, 228028, 49}};

    for (auto paramId : paramIds) {
        MarsKeyValueSet mars;
        MiscKeyValueSet misc;

        key<MarsKeys::PARAM>(mars).set(paramId);

        auto res = mars2mars::applyMappings(mars, misc);

        EXPECT(res);
        EXPECT(key<MarsKeys::PARAM>(mars).get() == 237318);
    }
};


CASE("Test fixTimespanMeanFlashDensity") {
    using namespace multio::mars2grib::rules;
    using namespace multio::mars2grib;
    using namespace multio::datamod;

    std::vector<std::int64_t> paramIds{{228051, 228057, 228058}};

    for (auto paramId : paramIds) {
        MarsKeyValueSet mars;
        MiscKeyValueSet misc;

        key<MarsKeys::PARAM>(mars).set(paramId);

        auto res = mars2mars::applyMappings(mars, misc);

        EXPECT(res);
        EXPECT(key<MarsKeys::PARAM>(mars).get() == 235326);
    }
};


CASE("Test fixTimespanMin2T") {
    using namespace multio::mars2grib::rules;
    using namespace multio::mars2grib;
    using namespace multio::datamod;

    std::vector<std::int64_t> paramIds{{121, 228027, 202}};

    for (auto paramId : paramIds) {
        MarsKeyValueSet mars;
        MiscKeyValueSet misc;

        key<MarsKeys::PARAM>(mars).set(paramId);

        auto res = mars2mars::applyMappings(mars, misc);

        EXPECT(res);
        EXPECT(key<MarsKeys::PARAM>(mars).get() == 238167);
    }
};


CASE("Test fixTimespanMinPrecipRate") {
    using namespace multio::mars2grib::rules;
    using namespace multio::mars2grib;
    using namespace multio::datamod;

    std::vector<std::int64_t> paramIds{{228223, 228225, 228227}};

    for (auto paramId : paramIds) {
        MarsKeyValueSet mars;
        MiscKeyValueSet misc;

        key<MarsKeys::PARAM>(mars).set(paramId);

        auto res = mars2mars::applyMappings(mars, misc);

        EXPECT(res);
        EXPECT(key<MarsKeys::PARAM>(mars).get() == 238055);
    }
};


CASE("Test fixTimespanModePrecip") {
    using namespace multio::mars2grib::rules;
    using namespace multio::mars2grib;
    using namespace multio::datamod;

    std::vector<std::int64_t> paramIds{{260320, 260321, 260339}};

    for (auto paramId : paramIds) {
        MarsKeyValueSet mars;
        MiscKeyValueSet misc;

        key<MarsKeys::PARAM>(mars).set(paramId);

        auto res = mars2mars::applyMappings(mars, misc);

        EXPECT(res);
        EXPECT(key<MarsKeys::PARAM>(mars).get() == 260683);
    }
};


CASE("Test fixTimespanSeverityPrecip") {
    using namespace multio::mars2grib::rules;
    using namespace multio::mars2grib;
    using namespace multio::datamod;

    std::vector<std::int64_t> paramIds{{260318, 260319, 260338}};

    for (auto paramId : paramIds) {
        MarsKeyValueSet mars;
        MiscKeyValueSet misc;

        key<MarsKeys::PARAM>(mars).set(paramId);

        auto res = mars2mars::applyMappings(mars, misc);

        EXPECT(res);
        EXPECT(key<MarsKeys::PARAM>(mars).get() == 260682);
    }
};


CASE("Test fixWindspeedU100m") {
    using namespace multio::mars2grib::rules;
    using namespace multio::mars2grib;
    using namespace multio::datamod;

    MarsKeyValueSet mars;
    MiscKeyValueSet misc;

    key<MarsKeys::PARAM>(mars).set(228246);
    key<MarsKeys::LEVTYPE>(mars).set(LevType::SFC);

    auto res = mars2mars::applyMappings(mars, misc);

    EXPECT(res);
    EXPECT(key<MarsKeys::LEVTYPE>(mars).get() == LevType::HL);
    EXPECT(key<MarsKeys::LEVELIST>(mars).get() == 100);
    EXPECT(key<MarsKeys::PARAM>(mars).get() == 131);
};

CASE("Test fixWindspeedU200m") {
    using namespace multio::mars2grib::rules;
    using namespace multio::mars2grib;
    using namespace multio::datamod;

    MarsKeyValueSet mars;
    MiscKeyValueSet misc;

    key<MarsKeys::PARAM>(mars).set(228239);
    key<MarsKeys::LEVTYPE>(mars).set(LevType::SFC);

    auto res = mars2mars::applyMappings(mars, misc);

    EXPECT(res);
    EXPECT(key<MarsKeys::LEVTYPE>(mars).get() == LevType::HL);
    EXPECT(key<MarsKeys::LEVELIST>(mars).get() == 200);
    EXPECT(key<MarsKeys::PARAM>(mars).get() == 131);
};

CASE("Test fixWindspeedV100m") {
    using namespace multio::mars2grib::rules;
    using namespace multio::mars2grib;
    using namespace multio::datamod;

    MarsKeyValueSet mars;
    MiscKeyValueSet misc;

    key<MarsKeys::PARAM>(mars).set(228247);
    key<MarsKeys::LEVTYPE>(mars).set(LevType::SFC);

    auto res = mars2mars::applyMappings(mars, misc);

    EXPECT(res);
    EXPECT(key<MarsKeys::LEVTYPE>(mars).get() == LevType::HL);
    EXPECT(key<MarsKeys::LEVELIST>(mars).get() == 100);
    EXPECT(key<MarsKeys::PARAM>(mars).get() == 132);
};


CASE("Test fixWindspeedV200m") {
    using namespace multio::mars2grib::rules;
    using namespace multio::mars2grib;
    using namespace multio::datamod;

    MarsKeyValueSet mars;
    MiscKeyValueSet misc;

    key<MarsKeys::PARAM>(mars).set(228240);
    key<MarsKeys::LEVTYPE>(mars).set(LevType::SFC);

    auto res = mars2mars::applyMappings(mars, misc);

    EXPECT(res);
    EXPECT(key<MarsKeys::LEVTYPE>(mars).get() == LevType::HL);
    EXPECT(key<MarsKeys::LEVELIST>(mars).get() == 200);
    EXPECT(key<MarsKeys::PARAM>(mars).get() == 132);
};


CASE("Test fixWindspeed100m") {
    using namespace multio::mars2grib::rules;
    using namespace multio::mars2grib;
    using namespace multio::datamod;

    MarsKeyValueSet mars;
    MiscKeyValueSet misc;

    key<MarsKeys::PARAM>(mars).set(228249);
    key<MarsKeys::LEVTYPE>(mars).set(LevType::SFC);

    auto res = mars2mars::applyMappings(mars, misc);

    EXPECT(res);
    EXPECT(key<MarsKeys::LEVTYPE>(mars).get() == LevType::HL);
    EXPECT(key<MarsKeys::LEVELIST>(mars).get() == 100);
    EXPECT(key<MarsKeys::PARAM>(mars).get() == 10);
};

CASE("Test fixWindspeed200m") {
    using namespace multio::mars2grib::rules;
    using namespace multio::mars2grib;
    using namespace multio::datamod;

    MarsKeyValueSet mars;
    MiscKeyValueSet misc;

    key<MarsKeys::PARAM>(mars).set(228241);
    key<MarsKeys::LEVTYPE>(mars).set(LevType::SFC);

    auto res = mars2mars::applyMappings(mars, misc);

    EXPECT(res);
    EXPECT(key<MarsKeys::LEVTYPE>(mars).get() == LevType::HL);
    EXPECT(key<MarsKeys::LEVELIST>(mars).get() == 200);
    EXPECT(key<MarsKeys::PARAM>(mars).get() == 131);
};


CASE("Test ruleWaveBitsPerValueS1") {
    using namespace multio::mars2grib::rules;
    using namespace multio::mars2grib;
    using namespace multio::datamod;

    MarsKeyValueSet mars;
    MiscKeyValueSet misc;

    key<MarsKeys::PARAM>(mars).set(22824);
    key<MarsKeys::STEP>(mars).set(123);

    auto res = mars2mars::applyMappings(mars, misc);

    EXPECT(res);
    EXPECT(key<MiscKeys::BitsPerValue>(misc).has());
    EXPECT(key<MiscKeys::BitsPerValue>(misc).get() == 24);
};

CASE("Test ruleWaveBitsPerValue") {
    using namespace multio::mars2grib::rules;
    using namespace multio::mars2grib;
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
        MarsKeyValueSet mars;
        MiscKeyValueSet misc;

        key<MarsKeys::PARAM>(mars).set(paramId);

        auto res = mars2mars::applyMappings(mars, misc);

        EXPECT(res);
        EXPECT(key<MiscKeys::BitsPerValue>(misc).has());
        EXPECT(key<MiscKeys::BitsPerValue>(misc).get() == 24);
    }
};

}  // namespace multio::test

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}

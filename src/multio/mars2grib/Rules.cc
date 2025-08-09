#include "multio/mars2grib/Rules.h"
#include "multio/datamod/GribKeys.h"
#include "multio/datamod/MarsMiscGeo.h"
#include "multio/datamod/core/EntryDef.h"
#include "multio/datamod/core/Record.h"
#include "multio/mars2grib/EncoderConf.h"
#include "multio/mars2grib/Mars2GribException.h"
#include "multio/mars2grib/generated/InferPDT.h"
#include "multio/mars2grib/rules/Matcher.h"
#include "multio/mars2grib/rules/ParamMatcher.h"
#include "multio/mars2grib/rules/Rule.h"
#include "multio/mars2grib/rules/Setter.h"
#include "multio/mars2grib/sections/SectionTypes.h"

#include "multio/util/Print.h"


namespace multio::mars2grib::rules {

using namespace matcher;
using namespace sections;

namespace dm = multio::datamod;

//-----------------------------------------------------------------------------
// Matchers
//-----------------------------------------------------------------------------

auto matchChemical() {
    return all(Has{dm::CHEM}, Missing{dm::WAVELENGTH}, lessThan(dm::CHEM, 900));
}

auto matchAerosol() {
    return all(Has{dm::CHEM}, Missing{dm::WAVELENGTH}, greaterEqual(dm::CHEM, 900));
}

auto matchOptical() {
    return all(Missing{dm::CHEM}, Has{dm::WAVELENGTH});
}
auto matchChemicalOptical() {
    return all(Has{dm::CHEM}, Has{dm::WAVELENGTH});
}

auto matchLevType(dm::LevType lt) {
    return OneOf{dm::LEVTYPE, {lt}};
}


//-----------------------------------------------------------------------------
// Setters
//-----------------------------------------------------------------------------

// Category setters
auto pointInTime() {
    return Setter([](SectionsConf& c) {
        c.product.ensureInit().modify().pdtCat.ensureInit().modify().timeExtent.set(TimeExtent::PointInTime);
        c.product.ensureInit().modify().pointInTime.ensureInit();
    });
}
auto timeRange(TimeRangeType type, dm::TypeOfStatisticalProcessing typeOfStatisticalProcessing) {
    return Setter([=](SectionsConf& c) {
        c.product.ensureInit().modify().pdtCat.ensureInit().modify().timeExtent.set(TimeExtent::TimeRange);
        c.product.ensureInit().modify().timeRange.ensureInit().modify().type.set(type);
        c.product.ensureInit().modify().timeRange.ensureInit().modify().typeOfStatisticalProcessing.set(
            typeOfStatisticalProcessing);
    });
}
auto overallLengthOfTimeRange(const std::string& l) {
    return Setter([=](SectionsConf& c) {
        c.product.ensureInit().modify().timeRange.ensureInit().modify().overallLengthOfTimeRange.set(l);
    });
}

auto ensemble() {
    return Setter([](SectionsConf& c) {
        c.product.ensureInit().modify().pdtCat.ensureInit().modify().processSubType.set(ProcessSubType::Ensemble);
        c.product.ensureInit().modify().process.ensureInit();
    });
}
auto largeEnsemble() {
    return Setter([](SectionsConf& c) {
        c.product.ensureInit().modify().pdtCat.ensureInit().modify().processSubType.set(ProcessSubType::LargeEnsemble);
        c.product.ensureInit().modify().process.ensureInit();
    });
}
auto reforecast() {
    return Setter([](SectionsConf& c) {
        c.product.ensureInit().modify().pdtCat.ensureInit().modify().processType.set(ProcessType::Reforecast);
        c.product.ensureInit().modify().process.ensureInit();
    });
}

auto chemical() {
    return Setter([](SectionsConf& c) {
        c.product.ensureInit().modify().chemical.ensureInit();
        c.product.ensureInit().modify().pdtCat.ensureInit().modify().productCategory.set(ProductCategory::Chemical);
    });
}

auto periodRange() {
    return Setter([](SectionsConf& c) {
        c.product.ensureInit().modify().periodRange.ensureInit();
        c.product.ensureInit().modify().pdtCat.ensureInit().modify().productCategory.set(ProductCategory::Wave);
        c.product.ensureInit().modify().pdtCat.ensureInit().modify().productSubCategory.set(
            ProductSubCategory::PeriodRange);
    });
}

auto dirFreq() {
    return Setter([](SectionsConf& c) {
        c.product.ensureInit().modify().dirFreq.ensureInit();
        c.product.ensureInit().modify().pdtCat.ensureInit().modify().productCategory.set(ProductCategory::Wave);
        c.product.ensureInit().modify().pdtCat.ensureInit().modify().productSubCategory.set(
            ProductSubCategory::SpectraList);
    });
}

auto satellite() {
    return Setter([](SectionsConf& c) {
        c.product.ensureInit().modify().satellite.ensureInit();
        c.product.ensureInit().modify().pdtCat.ensureInit().modify().productCategory.set(ProductCategory::Satellite);
    });
}

auto randomPattern() {
    return Setter([](SectionsConf& c) {
        c.product.ensureInit().modify().randomPatterns.ensureInit();
        c.product.ensureInit().modify().pdtCat.ensureInit().modify().spatialExtent.set(SpatialExtent::RandomPatterns);
    });
}


// Other setters

auto typeOfLevel(dm::TypeOfLevel type) {
    return Setter([=](SectionsConf& c) { c.product.ensureInit().modify().level.ensureInit().modify().type.set(type); });
}
auto fixedLevel(dm::EntryValueType_t<decltype(FixedLevel)> lvl) {
    return Setter(
        [=](SectionsConf& c) { c.product.ensureInit().modify().level.ensureInit().modify().fixedLevel.set(lvl); });
}
auto localUse(std::int64_t num) {
    return Setter([=](SectionsConf& c) { c.localUse.ensureInit().modify().templateNumber.set(num); });
}

auto dataRepres(std::int64_t num) {
    return Setter([=](SectionsConf& c) { c.dataRepres.ensureInit().modify().templateNumber.set(num); });
}


auto tablesConfig(const std::string& type) {
    return Setter(
        [=](SectionsConf& c) { c.identification.ensureInit().modify().tables.ensureInit().modify().type.set(type); });
}

auto tablesVersion(std::int64_t version) {
    return Setter([=](SectionsConf& c) {
        c.identification.ensureInit().modify().tables.ensureInit().modify().tablesVersion.set(version);
    });
}

auto localTablesVersion(std::int64_t version) {
    return Setter([=](SectionsConf& c) {
        c.identification.ensureInit().modify().tables.ensureInit().modify().localTablesVersion.set(version);
    });
}


//-----------------------------------------------------------------------------
// Composed rules
//-----------------------------------------------------------------------------

auto makeGridRule(dm::Repres repres, std::int64_t num) {
    return rule(OneOf{dm::REPRES, {repres}},
                Setter([=](SectionsConf& c) { c.grid.ensureInit().modify().templateNumber.set(num); }));
}


auto gridRules() {
    return exclusiveRuleList(makeGridRule(dm::Repres::GG, 40));
}
auto gridRuleSH() {
    return exclusiveRuleList(makeGridRule(dm::Repres::SH, 50));
}


auto localSectionRules() {
    return exclusiveRuleList(  //
        rule(all(Missing{dm::ANOFFSET}, NoneOf{dm::CLASS, {"d1"}}, Missing{dm::METHOD}), localUse(1)),
        rule(all(Missing{dm::ANOFFSET}, NoneOf{dm::CLASS, {"d1"}}, Has{dm::METHOD}), localUse(15)),
        rule(all(Has{dm::ANOFFSET}, NoneOf{dm::CLASS, {"d1"}}), localUse(36)),
        rule(all(Missing{dm::ANOFFSET}, OneOf{dm::CLASS, {"d1"}}), localUse(1001)),
        rule(all(Has{dm::ANOFFSET}, OneOf{dm::CLASS, {"d1"}}), localUse(1036)));
}

auto processTypesRules() {
    return exclusiveRuleList(  //
        rule(all(Missing{dm::NUMBER}, Missing{dm::HDATE})), rule(all(Has{dm::NUMBER}, Missing{dm::HDATE}), ensemble()),
        rule(all(Has{dm::NUMBER}, Has{dm::HDATE}), reforecast(), ensemble()));
}

auto processTypesRulesAl() {
    return exclusiveRuleList(  //
        rule(all(Has{dm::NUMBER}), largeEnsemble()));
}


auto packingRules() {
    return exclusiveRuleList(  //
        rule(OneOf{dm::PACKING, {"simple"}}, dataRepres(0)), rule(OneOf{dm::PACKING, {"ccsds"}}, dataRepres(42)));
}
auto packingRulesSH() {
    return exclusiveRuleList(  //
        rule(OneOf{dm::PACKING, {"complex"}}, dataRepres(51)));
}


//-----------------------------------------------------------------------------
// Params
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// SFC
//-----------------------------------------------------------------------------

auto paramSFCRules() {
    return exclusiveRuleList(                                           //
        rule(all(matchLevType(dm::LevType::SFC), matchParams(228023)),  //
             pointInTime(), typeOfLevel(dm::TypeOfLevel::CloudBase)),   //
        rule(all(matchLevType(dm::LevType::SFC),                        //
                 matchParams(                                           //
                     59, 78, 79, 136, 137, 164, 206, paramRange(162059, 162063), 162071, 162072, 162093, 228044, 228050,
                     228052, 228088, 228089, 228090, 228164, 260132)),          //
             pointInTime(), typeOfLevel(dm::TypeOfLevel::EntireAtmosphere)),    //
        rule(all(matchLevType(dm::LevType::SFC), matchParams(228007, 228011)),  //
             pointInTime(), typeOfLevel(dm::TypeOfLevel::EntireLake)),          //
        rule(all(matchLevType(dm::LevType::SFC), matchParams(121)),             //
             timeRange(TimeRangeType::FixedTimeRange, dm::TypeOfStatisticalProcessing::Maximum),
             overallLengthOfTimeRange("6h"),                                       //
             typeOfLevel(dm::TypeOfLevel::HeightAboveGroundAt2m), fixedLevel(2)),  //
        rule(all(matchLevType(dm::LevType::SFC), matchParams(122)),                //
             timeRange(TimeRangeType::FixedTimeRange, dm::TypeOfStatisticalProcessing::Minimum),
             overallLengthOfTimeRange("6h"),                                                                   //
             typeOfLevel(dm::TypeOfLevel::HeightAboveGroundAt2m), fixedLevel(2)),                              //
        rule(all(matchLevType(dm::LevType::SFC), matchParams(201, 237167)),                                    //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, dm::TypeOfStatisticalProcessing::Maximum),  //
             typeOfLevel(dm::TypeOfLevel::HeightAboveGroundAt2m), fixedLevel(2), fixedLevel(10)),              //
        rule(all(matchLevType(dm::LevType::SFC), matchParams(202, 238167)),                                    //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, dm::TypeOfStatisticalProcessing::Minimum),  //
             typeOfLevel(dm::TypeOfLevel::HeightAboveGroundAt2m), fixedLevel(2)),                              //
        rule(all(matchLevType(dm::LevType::SFC), matchParams(123)),                                            //
             timeRange(TimeRangeType::FixedTimeRange, dm::TypeOfStatisticalProcessing::Maximum),
             overallLengthOfTimeRange("6h"),                                         //
             typeOfLevel(dm::TypeOfLevel::HeightAboveGroundAt10m), fixedLevel(10)),  //
        rule(all(matchLevType(dm::LevType::SFC), matchParams(228028)),               //
             timeRange(TimeRangeType::FixedTimeRange, dm::TypeOfStatisticalProcessing::Maximum),
             overallLengthOfTimeRange("3h"),                                                                    //
             typeOfLevel(dm::TypeOfLevel::HeightAboveGroundAt10m), fixedLevel(10)),                             //
        rule(all(matchLevType(dm::LevType::SFC), matchParams(49, 237207, 237318)),                              //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, dm::TypeOfStatisticalProcessing::Maximum),   //
             typeOfLevel(dm::TypeOfLevel::HeightAboveGroundAt10m), fixedLevel(10)),                             //
        rule(all(matchLevType(dm::LevType::SFC), matchParams(235087, 235088, 235136, 235137, 235288, 235383)),  //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, dm::TypeOfStatisticalProcessing::Average),   //
             typeOfLevel(dm::TypeOfLevel::EntireAtmosphere)),                                                   //
        rule(all(matchLevType(dm::LevType::SFC), matchParams(228005, 235165, 235166)),                          //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, dm::TypeOfStatisticalProcessing::Average),
             typeOfLevel(dm::TypeOfLevel::HeightAboveGroundAt10m), fixedLevel(10)),                            //
        rule(all(matchLevType(dm::LevType::SFC), matchParams(235151)),                                         //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, dm::TypeOfStatisticalProcessing::Average),  //
             typeOfLevel(dm::TypeOfLevel::MeanSea)),                                                           //
        rule(all(matchLevType(dm::LevType::SFC), matchParams(235039, 235040, 235049, 235050, 235053)),         //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, dm::TypeOfStatisticalProcessing::Average),  //
             typeOfLevel(dm::TypeOfLevel::NominalTop)),                                                        //
        rule(all(matchLevType(dm::LevType::SFC),                                                               //
                 matchParams(235020, 235021, 235031, paramRange(235033, 235038), paramRange(235041, 235043), 235051,
                             235052, 235055, 235078, 235079, 235134, 235283)),                                   //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, dm::TypeOfStatisticalProcessing::Average),    //
             typeOfLevel(dm::TypeOfLevel::Surface)),                                                             //
        rule(all(matchLevType(dm::LevType::SFC), matchParams(260683)),                                           //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, dm::TypeOfStatisticalProcessing::Mode),       //
             typeOfLevel(dm::TypeOfLevel::Surface)),                                                             //
        rule(all(matchLevType(dm::LevType::SFC), matchParams(260682)),                                           //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, dm::TypeOfStatisticalProcessing::Severity),   //
             typeOfLevel(dm::TypeOfLevel::Surface)),                                                             //
        rule(all(matchLevType(dm::LevType::SFC), matchParams(129172)),                                           //
             pointInTime(),                                                                                      //
             typeOfLevel(dm::TypeOfLevel::HeightAboveGround)),                                                   //
        rule(all(matchLevType(dm::LevType::SFC), matchParams(165, 166, 207, 228029, 228131, 228132, 260260)),    //
             pointInTime(),                                                                                      //
             typeOfLevel(dm::TypeOfLevel::HeightAboveGroundAt10m), fixedLevel(10)),                              //
        rule(all(matchLevType(dm::LevType::SFC), matchParams(167, 168, 174096, 228037, 260242)),                 //
             pointInTime(),                                                                                      //
             typeOfLevel(dm::TypeOfLevel::HeightAboveGroundAt2m), fixedLevel(2)),                                //
        rule(all(matchLevType(dm::LevType::SFC), matchParams(140245, 140249, 140233)),                           //
             pointInTime(),                                                                                      //
             typeOfLevel(dm::TypeOfLevel::HeightAboveSeaAt10m), fixedLevel(10)),                                 //
        rule(all(matchLevType(dm::LevType::SFC), matchParams(3075)),                                             //
             pointInTime(),                                                                                      //
             typeOfLevel(dm::TypeOfLevel::HighCloudLayer)),                                                      //
        rule(all(matchLevType(dm::LevType::SFC), matchParams(3074)),                                             //
             pointInTime(),                                                                                      //
             typeOfLevel(dm::TypeOfLevel::MediumCloudLayer)),                                                    //
        rule(all(matchLevType(dm::LevType::SFC), matchParams(3073)),                                             //
             pointInTime(),                                                                                      //
             typeOfLevel(dm::TypeOfLevel::LowCloudLayer)),                                                       //
        rule(all(matchLevType(dm::LevType::SFC), matchParams(228014)),                                           //
             pointInTime(),                                                                                      //
             typeOfLevel(dm::TypeOfLevel::IceLayerOnWater)),                                                     //
        rule(all(matchLevType(dm::LevType::SFC), matchParams(228013)),                                           //
             pointInTime(),                                                                                      //
             typeOfLevel(dm::TypeOfLevel::IceTopOnWater)),                                                       //
        rule(all(matchLevType(dm::LevType::SFC), matchParams(228010)),                                           //
             pointInTime(),                                                                                      //
             typeOfLevel(dm::TypeOfLevel::LakeBottom)),                                                          //
        rule(all(matchLevType(dm::LevType::SFC), matchParams(151)),                                              //
             pointInTime(),                                                                                      //
             typeOfLevel(dm::TypeOfLevel::MeanSea)),                                                             //
        rule(all(matchLevType(dm::LevType::SFC), matchParams(262118)),                                           //
             pointInTime(),                                                                                      //
             typeOfLevel(dm::TypeOfLevel::DepthBelowSeaLayer)),                                                  //
        rule(all(matchLevType(dm::LevType::SFC), matchParams(228231, 228232, 228233, 228234)),                   //
             pointInTime(),                                                                                      //
             typeOfLevel(dm::TypeOfLevel::MixedLayerParcel)),                                                    //
        rule(all(matchLevType(dm::LevType::SFC), matchParams(228008, 228009)),                                   //
             pointInTime(),                                                                                      //
             typeOfLevel(dm::TypeOfLevel::MixingLayer)),                                                         //
        rule(all(matchLevType(dm::LevType::SFC), matchParams(228235, 228236, 228237)),                           //
             pointInTime(),                                                                                      //
             typeOfLevel(dm::TypeOfLevel::MostUnstableParcel)),                                                  //
        rule(all(matchLevType(dm::LevType::SFC), matchParams(178, 179, 208, 209, 212)),                          //
             timeRange(TimeRangeType::SinceBeginningOfForecast, dm::TypeOfStatisticalProcessing::Accumulation),  //
             typeOfLevel(dm::TypeOfLevel::NominalTop)),                                                          //
        rule(all(matchLevType(dm::LevType::SFC), matchParams(235039, 235040)),                                   //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, dm::TypeOfStatisticalProcessing::Average),    //
             typeOfLevel(dm::TypeOfLevel::NominalTop)),                                                          //
        rule(all(matchLevType(dm::LevType::SFC), matchParams(228045)),                                           //
             pointInTime(),                                                                                      //
             typeOfLevel(dm::TypeOfLevel::Tropopause)),                                                          //
        rule(all(matchLevType(dm::LevType::SFC),                                                                 //
                 matchParams(228080, 228081, 228082, paramRange(233032, 233035), 235062, 235063, 235064),
                 matchChemical()),                                                                               //
             timeRange(TimeRangeType::SinceBeginningOfForecast, dm::TypeOfStatisticalProcessing::Accumulation),  //
             chemical(),                                                                                         //
             typeOfLevel(dm::TypeOfLevel::Surface),                                                              //
             tablesConfig("custom"), localTablesVersion(0), tablesVersion(30)),                                  //
        rule(all(matchLevType(dm::LevType::SFC),                                                                 //
                 matchParams(                                                                                    //
                     8, 9, 20, 44, 45, 47, 50, 57, 58, paramRange(142, 147), 169, 175, 176, 177, 180, 181, 182, 189,
                     195, 196, 197, 205, 210, 211, 213, 228, 239, 240, 3062, 3099, paramRange(162100, 162113),
                     paramRange(222001, 222256), 228021, 228022, 228129, 228130, 228143, 228144, 228216, 228228, 228251,
                     231001, 231002, 231003, 231005, 231010, 231012, 231057, 231058, paramRange(233000, 233031),
                     260259)),  //
             timeRange(TimeRangeType::SinceBeginningOfForecast, dm::TypeOfStatisticalProcessing::Accumulation),
             typeOfLevel(dm::TypeOfLevel::Surface)),  //
        rule(all(matchLevType(dm::LevType::SFC),      //
                 matchParams(228051, 228053)),        //
             timeRange(TimeRangeType::FixedTimeRange, dm::TypeOfStatisticalProcessing::Average),
             overallLengthOfTimeRange("1h"),          //
             typeOfLevel(dm::TypeOfLevel::Surface)),  //
        rule(all(matchLevType(dm::LevType::SFC),      //
                 matchParams(228057, 228059)),        //
             timeRange(TimeRangeType::FixedTimeRange, dm::TypeOfStatisticalProcessing::Average),
             overallLengthOfTimeRange("3h"),          //
             typeOfLevel(dm::TypeOfLevel::Surface)),  //
        rule(all(matchLevType(dm::LevType::SFC),      //
                 matchParams(228058, 228060)),        //
             timeRange(TimeRangeType::FixedTimeRange, dm::TypeOfStatisticalProcessing::Average),
             overallLengthOfTimeRange("6h"),          //
             typeOfLevel(dm::TypeOfLevel::Surface)),  //
        rule(all(matchLevType(dm::LevType::SFC),      //
                 matchParams(228026, 228222)),        //
             timeRange(TimeRangeType::FixedTimeRange, dm::TypeOfStatisticalProcessing::Maximum),
             overallLengthOfTimeRange("3h"),            //
             typeOfLevel(dm::TypeOfLevel::Surface)),    //
        rule(all(matchLevType(dm::LevType::SFC),        //
                 matchParams(228224, 228035, 228036)),  //
             timeRange(TimeRangeType::FixedTimeRange, dm::TypeOfStatisticalProcessing::Maximum),
             overallLengthOfTimeRange("6h"),          //
             typeOfLevel(dm::TypeOfLevel::Surface)),  //
        rule(all(matchLevType(dm::LevType::SFC),      //
                 matchParams(228027, 228223)),        //
             timeRange(TimeRangeType::FixedTimeRange, dm::TypeOfStatisticalProcessing::Minimum),
             overallLengthOfTimeRange("3h"),          //
             typeOfLevel(dm::TypeOfLevel::Surface)),  //
        rule(all(matchLevType(dm::LevType::SFC),      //
                 matchParams(228225)),                //
             timeRange(TimeRangeType::FixedTimeRange, dm::TypeOfStatisticalProcessing::Minimum),
             overallLengthOfTimeRange("6h"),                                                                   //
             typeOfLevel(dm::TypeOfLevel::Surface)),                                                           //
        rule(all(matchLevType(dm::LevType::SFC),                                                               //
                 matchParams(paramRange(235033, 235038), 235189, 235326)),                                     //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, dm::TypeOfStatisticalProcessing::Average),  //
             typeOfLevel(dm::TypeOfLevel::Surface)),                                                           //
        rule(all(matchLevType(dm::LevType::SFC),                                                               //
                 matchParams(260320)),                                                                         //
             timeRange(TimeRangeType::FixedTimeRange, dm::TypeOfStatisticalProcessing::Mode),
             overallLengthOfTimeRange("1h"),          //
             typeOfLevel(dm::TypeOfLevel::Surface)),  //
        rule(all(matchLevType(dm::LevType::SFC),      //
                 matchParams(260321)),                //
             timeRange(TimeRangeType::FixedTimeRange, dm::TypeOfStatisticalProcessing::Mode),
             overallLengthOfTimeRange("3h"),          //
             typeOfLevel(dm::TypeOfLevel::Surface)),  //
        rule(all(matchLevType(dm::LevType::SFC),      //
                 matchParams(260339)),                //
             timeRange(TimeRangeType::FixedTimeRange, dm::TypeOfStatisticalProcessing::Mode),
             overallLengthOfTimeRange("6h"),          //
             typeOfLevel(dm::TypeOfLevel::Surface)),  //
        rule(all(matchLevType(dm::LevType::SFC),      //
                 matchParams(260318)),                //
             timeRange(TimeRangeType::FixedTimeRange, dm::TypeOfStatisticalProcessing::Severity),
             overallLengthOfTimeRange("1h"),          //
             typeOfLevel(dm::TypeOfLevel::Surface)),  //
        rule(all(matchLevType(dm::LevType::SFC),      //
                 matchParams(260319)),                //
             timeRange(TimeRangeType::FixedTimeRange, dm::TypeOfStatisticalProcessing::Severity),
             overallLengthOfTimeRange("3h"),          //
             typeOfLevel(dm::TypeOfLevel::Surface)),  //
        rule(all(matchLevType(dm::LevType::SFC),      //
                 matchParams(260338)),                //
             timeRange(TimeRangeType::FixedTimeRange, dm::TypeOfStatisticalProcessing::Severity),
             overallLengthOfTimeRange("6h"),          //
             typeOfLevel(dm::TypeOfLevel::Surface)),  //
        rule(all(matchLevType(dm::LevType::SFC),      //
                 matchParams(                         //
                     paramRange(15, 18), paramRange(26, 32), 33, paramRange(34, 43), paramRange(66, 67), 74, 129, 134,
                     139, 141, 148, 159, paramRange(160, 163), 170, paramRange(172, 174), paramRange(186, 188), 198,
                     paramRange(229, 232), paramRange(234, 236), 238, paramRange(243, 245), 3020, 3067, 160198, 200199,
                     210200, 210201, 210202, 228003, 228012, paramRange(210186, 210191), 210262, 210263, 210264,
                     paramRange(228015, 228020), 228024, 228032, paramRange(228046, 228048), 228141,
                     paramRange(228217, 228221), 260004, 260005, 260015, 260048, 260109, 260121, 260123, 260255, 260289,
                     260509, 260688, 261001, 261002, 261014, 261015, 261016, 261018, 262000, 262100, 262139, 262140,
                     262144, 262124)),                     //
             pointInTime(),                                //
             typeOfLevel(dm::TypeOfLevel::Surface)),       //
        rule(all(matchLevType(dm::LevType::SFC),           //
                 matchParams(paramRange(228083, 228085)),  //
                 matchChemical()),                         //
             pointInTime(),                                //
             chemical(),
             typeOfLevel(dm::TypeOfLevel::Surface)),  //
        rule(all(matchLevType(dm::LevType::SFC),      //
                 matchParams(                         //
                     paramRange(140098, 140105), paramRange(140112, 140113), paramRange(140121, 140129),
                     paramRange(140131, 140134), paramRange(140207, 140209), paramRange(140211, 140212),
                     paramRange(140214, 140232), paramRange(140234, 140239), 140244, paramRange(140252, 140254))),  //
             pointInTime(),                                                                                         //
             typeOfLevel(dm::TypeOfLevel::Surface)),                                                                //
        rule(all(matchLevType(dm::LevType::SFC),                                                                    //
                 matchParams(paramRange(140114, 140120))),                                                          //
             pointInTime(),                                                                                         //
             periodRange(),
             typeOfLevel(dm::TypeOfLevel::Surface)),                                                           //
        rule(all(matchLevType(dm::LevType::SFC),                                                               //
                 matchParams(228226, 237013, 237055, 237117, 237321)),                                         //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, dm::TypeOfStatisticalProcessing::Maximum),  //
             typeOfLevel(dm::TypeOfLevel::Surface)),                                                           //
        rule(all(matchLevType(dm::LevType::SFC),                                                               //
                 matchParams(228227, 238055, 238013)),                                                         //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, dm::TypeOfStatisticalProcessing::Minimum),  //
             typeOfLevel(dm::TypeOfLevel::Surface)),                                                           //
        rule(all(matchLevType(dm::LevType::SFC),                                                               //
                 matchParams(140251)),                                                                         //
             pointInTime(),                                                                                    //
             dirFreq()),                                                                                       //
        rule(all(matchLevType(dm::LevType::SFC),                                                               //
                 matchParams(262104)),                                                                         //
             pointInTime(),                                                                                    //
             typeOfLevel(dm::TypeOfLevel::Isothermal))                                                         //
        // // Wind speed 100m & 200m (has been fix before)
        // rule(all(matchLevType(dm::LevType::SFC), matchParams(228241, 228249)),  //
        //      pointInTime(),                                                     //
        //      typeOfLevel(dm::TypeOfLevel::HeightAboveGround))                       //
    );
}


auto paramHLRules() {
    return exclusiveRuleList(                                                                                  //
        rule(all(matchLevType(dm::LevType::HL), matchParams(10, 54, 130, 131, 132, 157, 246, 247, 3031)),      //
             pointInTime(),                                                                                    //
             typeOfLevel(dm::TypeOfLevel::HeightAboveGround)),                                                 //
        rule(all(matchLevType(dm::LevType::HL), matchParams(235131, 235132)),                                  //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, dm::TypeOfStatisticalProcessing::Average),  //
             typeOfLevel(dm::TypeOfLevel::HeightAboveGround))                                                  //
    );
}


//-----------------------------------------------------------------------------
// ML
//-----------------------------------------------------------------------------

auto paramMLRules() {
    return exclusiveRuleList(                                                                                    //
        rule(all(matchLevType(dm::LevType::ML), matchParams(75, 76, 133, 203, 246, 247, 248, 260290)),           //
             pointInTime(),                                                                                      //
             typeOfLevel(dm::TypeOfLevel::Hybrid)),                                                              //
        rule(all(matchLevType(dm::LevType::ML), matchParams(paramRange(162100, 162113))),                        //
             timeRange(TimeRangeType::SinceBeginningOfForecast, dm::TypeOfStatisticalProcessing::Accumulation),  //
             typeOfLevel(dm::TypeOfLevel::Hybrid))                                                               //
    );
}

auto paramMLRulesSH() {
    return exclusiveRuleList(                                                                              //
        rule(all(matchLevType(dm::LevType::ML), matchParams(77, 129, 130, 131, 132, 135, 138, 152, 155)),  //
             pointInTime(),                                                                                //
             typeOfLevel(dm::TypeOfLevel::Hybrid))                                                         //
    );
}


//-----------------------------------------------------------------------------
// PL
//-----------------------------------------------------------------------------

// Special composer to handle pressure units differently
// The exclusion list should be the outermost - hence we are passing in the specializations
template <typename MkTail>
auto plLevelRules(MkTail&& mkTail) {
    return exclusiveRuleList(                                                          //
        chainedRuleList(                                                               //
            rule(all(matchLevType(dm::LevType::PL), greaterEqual(dm::LEVELIST, 100)),  //
                 typeOfLevel(dm::TypeOfLevel::IsobaricInhPa)),
            mkTail()),                                                             //
        chainedRuleList(                                                           //
            rule(all(matchLevType(dm::LevType::PL), lessThan(dm::LEVELIST, 100)),  //
                 typeOfLevel(dm::TypeOfLevel::IsobaricInPa)),
            mkTail())  //
    );
}

auto paramPLRules() {
    return plLevelRules([]() {
        return exclusiveRuleList(                                                                                  //
            rule(matchParams(60, 75, 76, paramRange(129, 135), 203, 246, 247, 248, 157, 260290),                   //
                 pointInTime()),                                                                                   //
            rule(matchParams(235100, paramRange(235129, 235133), 235135, 235157, 235246),                          //
                 timeRange(TimeRangeType::SinceLastPostProcessingStep, dm::TypeOfStatisticalProcessing::Average))  //
        );
    });
}

auto paramPLRulesSH() {
    return plLevelRules([]() {
        return exclusiveRuleList(                                              //
            rule(matchParams(1, 2, paramRange(129, 135), 138, 152, 155, 157),  //
                 pointInTime())                                                //
        );
    });
}


//-----------------------------------------------------------------------------
// PT
//-----------------------------------------------------------------------------

auto paramPTRules() {
    return exclusiveRuleList(                                                                                  //
        rule(all(matchLevType(dm::LevType::PT), matchParams(53, 54, 60, 131, 132, 133, 138, 155, 203)),        //
             pointInTime(),                                                                                    //
             typeOfLevel(dm::TypeOfLevel::Theta)),                                                             //
        rule(all(matchLevType(dm::LevType::PT), matchParams(235203)),                                          //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, dm::TypeOfStatisticalProcessing::Average),  //
             typeOfLevel(dm::TypeOfLevel::Theta)),                                                             //
        rule(all(matchLevType(dm::LevType::PT), matchParams(237203)),                                          //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, dm::TypeOfStatisticalProcessing::Maximum),  //
             typeOfLevel(dm::TypeOfLevel::Theta)),                                                             //
        rule(all(matchLevType(dm::LevType::PT), matchParams(238203)),                                          //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, dm::TypeOfStatisticalProcessing::Minimum),  //
             typeOfLevel(dm::TypeOfLevel::Theta)),                                                             //
        rule(all(matchLevType(dm::LevType::PT), matchParams(239203)),                                          //
             timeRange(TimeRangeType::SinceLastPostProcessingStep,
                       dm::TypeOfStatisticalProcessing::StandardDeviation),  //
             typeOfLevel(dm::TypeOfLevel::Theta))                            //
    );
}

auto paramPTRulesSH() {
    return exclusiveRuleList(                                                                   //
        rule(all(matchLevType(dm::LevType::PT), matchParams(53, 54, 131, 132, 133, 138, 155)),  //
             pointInTime(),                                                                     //
             typeOfLevel(dm::TypeOfLevel::Theta))                                               //
    );
}

//-----------------------------------------------------------------------------
// PV
//-----------------------------------------------------------------------------

auto paramPVRules() {
    return exclusiveRuleList(                                                                  //
        rule(all(matchLevType(dm::LevType::PV), matchParams(3, 54, 129, 131, 132, 133, 203)),  //
             pointInTime(),                                                                    //
             typeOfLevel(dm::TypeOfLevel::PotentialVorticity))                                 //
    );
}

auto paramPVRulesSH() {
    return exclusiveRuleList(                                              //
        rule(all(matchLevType(dm::LevType::PV), matchParams(3, 54, 129)),  //
             pointInTime(),                                                //
             typeOfLevel(dm::TypeOfLevel::PotentialVorticity))             //
    );
}


//-----------------------------------------------------------------------------
// Soil
//-----------------------------------------------------------------------------

auto paramSOLRules() {
    return exclusiveRuleList(                                                                                  //
        rule(all(matchLevType(dm::LevType::SOL), matchParams(262000, 262024)),                                 //
             pointInTime(),                                                                                    //
             typeOfLevel(dm::TypeOfLevel::SeaIceLayer)),                                                       //
        rule(all(matchLevType(dm::LevType::SOL), matchParams(33, 74, 238, 228038)),                            //
             pointInTime(),                                                                                    //
             typeOfLevel(dm::TypeOfLevel::SnowLayer)),                                                         //
        rule(all(matchLevType(dm::LevType::SOL), matchParams(228141)),                                         //
             pointInTime(),                                                                                    //
             typeOfLevel(dm::TypeOfLevel::Snow)),                                                              //
        rule(all(matchLevType(dm::LevType::SOL), matchParams(260360, 260199, 183)),                            //
             pointInTime(),                                                                                    //
             typeOfLevel(dm::TypeOfLevel::SoilLayer)),                                                         //
        rule(all(matchLevType(dm::LevType::SOL), matchParams(235077)),                                         //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, dm::TypeOfStatisticalProcessing::Average),  //
             typeOfLevel(dm::TypeOfLevel::SoilLayer)),                                                         //
        rule(all(matchLevType(dm::LevType::SOL), matchParams(235078)),                                         //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, dm::TypeOfStatisticalProcessing::Average),  //
             typeOfLevel(dm::TypeOfLevel::Snow))                                                               //
    );
}


//-----------------------------------------------------------------------------
// Al
//-----------------------------------------------------------------------------

auto paramAlRules() {
    return exclusiveRuleList(                                                              //
        rule(all(matchLevType(dm::LevType::AL), matchParams(paramRange(213101, 213160))),  //
             pointInTime(),                                                                //
             randomPattern(),                                                              //
             typeOfLevel(dm::TypeOfLevel::AbstractSingleLevel))                            //
    );
}


//-----------------------------------------------------------------------------
// Satellite
//-----------------------------------------------------------------------------

auto paramSatelliteRules() {
    return exclusiveRuleList(                                                               //
        rule(all(matchLevType(dm::LevType::SFC), matchParams(paramRange(260510, 260513))),  //
             pointInTime(),                                                                 //
             satellite()));
}


//-----------------------------------------------------------------------------
// Final composed param rules
//-----------------------------------------------------------------------------

// TODO can be optimized by pulling the levtype check one level out -
// exclusiveRuleList(
//    chainedRuleList(rule(matchLevType(XXX)), paramXXXRules()),
//    ....
// )

auto paramRules() {
    return mergeRuleList(      //
        paramSFCRules(),       //
        paramHLRules(),        //
        paramMLRules(),        //
        paramPLRules(),        //
        paramPTRules(),        //
        paramPVRules(),        //
        paramSOLRules(),       //
        paramSatelliteRules()  //
    );
}


auto paramRulesSH() {
    return mergeRuleList(  //
        paramMLRulesSH(),  //
        paramPLRulesSH(),  //
        paramPTRulesSH(),  //
        paramPVRulesSH()   //
    );
}


//-----------------------------------------------------------------------------
// Big rule tree...
//-----------------------------------------------------------------------------

const ExclusiveRuleList& allRules() {
    static auto all_ = exclusiveRuleList(
        // Branch for grids
        chainedRuleList(                                                       //
            rule(all(Has{dm::GRID}, NoneOf{dm::LEVTYPE, {dm::LevType::AL}})),  //
            gridRules(),                                                       //
            localSectionRules(),                                               //
            processTypesRules(),                                               //
            paramRules(),                                                      //
            packingRules()                                                     //
            ),

        // Branch for spherical harmonics
        chainedRuleList(                                                             //
            rule(all(Has{dm::TRUNCATION}, NoneOf{dm::LEVTYPE, {dm::LevType::AL}})),  //
            gridRuleSH(),                                                            //
            localSectionRules(),                                                     //
            processTypesRules(),                                                     //
            paramRulesSH(),                                                          //
            packingRulesSH()                                                         //
            ),

        // Branch for abstract level
        chainedRuleList(                          //
            rule(matchLevType(dm::LevType::AL)),  //
            gridRules(),                          //
            localSectionRules(),                  //
            processTypesRulesAl(),                //
            paramAlRules(),                       //
            packingRules()                        //
            ));
    return all_;
}

SectionsConf buildEncoderConf(const dm::MarsRecord& mars) {
    SectionsConf sections;
    if (!allRules()(mars, sections)) {
        std::ostringstream oss;
        oss << "Cannot map mars keys. None of the outermost rules apply: ";
        util::print(oss, allRules());
        throw Mars2GribException(oss.str(), Here());
    }
    dm::applyRecordDefaults(sections);
    dm::validateRecord(sections);
    return sections;
}

}  // namespace multio::mars2grib::rules

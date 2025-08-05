#include "multio/mars2grib/Rules.h"
#include "multio/datamod/DataModelling.h"
#include "multio/datamod/GribTypes.h"
#include "multio/datamod/MarsMiscGeo.h"
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
using namespace rules;
using namespace datamod;
using namespace sections;

//-----------------------------------------------------------------------------
// Matchers
//-----------------------------------------------------------------------------

auto matchChemical() {
    return all(Has<MarsKeys::CHEM>{}, Missing<MarsKeys::WAVELENGTH>{}, LessThan<MarsKeys::CHEM>{900});
}

auto matchAerosol() {
    return all(Has<MarsKeys::CHEM>{}, Missing<MarsKeys::WAVELENGTH>{}, GreaterEqual<MarsKeys::CHEM>{900});
}

auto matchOptical() {
    return all(Missing<MarsKeys::CHEM>{}, Has<MarsKeys::WAVELENGTH>{});
}
auto matchChemicalOptical() {
    return all(Has<MarsKeys::CHEM>{}, Has<MarsKeys::WAVELENGTH>{});
}

auto matchLevType(datamod::LevType lt) {
    return OneOf<MarsKeys::LEVTYPE>{{lt}};
}


//-----------------------------------------------------------------------------
// Setters
//-----------------------------------------------------------------------------

// Category setters
auto pointInTime() {
    return setAll(setKey<PDTCatDef::TimeExtent, EncoderSectionsDef::Product, EncoderProductDef::PDTCat>(
                      {TimeExtent::PointInTime}),
                  setKey<EncoderProductDef::PointInTime, EncoderSectionsDef::Product>());
}
auto timeRange(TimeRangeType type, TypeOfStatisticalProcessing typeOfStatisticalProcessing) {
    return setAll(
        setKey<PDTCatDef::TimeExtent, EncoderSectionsDef::Product, EncoderProductDef::PDTCat>({TimeExtent::TimeRange}),
        setKey<EncoderTimeRangeDef::Type, EncoderSectionsDef::Product, EncoderProductDef::TimeRange>({type}),
        setKey<EncoderTimeRangeDef::TypeOfStatisticalProcessing, EncoderSectionsDef::Product,
               EncoderProductDef::TimeRange>({typeOfStatisticalProcessing}));
}
auto overallLengthOfTimeRange(const std::string& l) {
    return setKey<EncoderTimeRangeDef::OverallLengthOfTimeRange, EncoderSectionsDef::Product,
                  EncoderProductDef::TimeRange>({l});
}

auto ensemble() {
    return setAll(setKey<PDTCatDef::ProcessSubType, EncoderSectionsDef::Product, EncoderProductDef::PDTCat>(
                      {ProcessSubType::Ensemble}),
                  setKey<EncoderProductDef::Process, EncoderSectionsDef::Product>());
}
auto largeEnsemble() {
    return setAll(setKey<PDTCatDef::ProcessSubType, EncoderSectionsDef::Product, EncoderProductDef::PDTCat>(
                      {ProcessSubType::LargeEnsemble}),
                  setKey<EncoderProductDef::Process, EncoderSectionsDef::Product>());
}
auto reforecast() {
    return setAll(setKey<PDTCatDef::ProcessType, EncoderSectionsDef::Product, EncoderProductDef::PDTCat>(
                      {ProcessType::Reforecast}),
                  setKey<EncoderProductDef::Process, EncoderSectionsDef::Product>());
}

auto chemical() {
    return setAll(setKey<EncoderProductDef::Chemical, EncoderSectionsDef::Product>(),
                  setKey<PDTCatDef::ProductCategory, EncoderSectionsDef::Product, EncoderProductDef::PDTCat>(
                      {ProductCategory::Chemical}));
}

auto periodRange() {
    return setAll(setKey<EncoderProductDef::PeriodRange, EncoderSectionsDef::Product>(),
                  setKey<PDTCatDef::ProductCategory, EncoderSectionsDef::Product, EncoderProductDef::PDTCat>(
                      {ProductCategory::Wave}),
                  setKey<PDTCatDef::ProductSubCategory, EncoderSectionsDef::Product, EncoderProductDef::PDTCat>(
                      {ProductSubCategory::PeriodRange}));
}

auto dirFreq() {
    return setAll(setKey<EncoderProductDef::DirFreq, EncoderSectionsDef::Product>(),
                  setKey<PDTCatDef::ProductCategory, EncoderSectionsDef::Product, EncoderProductDef::PDTCat>(
                      {ProductCategory::Wave}),
                  setKey<PDTCatDef::ProductSubCategory, EncoderSectionsDef::Product, EncoderProductDef::PDTCat>(
                      {ProductSubCategory::SpectraList}));
}

auto satellite() {
    return setAll(setKey<EncoderProductDef::Satellite, EncoderSectionsDef::Product>(),
                  setKey<PDTCatDef::ProductCategory, EncoderSectionsDef::Product, EncoderProductDef::PDTCat>(
                      {ProductCategory::Satellite}));
}

auto randomPattern() {
    return setAll(setKey<EncoderProductDef::RandomPatterns, EncoderSectionsDef::Product>(),
                  setKey<PDTCatDef::SpatialExtent, EncoderSectionsDef::Product, EncoderProductDef::PDTCat>(
                      {SpatialExtent::RandomPatterns}));
}

// Other setters

auto typeOfLevel(TypeOfLevel lvl) {
    return setKey<LevelDef::Type, EncoderSectionsDef::Product, EncoderProductDef::Level>({lvl});
}
auto fixedLevel(KeyDefValueType_t<LevelDef::FixedLevel> lvl) {
    return setKey<LevelDef::FixedLevel, EncoderSectionsDef::Product, EncoderProductDef::Level>({lvl});
}
auto localUse(std::int64_t num) {
    return setKey<EncoderLocalUseDef::TemplateNumber, EncoderSectionsDef::LocalUse>({num});
}

auto dataRepres(std::int64_t num) {
    return setKey<EncoderDataRepresDef::TemplateNumber, EncoderSectionsDef::DataRepres>({num});
}


auto tablesConfig(const std::string& type) {
    return setKey<EncoderTablesDef::Type, EncoderSectionsDef::Identification, EncoderIdentificationDef::Tables>({type});
}

auto tablesVersion(std::int64_t version) {
    return setKey<EncoderTablesDef::TablesVersion, EncoderSectionsDef::Identification,
                  EncoderIdentificationDef::Tables>({version});
}

auto localTablesVersion(std::int64_t version) {
    return setKey<EncoderTablesDef::LocalTablesVersion, EncoderSectionsDef::Identification,
                  EncoderIdentificationDef::Tables>({version});
}


//-----------------------------------------------------------------------------
// Composed rules
//-----------------------------------------------------------------------------

auto makeGridRule(datamod::Repres repres, std::int64_t num) {
    return rule(OneOf<MarsKeys::REPRES>{{repres}},
                setKey<EncoderGridDef::TemplateNumber, EncoderSectionsDef::Grid>({num}));
}


auto gridRules() {
    return exclusiveRuleList(makeGridRule(Repres::GG, 40));
}
auto gridRuleSH() {
    return exclusiveRuleList(makeGridRule(Repres::SH, 50));
}


auto localSectionRules() {
    return exclusiveRuleList(  //
        rule(all(Missing<MarsKeys::ANOFFSET>{}, NoneOf<MarsKeys::CLASS>{{"d1"}}, Missing<MarsKeys::METHOD>{}),
             localUse(1)),
        rule(all(Missing<MarsKeys::ANOFFSET>{}, NoneOf<MarsKeys::CLASS>{{"d1"}}, Has<MarsKeys::METHOD>{}),
             localUse(15)),
        rule(all(Has<MarsKeys::ANOFFSET>{}, NoneOf<MarsKeys::CLASS>{{"d1"}}), localUse(36)),
        rule(all(Missing<MarsKeys::ANOFFSET>{}, OneOf<MarsKeys::CLASS>{{"d1"}}), localUse(1001)),
        rule(all(Has<MarsKeys::ANOFFSET>{}, OneOf<MarsKeys::CLASS>{{"d1"}}), localUse(1036)));
}

auto processTypesRules() {
    return exclusiveRuleList(  //
        rule(all(Missing<MarsKeys::NUMBER>{}, Missing<MarsKeys::HDATE>{})),
        rule(all(Has<MarsKeys::NUMBER>{}, Missing<MarsKeys::HDATE>{}), ensemble()),
        rule(all(Has<MarsKeys::NUMBER>{}, Has<MarsKeys::HDATE>{}), reforecast(), ensemble()));
}

auto processTypesRulesAl() {
    return exclusiveRuleList(  //
        rule(all(Has<MarsKeys::NUMBER>{}), largeEnsemble()));
}


auto packingRules() {
    return exclusiveRuleList(  //
        rule(OneOf<MarsKeys::PACKING>{{"simple"}}, dataRepres(0)),
        rule(OneOf<MarsKeys::PACKING>{{"ccsds"}}, dataRepres(42)));
}
auto packingRulesSH() {
    return exclusiveRuleList(  //
        rule(OneOf<MarsKeys::PACKING>{{"complex"}}, dataRepres(52)));
}


//-----------------------------------------------------------------------------
// Params
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// SFC
//-----------------------------------------------------------------------------

auto paramSFCRules() {
    return exclusiveRuleList(                                       //
        rule(all(matchLevType(LevType::SFC), matchParams(228023)),  //
             pointInTime(), typeOfLevel(TypeOfLevel::CloudBase)),   //
        rule(all(matchLevType(LevType::SFC),                        //
                 matchParams(                                       //
                     59, 78, 79, 136, 137, 164, 206, paramRange(162059, 162063), 162071, 162072, 162093, 228044, 228050,
                     228052, 228088, 228089, 228090, 228164, 260132)),      //
             pointInTime(), typeOfLevel(TypeOfLevel::EntireAtmosphere)),    //
        rule(all(matchLevType(LevType::SFC), matchParams(228007, 228011)),  //
             pointInTime(), typeOfLevel(TypeOfLevel::EntireLake)),          //
        rule(all(matchLevType(LevType::SFC), matchParams(121)),             //
             timeRange(TimeRangeType::FixedTimeRange, TypeOfStatisticalProcessing::Maximum),
             overallLengthOfTimeRange("6h"),                                   //
             typeOfLevel(TypeOfLevel::HeightAboveGroundAt2m), fixedLevel(2)),  //
        rule(all(matchLevType(LevType::SFC), matchParams(122)),                //
             timeRange(TimeRangeType::FixedTimeRange, TypeOfStatisticalProcessing::Minimum),
             overallLengthOfTimeRange("6h"),                                                               //
             typeOfLevel(TypeOfLevel::HeightAboveGroundAt2m), fixedLevel(2)),                              //
        rule(all(matchLevType(LevType::SFC), matchParams(201, 237167)),                                    //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TypeOfStatisticalProcessing::Maximum),  //
             typeOfLevel(TypeOfLevel::HeightAboveGroundAt2m), fixedLevel(2), fixedLevel(10)),              //
        rule(all(matchLevType(LevType::SFC), matchParams(202, 238167)),                                    //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TypeOfStatisticalProcessing::Minimum),  //
             typeOfLevel(TypeOfLevel::HeightAboveGroundAt2m), fixedLevel(2)),                              //
        rule(all(matchLevType(LevType::SFC), matchParams(123)),                                            //
             timeRange(TimeRangeType::FixedTimeRange, TypeOfStatisticalProcessing::Maximum),
             overallLengthOfTimeRange("6h"),                                     //
             typeOfLevel(TypeOfLevel::HeightAboveGroundAt10m), fixedLevel(10)),  //
        rule(all(matchLevType(LevType::SFC), matchParams(228028)),               //
             timeRange(TimeRangeType::FixedTimeRange, TypeOfStatisticalProcessing::Maximum),
             overallLengthOfTimeRange("3h"),                                                                //
             typeOfLevel(TypeOfLevel::HeightAboveGroundAt10m), fixedLevel(10)),                             //
        rule(all(matchLevType(LevType::SFC), matchParams(49, 237207, 237318)),                              //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TypeOfStatisticalProcessing::Maximum),   //
             typeOfLevel(TypeOfLevel::HeightAboveGroundAt10m), fixedLevel(10)),                             //
        rule(all(matchLevType(LevType::SFC), matchParams(235087, 235088, 235136, 235137, 235288, 235383)),  //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TypeOfStatisticalProcessing::Average),   //
             typeOfLevel(TypeOfLevel::EntireAtmosphere)),                                                   //
        rule(all(matchLevType(LevType::SFC), matchParams(228005, 235165, 235166)),                          //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TypeOfStatisticalProcessing::Average),
             typeOfLevel(TypeOfLevel::HeightAboveGroundAt10m), fixedLevel(10)),                            //
        rule(all(matchLevType(LevType::SFC), matchParams(235151)),                                         //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TypeOfStatisticalProcessing::Average),  //
             typeOfLevel(TypeOfLevel::MeanSea)),                                                           //
        rule(all(matchLevType(LevType::SFC), matchParams(235039, 235040, 235049, 235050, 235053)),         //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TypeOfStatisticalProcessing::Average),  //
             typeOfLevel(TypeOfLevel::NominalTop)),                                                        //
        rule(all(matchLevType(LevType::SFC),                                                               //
                 matchParams(235020, 235021, 235031, paramRange(235033, 235038), paramRange(235041, 235043), 235051,
                             235052, 235055, 235078, 235079, 235134, 235283)),                               //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TypeOfStatisticalProcessing::Average),    //
             typeOfLevel(TypeOfLevel::Surface)),                                                             //
        rule(all(matchLevType(LevType::SFC), matchParams(260683)),                                           //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TypeOfStatisticalProcessing::Mode),       //
             typeOfLevel(TypeOfLevel::Surface)),                                                             //
        rule(all(matchLevType(LevType::SFC), matchParams(260682)),                                           //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TypeOfStatisticalProcessing::Severity),   //
             typeOfLevel(TypeOfLevel::Surface)),                                                             //
        rule(all(matchLevType(LevType::SFC), matchParams(129172)),                                           //
             pointInTime(),                                                                                  //
             typeOfLevel(TypeOfLevel::HeightAboveGround)),                                                   //
        rule(all(matchLevType(LevType::SFC), matchParams(165, 166, 207, 228029, 228131, 228132, 260260)),    //
             pointInTime(),                                                                                  //
             typeOfLevel(TypeOfLevel::HeightAboveGroundAt10m), fixedLevel(10)),                              //
        rule(all(matchLevType(LevType::SFC), matchParams(167, 168, 174096, 228037, 260242)),                 //
             pointInTime(),                                                                                  //
             typeOfLevel(TypeOfLevel::HeightAboveGroundAt2m), fixedLevel(2)),                                //
        rule(all(matchLevType(LevType::SFC), matchParams(140245, 140249, 140233)),                           //
             pointInTime(),                                                                                  //
             typeOfLevel(TypeOfLevel::HeightAboveSeaAt10m), fixedLevel(10)),                                 //
        rule(all(matchLevType(LevType::SFC), matchParams(3075)),                                             //
             pointInTime(),                                                                                  //
             typeOfLevel(TypeOfLevel::HighCloudLayer)),                                                      //
        rule(all(matchLevType(LevType::SFC), matchParams(3074)),                                             //
             pointInTime(),                                                                                  //
             typeOfLevel(TypeOfLevel::MediumCloudLayer)),                                                    //
        rule(all(matchLevType(LevType::SFC), matchParams(3073)),                                             //
             pointInTime(),                                                                                  //
             typeOfLevel(TypeOfLevel::LowCloudLayer)),                                                       //
        rule(all(matchLevType(LevType::SFC), matchParams(228014)),                                           //
             pointInTime(),                                                                                  //
             typeOfLevel(TypeOfLevel::IceLayerOnWater)),                                                     //
        rule(all(matchLevType(LevType::SFC), matchParams(228013)),                                           //
             pointInTime(),                                                                                  //
             typeOfLevel(TypeOfLevel::IceTopOnWater)),                                                       //
        rule(all(matchLevType(LevType::SFC), matchParams(228010)),                                           //
             pointInTime(),                                                                                  //
             typeOfLevel(TypeOfLevel::LakeBottom)),                                                          //
        rule(all(matchLevType(LevType::SFC), matchParams(151)),                                              //
             pointInTime(),                                                                                  //
             typeOfLevel(TypeOfLevel::MeanSea)),                                                             //
        rule(all(matchLevType(LevType::SFC), matchParams(262118)),                                           //
             pointInTime(),                                                                                  //
             typeOfLevel(TypeOfLevel::DepthBelowSeaLayer)),                                                  //
        rule(all(matchLevType(LevType::SFC), matchParams(228231, 228232, 228233, 228234)),                   //
             pointInTime(),                                                                                  //
             typeOfLevel(TypeOfLevel::MixedLayerParcel)),                                                    //
        rule(all(matchLevType(LevType::SFC), matchParams(228008, 228009)),                                   //
             pointInTime(),                                                                                  //
             typeOfLevel(TypeOfLevel::MixingLayer)),                                                         //
        rule(all(matchLevType(LevType::SFC), matchParams(228235, 228236, 228237)),                           //
             pointInTime(),                                                                                  //
             typeOfLevel(TypeOfLevel::MostUnstableParcel)),                                                  //
        rule(all(matchLevType(LevType::SFC), matchParams(178, 179, 208, 209, 212)),                          //
             timeRange(TimeRangeType::SinceBeginningOfForecast, TypeOfStatisticalProcessing::Accumulation),  //
             typeOfLevel(TypeOfLevel::NominalTop)),                                                          //
        rule(all(matchLevType(LevType::SFC), matchParams(235039, 235040)),                                   //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TypeOfStatisticalProcessing::Average),    //
             typeOfLevel(TypeOfLevel::NominalTop)),                                                          //
        rule(all(matchLevType(LevType::SFC), matchParams(228045)),                                           //
             pointInTime(),                                                                                  //
             typeOfLevel(TypeOfLevel::Tropopause)),                                                          //
        rule(all(matchLevType(LevType::SFC),                                                                 //
                 matchParams(228080, 228081, 228082, paramRange(233032, 233035), 235062, 235063, 235064),
                 matchChemical()),                                                                           //
             timeRange(TimeRangeType::SinceBeginningOfForecast, TypeOfStatisticalProcessing::Accumulation),  //
             chemical(),                                                                                     //
             typeOfLevel(TypeOfLevel::Surface),                                                              //
             tablesConfig("custom"), localTablesVersion(0), tablesVersion(30)),                              //
        rule(all(matchLevType(LevType::SFC),                                                                 //
                 matchParams(                                                                                //
                     8, 9, 20, 44, 45, 47, 50, 57, 58, paramRange(142, 147), 169, 175, 176, 177, 180, 181, 182, 189,
                     195, 196, 197, 205, 210, 211, 213, 228, 239, 240, 3062, 3099, paramRange(162100, 162113),
                     paramRange(222001, 222256), 228021, 228022, 228129, 228130, 228143, 228144, 228216, 228228, 228251,
                     231001, 231002, 231003, 231005, 231010, 231012, 231057, 231058, paramRange(233000, 233031),
                     260259)),  //
             timeRange(TimeRangeType::SinceBeginningOfForecast, TypeOfStatisticalProcessing::Accumulation),
             overallLengthOfTimeRange("1h"),      //
             typeOfLevel(TypeOfLevel::Surface)),  //
        rule(all(matchLevType(LevType::SFC),      //
                 matchParams(228051, 228053)),    //
             timeRange(TimeRangeType::FixedTimeRange, TypeOfStatisticalProcessing::Average),
             overallLengthOfTimeRange("1h"),      //
             typeOfLevel(TypeOfLevel::Surface)),  //
        rule(all(matchLevType(LevType::SFC),      //
                 matchParams(228057, 228059)),    //
             timeRange(TimeRangeType::FixedTimeRange, TypeOfStatisticalProcessing::Average),
             overallLengthOfTimeRange("3h"),      //
             typeOfLevel(TypeOfLevel::Surface)),  //
        rule(all(matchLevType(LevType::SFC),      //
                 matchParams(228058, 228060)),    //
             timeRange(TimeRangeType::FixedTimeRange, TypeOfStatisticalProcessing::Average),
             overallLengthOfTimeRange("6h"),      //
             typeOfLevel(TypeOfLevel::Surface)),  //
        rule(all(matchLevType(LevType::SFC),      //
                 matchParams(228026, 228222)),    //
             timeRange(TimeRangeType::FixedTimeRange, TypeOfStatisticalProcessing::Maximum),
             overallLengthOfTimeRange("3h"),            //
             typeOfLevel(TypeOfLevel::Surface)),        //
        rule(all(matchLevType(LevType::SFC),            //
                 matchParams(228224, 228035, 228036)),  //
             timeRange(TimeRangeType::FixedTimeRange, TypeOfStatisticalProcessing::Maximum),
             overallLengthOfTimeRange("6h"),      //
             typeOfLevel(TypeOfLevel::Surface)),  //
        rule(all(matchLevType(LevType::SFC),      //
                 matchParams(228027, 228223)),    //
             timeRange(TimeRangeType::FixedTimeRange, TypeOfStatisticalProcessing::Minimum),
             overallLengthOfTimeRange("3h"),      //
             typeOfLevel(TypeOfLevel::Surface)),  //
        rule(all(matchLevType(LevType::SFC),      //
                 matchParams(228225)),            //
             timeRange(TimeRangeType::FixedTimeRange, TypeOfStatisticalProcessing::Minimum),
             overallLengthOfTimeRange("6h"),                                                               //
             typeOfLevel(TypeOfLevel::Surface)),                                                           //
        rule(all(matchLevType(LevType::SFC),                                                               //
                 matchParams(paramRange(235033, 235038), 235189, 235326)),                                 //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TypeOfStatisticalProcessing::Average),  //
             typeOfLevel(TypeOfLevel::Surface)),                                                           //
        rule(all(matchLevType(LevType::SFC),                                                               //
                 matchParams(260320)),                                                                     //
             timeRange(TimeRangeType::FixedTimeRange, TypeOfStatisticalProcessing::Mode),
             overallLengthOfTimeRange("1h"),      //
             typeOfLevel(TypeOfLevel::Surface)),  //
        rule(all(matchLevType(LevType::SFC),      //
                 matchParams(260321)),            //
             timeRange(TimeRangeType::FixedTimeRange, TypeOfStatisticalProcessing::Mode),
             overallLengthOfTimeRange("3h"),      //
             typeOfLevel(TypeOfLevel::Surface)),  //
        rule(all(matchLevType(LevType::SFC),      //
                 matchParams(260339)),            //
             timeRange(TimeRangeType::FixedTimeRange, TypeOfStatisticalProcessing::Mode),
             overallLengthOfTimeRange("6h"),      //
             typeOfLevel(TypeOfLevel::Surface)),  //
        rule(all(matchLevType(LevType::SFC),      //
                 matchParams(260318)),            //
             timeRange(TimeRangeType::FixedTimeRange, TypeOfStatisticalProcessing::Severity),
             overallLengthOfTimeRange("1h"),      //
             typeOfLevel(TypeOfLevel::Surface)),  //
        rule(all(matchLevType(LevType::SFC),      //
                 matchParams(260319)),            //
             timeRange(TimeRangeType::FixedTimeRange, TypeOfStatisticalProcessing::Severity),
             overallLengthOfTimeRange("3h"),      //
             typeOfLevel(TypeOfLevel::Surface)),  //
        rule(all(matchLevType(LevType::SFC),      //
                 matchParams(260338)),            //
             timeRange(TimeRangeType::FixedTimeRange, TypeOfStatisticalProcessing::Severity),
             overallLengthOfTimeRange("6h"),      //
             typeOfLevel(TypeOfLevel::Surface)),  //
        rule(all(matchLevType(LevType::SFC),      //
                 matchParams(                     //
                     paramRange(15, 18), paramRange(26, 32), 33, paramRange(34, 43), paramRange(66, 67), 74, 129, 134,
                     139, 141, 148, 159, paramRange(160, 163), 170, paramRange(172, 174), paramRange(186, 188), 198,
                     paramRange(229, 232), paramRange(234, 236), 238, paramRange(243, 245), 3020, 3067, 160198, 200199,
                     210200, 210201, 210202, 228003, 228012, paramRange(210186, 210191), 210262, 210263, 210264,
                     paramRange(228015, 228020), 228024, 228032, paramRange(228046, 228048), 228141,
                     paramRange(228217, 228221), 260004, 260005, 260015, 260048, 260109, 260121, 260123, 260255, 260289,
                     260509, 260688, 261001, 261002, 261014, 261015, 261016, 261018, 262000, 262100, 262139, 262140,
                     262144, 262124)),                     //
             pointInTime(),                                //
             typeOfLevel(TypeOfLevel::Surface)),           //
        rule(all(matchLevType(LevType::SFC),               //
                 matchParams(paramRange(228083, 228085)),  //
                 matchChemical()),                         //
             pointInTime(),                                //
             chemical(),
             typeOfLevel(TypeOfLevel::Surface)),  //
        rule(all(matchLevType(LevType::SFC),      //
                 matchParams(                     //
                     paramRange(140098, 140105), paramRange(140112, 140113), paramRange(140121, 140129),
                     paramRange(140131, 140134), paramRange(140207, 140209), paramRange(140211, 140212),
                     paramRange(140214, 140232), paramRange(140234, 140239), 140244, paramRange(140252, 140254))),  //
             pointInTime(),                                                                                         //
             typeOfLevel(TypeOfLevel::Surface)),                                                                    //
        rule(all(matchLevType(LevType::SFC),                                                                        //
                 matchParams(paramRange(140114, 140120))),                                                          //
             pointInTime(),                                                                                         //
             periodRange(),
             typeOfLevel(TypeOfLevel::Surface)),                                                           //
        rule(all(matchLevType(LevType::SFC),                                                               //
                 matchParams(228226, 237013, 237055, 237117, 237321)),                                     //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TypeOfStatisticalProcessing::Maximum),  //
             typeOfLevel(TypeOfLevel::Surface)),                                                           //
        rule(all(matchLevType(LevType::SFC),                                                               //
                 matchParams(228227, 238055, 238013)),                                                     //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TypeOfStatisticalProcessing::Minimum),  //
             typeOfLevel(TypeOfLevel::Surface)),                                                           //
        rule(all(matchLevType(LevType::SFC),                                                               //
                 matchParams(140251)),                                                                     //
             pointInTime(),                                                                                //
             dirFreq(),                                                                                    //
             typeOfLevel(TypeOfLevel::Surface)),                                                           //
        rule(all(matchLevType(LevType::SFC),                                                               //
                 matchParams(262104)),                                                                     //
             pointInTime(),                                                                                //
             typeOfLevel(TypeOfLevel::Isothermal))                                                         //
    );
}


auto paramHLRules() {
    return exclusiveRuleList(                                                                              //
        rule(all(matchLevType(LevType::HL), matchParams(10, 54, 130, 131, 132, 157, 246, 247, 3031)),      //
             pointInTime(),                                                                                //
             typeOfLevel(TypeOfLevel::HeightAboveGround)),                                                 //
        rule(all(matchLevType(LevType::HL), matchParams(235131, 235132)),                                  //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TypeOfStatisticalProcessing::Average),  //
             typeOfLevel(TypeOfLevel::HeightAboveGround))                                                  //
    );
}


//-----------------------------------------------------------------------------
// ML
//-----------------------------------------------------------------------------

auto paramMLRules() {
    return exclusiveRuleList(                                                                                //
        rule(all(matchLevType(LevType::ML), matchParams(75, 76, 133, 203, 246, 247, 248, 260290)),           //
             pointInTime(),                                                                                  //
             typeOfLevel(TypeOfLevel::Hybrid)),                                                              //
        rule(all(matchLevType(LevType::ML), matchParams(paramRange(162100, 162113))),                        //
             timeRange(TimeRangeType::SinceBeginningOfForecast, TypeOfStatisticalProcessing::Accumulation),  //
             typeOfLevel(TypeOfLevel::Hybrid))                                                               //
    );
}

auto paramMLRulesSH() {
    return exclusiveRuleList(                                                                          //
        rule(all(matchLevType(LevType::ML), matchParams(77, 129, 130, 131, 132, 135, 138, 152, 155)),  //
             pointInTime(),                                                                            //
             typeOfLevel(TypeOfLevel::Hybrid))                                                         //
    );
}


//-----------------------------------------------------------------------------
// PL
//-----------------------------------------------------------------------------

// Special composer to handle pressure units differently
// The exclusion list should be the outermost - hence we are passing in the specializations
template <typename MkTail>
auto plLevelRules(MkTail&& mkTail) {
    return exclusiveRuleList(                                                            //
        chainedRuleList(                                                                 //
            rule(all(matchLevType(LevType::PL), GreaterEqual<MarsKeys::LEVELIST>{100}),  //
                 typeOfLevel(TypeOfLevel::IsobaricInhPa)),
            mkTail()),                                                               //
        chainedRuleList(                                                             //
            rule(all(matchLevType(LevType::PL), LessThan<MarsKeys::LEVELIST>{100}),  //
                 typeOfLevel(TypeOfLevel::IsobaricInPa)),
            mkTail())  //
    );
}

auto paramPLRules() {
    return plLevelRules([]() {
        return exclusiveRuleList(                                                                              //
            rule(matchParams(60, 75, 76, paramRange(129, 135), 203, 246, 247, 248, 157, 260290),               //
                 pointInTime()),                                                                               //
            rule(matchParams(235100, paramRange(235129, 235133), 235135, 235157, 235246),                      //
                 timeRange(TimeRangeType::SinceLastPostProcessingStep, TypeOfStatisticalProcessing::Average))  //
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
    return exclusiveRuleList(                                                                                        //
        rule(all(matchLevType(LevType::PT), matchParams(53, 54, 60, 131, 132, 133, 138, 155, 203)),                  //
             pointInTime(),                                                                                          //
             typeOfLevel(TypeOfLevel::Theta)),                                                                       //
        rule(all(matchLevType(LevType::PT), matchParams(235203)),                                                    //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TypeOfStatisticalProcessing::Average),            //
             typeOfLevel(TypeOfLevel::Theta)),                                                                       //
        rule(all(matchLevType(LevType::PT), matchParams(237203)),                                                    //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TypeOfStatisticalProcessing::Maximum),            //
             typeOfLevel(TypeOfLevel::Theta)),                                                                       //
        rule(all(matchLevType(LevType::PT), matchParams(238203)),                                                    //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TypeOfStatisticalProcessing::Minimum),            //
             typeOfLevel(TypeOfLevel::Theta)),                                                                       //
        rule(all(matchLevType(LevType::PT), matchParams(239203)),                                                    //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TypeOfStatisticalProcessing::StandardDeviation),  //
             typeOfLevel(TypeOfLevel::Theta))                                                                        //
    );
}

auto paramPTRulesSH() {
    return exclusiveRuleList(                                                               //
        rule(all(matchLevType(LevType::PT), matchParams(53, 54, 131, 132, 133, 138, 155)),  //
             pointInTime(),                                                                 //
             typeOfLevel(TypeOfLevel::Theta))                                               //
    );
}

//-----------------------------------------------------------------------------
// PV
//-----------------------------------------------------------------------------

auto paramPVRules() {
    return exclusiveRuleList(                                                              //
        rule(all(matchLevType(LevType::PV), matchParams(3, 54, 129, 131, 132, 133, 203)),  //
             pointInTime(),                                                                //
             typeOfLevel(TypeOfLevel::PotentialVorticity))                                 //
    );
}

auto paramPVRulesSH() {
    return exclusiveRuleList(                                          //
        rule(all(matchLevType(LevType::PV), matchParams(3, 54, 129)),  //
             pointInTime(),                                            //
             typeOfLevel(TypeOfLevel::PotentialVorticity))             //
    );
}


//-----------------------------------------------------------------------------
// Soil
//-----------------------------------------------------------------------------

auto paramSOLRules() {
    return exclusiveRuleList(                                                                              //
        rule(all(matchLevType(LevType::SOL), matchParams(262000, 262024)),                                 //
             pointInTime(),                                                                                //
             typeOfLevel(TypeOfLevel::SeaIceLayer)),                                                       //
        rule(all(matchLevType(LevType::SOL), matchParams(33, 74, 238, 228038)),                            //
             pointInTime(),                                                                                //
             typeOfLevel(TypeOfLevel::SnowLayer)),                                                         //
        rule(all(matchLevType(LevType::SOL), matchParams(228141)),                                         //
             pointInTime(),                                                                                //
             typeOfLevel(TypeOfLevel::Snow)),                                                              //
        rule(all(matchLevType(LevType::SOL), matchParams(260360, 260199, 183)),                            //
             pointInTime(),                                                                                //
             typeOfLevel(TypeOfLevel::SoilLayer)),                                                         //
        rule(all(matchLevType(LevType::SOL), matchParams(235077)),                                         //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TypeOfStatisticalProcessing::Average),  //
             typeOfLevel(TypeOfLevel::SoilLayer)),                                                         //
        rule(all(matchLevType(LevType::SOL), matchParams(235078)),                                         //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TypeOfStatisticalProcessing::Average),  //
             typeOfLevel(TypeOfLevel::Snow))                                                               //
    );
}


//-----------------------------------------------------------------------------
// Al
//-----------------------------------------------------------------------------

auto paramAlRules() {
    return exclusiveRuleList(                                                          //
        rule(all(matchLevType(LevType::AL), matchParams(paramRange(213101, 213160))),  //
             pointInTime(),                                                            //
             randomPattern(),                                                          //
             typeOfLevel(TypeOfLevel::AbstractSingleLevel))                            //
    );
}


//-----------------------------------------------------------------------------
// Satellite
//-----------------------------------------------------------------------------

auto paramSatelliteRules() {
    return exclusiveRuleList(                                                           //
        rule(all(matchLevType(LevType::SFC), matchParams(paramRange(260510, 260513))),  //
             pointInTime(),                                                             //
             satellite()));
}


//-----------------------------------------------------------------------------
// Final composed param rules
//-----------------------------------------------------------------------------

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

const ExclusiveRuleList<MarsKeySet>& allRules() {
    static auto all_ = exclusiveRuleList(
        // Branch for grids
        chainedRuleList(                                                                 //
            rule(all(Has<MarsKeys::GRID>{}, NoneOf<MarsKeys::LEVTYPE>{{LevType::AL}})),  //
            gridRules(),                                                                 //
            localSectionRules(),                                                         //
            processTypesRules(),                                                         //
            paramRules(),                                                                //
            packingRules()                                                               //
            ),

        // Branch for spherical harmonics
        chainedRuleList(                                                                       //
            rule(all(Has<MarsKeys::TRUNCATION>{}, NoneOf<MarsKeys::LEVTYPE>{{LevType::AL}})),  //
            gridRuleSH(),                                                                      //
            localSectionRules(),                                                               //
            processTypesRules(),                                                               //
            paramRulesSH(),                                                                    //
            packingRulesSH()                                                                   //
            ),

        // Branch for abstract level
        chainedRuleList(                      //
            rule(matchLevType(LevType::AL)),  //
            gridRules(),                      //
            localSectionRules(),              //
            processTypesRulesAl(),            //
            paramAlRules(),                   //
            packingRules()                    //
            ));
    return all_;
}

EncoderSections buildEncoderConf(const MarsKeyValueSet& mars) {
    EncoderSections sections;
    if (!allRules()(mars, sections)) {
        std::ostringstream oss;
        oss << "Cannot map mars keys. None of the outermost rules apply: ";
        util::print(oss, allRules());
        throw Mars2GribException(oss.str(), Here());
    }
    alterAndValidate(sections);
    return sections;
}

}  // namespace multio::mars2grib::rules

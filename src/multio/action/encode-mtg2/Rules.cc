#include "multio/action/encode-mtg2/Rules.h"
#include <type_traits>
#include "multio/action/encode-mtg2/EncoderConf.h"
#include "multio/action/encode-mtg2/generated/InferPDT.h"
#include "multio/action/encode-mtg2/rules/Matcher.h"
#include "multio/action/encode-mtg2/rules/Rule.h"
#include "multio/action/encode-mtg2/rules/Setter.h"
#include "multio/datamod/DataModelling.h"
#include "multio/datamod/MarsMiscGeo.h"


namespace multio::action::rules {
using namespace rules;
using namespace datamod;

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

auto matchLevType(const std::string& lt) {
    return OneOf<MarsKeys::LEVTYPE>{{lt}};
}


//-----------------------------------------------------------------------------
// Param matchers
//-----------------------------------------------------------------------------

Range<MarsKeys::PARAM> paramRange(KeyDefValueType_t<MarsKeys::PARAM> start, KeyDefValueType_t<MarsKeys::PARAM> end) {
    return Range<MarsKeys::PARAM>{start, end};
}

using ParamMatcher = Any<OneOf<MarsKeys::PARAM>, Ranges<MarsKeys::PARAM>>;

ParamMatcher matchParams(std::vector<KeyDefValueType_t<MarsKeys::PARAM>> params) {
    return ParamMatcher{std::make_tuple(OneOf<MarsKeys::PARAM>{std::move(params)}, Ranges<MarsKeys::PARAM>{{}})};
}

ParamMatcher matchParams(KeyDefValueType_t<MarsKeys::PARAM> param) {
    return matchParams(std::vector{param});
}

ParamMatcher matchParams(Range<MarsKeys::PARAM> range) {
    return ParamMatcher{std::make_tuple(OneOf<MarsKeys::PARAM>{{}}, Ranges<MarsKeys::PARAM>{{range}})};
}

ParamMatcher matchParams(ParamMatcher m) {
    return m;
}

ParamMatcher matchParams(ParamMatcher m1, ParamMatcher&& m2) {
    auto& m1Vec = std::get<0>(m1.matchers).values;
    auto& m2Vec = std::get<0>(m2.matchers).values;

    m1Vec.insert(m1Vec.end(), std::make_move_iterator(m2Vec.begin()), std::make_move_iterator(m2Vec.end()));


    auto& m1Ranges = std::get<1>(m1.matchers).ranges;
    auto& m2Ranges = std::get<1>(m2.matchers).ranges;

    m1Ranges.insert(m1Ranges.end(), std::make_move_iterator(m2Ranges.begin()), std::make_move_iterator(m2Ranges.end()));

    return m1;
}

template <typename Arg, typename Arg2, typename... More>
ParamMatcher matchParams(Arg&& arg, Arg2&& arg2, More&&... more) {
    return matchParams(matchParams(matchParams(std::forward<Arg>(arg)), matchParams(std::forward<Arg2>(arg2))),
                       std::forward<More>(more)...);
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
auto timeRange(const std::string& type, const std::string& typeOfStatisticalProcessing) {
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
    return setKey<PDTCatDef::ProcessSubType, EncoderSectionsDef::Product, EncoderProductDef::PDTCat>(
        {ProcessSubType::Ensemble});
}
auto largeEnsemble() {
    return setKey<PDTCatDef::ProcessSubType, EncoderSectionsDef::Product, EncoderProductDef::PDTCat>(
        {ProcessSubType::LargeEnsemble});
}
auto reforecast() {
    return setKey<PDTCatDef::ProcessType, EncoderSectionsDef::Product, EncoderProductDef::PDTCat>(
        {ProcessType::Reforecast});
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

auto typeOfLevel(const std::string& lvl) {
    return setKey<EncoderLevelDef::Type, EncoderSectionsDef::Product, EncoderProductDef::Level>({lvl});
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
        rule(all(Missing<MarsKeys::ANOFFSET>{}, NoneOf<MarsKeys::CLASS>{{"d1"}}), localUse(1)),
        rule(all(Has<MarsKeys::ANOFFSET>{}, NoneOf<MarsKeys::CLASS>{{"d1"}}), localUse(36)),
        rule(all(Missing<MarsKeys::ANOFFSET>{}, OneOf<MarsKeys::CLASS>{{"d1"}}), localUse(1001)));
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
    return exclusiveRuleList(                                //
        rule(all(matchLevType("sfc"), matchParams(228023)),  //
             pointInTime(), typeOfLevel("cloudbase")),       //
        rule(all(matchLevType("sfc"),                        //
                 matchParams(                                //
                     59, 78, 79, 136, 137, 164, 206, paramRange(162059, 162063), 162071, 162072, 162093, 228044, 228050,
                     228052, 228088, 228089, 228090, 228164, 260132)),                       //
             pointInTime(), typeOfLevel("entireAtmosphere")),                                //
        rule(all(matchLevType("sfc"), matchParams(228007, 228011)),                          //
             pointInTime(), typeOfLevel("entireLake")),                                      //
        rule(all(matchLevType("sfc"), matchParams(121)),                                     //
             timeRange("fixed-timerange", "max"), overallLengthOfTimeRange("6h"),            //
             typeOfLevel("heightAboveGroundAt2m")),                                          //
        rule(all(matchLevType("sfc"), matchParams(122)),                                     //
             timeRange("fixed-timerange", "min"), overallLengthOfTimeRange("6h"),            //
             typeOfLevel("heightAboveGroundAt2m")),                                          //
        rule(all(matchLevType("sfc"), matchParams(201)),                                     //
             timeRange("since-last-post-processing-step", "max"),                            //
             typeOfLevel("heightAboveGroundAt2m")),                                          //
        rule(all(matchLevType("sfc"), matchParams(202)),                                     //
             timeRange("since-last-post-processing-step", "min"),                            //
             typeOfLevel("heightAboveGroundAt2m")),                                          //
        rule(all(matchLevType("sfc"), matchParams(123)),                                     //
             timeRange("fixed-timerange", "max"), overallLengthOfTimeRange("6h"),            //
             typeOfLevel("heightAboveGroundAt10m")),                                         //
        rule(all(matchLevType("sfc"), matchParams(228028)),                                  //
             timeRange("fixed-timerange", "max"), overallLengthOfTimeRange("3h"),            //
             typeOfLevel("heightAboveGroundAt10m")),                                         //
        rule(all(matchLevType("sfc"), matchParams(49)),                                      //
             timeRange("since-last-post-processing-step", "max"),                            //
             typeOfLevel("heightAboveGroundAt10m")),                                         //
        rule(all(matchLevType("sfc"), matchParams(235087, 235088, 235136, 235137, 235288)),  //
             timeRange("since-last-post-processing-step", "average"),                        //
             typeOfLevel("entireAtmosphere")),                                               //
        rule(all(matchLevType("sfc"), matchParams(228005, 235165, 235166)),                  //
             timeRange("since-last-post-processing-step", "average"),
             typeOfLevel("heightAboveGroundAt10m")),                                         //
        rule(all(matchLevType("sfc"), matchParams(235151)),                                  //
             timeRange("since-last-post-processing-step", "average"),                        //
             typeOfLevel("meanSea")),                                                        //
        rule(all(matchLevType("sfc"), matchParams(235039, 235040, 235049, 235050, 235053)),  //
             timeRange("since-last-post-processing-step", "average"),                        //
             typeOfLevel("nominalTop")),                                                     //
        rule(all(matchLevType("sfc"),                                                        //
                 matchParams(235020, 235021, 235031, paramRange(235033, 235038), paramRange(235041, 235043), 235051,
                             235052, 235055, 235078, 235079, 235134)),                      //
             timeRange("since-last-post-processing-step", "average"),                       //
             typeOfLevel("surface")),                                                       //
        rule(all(matchLevType("sfc"), matchParams(129172)),                                 //
             pointInTime(),                                                                 //
             typeOfLevel("heightAboveGround")),                                             //
        rule(all(matchLevType("sfc"), matchParams(165, 166, 207, 228029, 228131, 228132)),  //
             pointInTime(),                                                                 //
             typeOfLevel("heightAboveGroundAt10m")),                                        //
        rule(all(matchLevType("sfc"), matchParams(167, 168, 174096, 228037, 260242)),       //
             pointInTime(),                                                                 //
             typeOfLevel("heightAboveGroundAt2m")),                                         //
        rule(all(matchLevType("sfc"), matchParams(140245, 140249, 140233)),                 //
             pointInTime(),                                                                 //
             typeOfLevel("heightAboveSeaAt10m")),                                           //
        rule(all(matchLevType("sfc"), matchParams(3075)),                                   //
             pointInTime(),                                                                 //
             typeOfLevel("highCloudLayer")),                                                //
        rule(all(matchLevType("sfc"), matchParams(3074)),                                   //
             pointInTime(),                                                                 //
             typeOfLevel("mediumCloudLayer")),                                              //
        rule(all(matchLevType("sfc"), matchParams(3073)),                                   //
             pointInTime(),                                                                 //
             typeOfLevel("lowCloudLayer")),                                                 //
        rule(all(matchLevType("sfc"), matchParams(228014)),                                 //
             pointInTime(),                                                                 //
             typeOfLevel("iceLayerOnWater")),                                               //
        rule(all(matchLevType("sfc"), matchParams(228013)),                                 //
             pointInTime(),                                                                 //
             typeOfLevel("iceTopOnWater")),                                                 //
        rule(all(matchLevType("sfc"), matchParams(228010)),                                 //
             pointInTime(),                                                                 //
             typeOfLevel("lakeBottom")),                                                    //
        rule(all(matchLevType("sfc"), matchParams(151)),                                    //
             pointInTime(),                                                                 //
             typeOfLevel("meanSea")),                                                       //
        rule(all(matchLevType("sfc"), matchParams(262118)),                                 //
             pointInTime(),                                                                 //
             typeOfLevel("depthBelowSeaLayer")),                                            //
        rule(all(matchLevType("sfc"), matchParams(228231, 228232, 228233, 228234)),         //
             pointInTime(),                                                                 //
             typeOfLevel("mixedLayerParcel")),                                              //
        rule(all(matchLevType("sfc"), matchParams(228008, 228009)),                         //
             pointInTime(),                                                                 //
             typeOfLevel("mixingLayer")),                                                   //
        rule(all(matchLevType("sfc"), matchParams(228235, 228236, 228237)),                 //
             pointInTime(),                                                                 //
             typeOfLevel("mostUnstableParcel")),                                            //
        rule(all(matchLevType("sfc"), matchParams(178, 179, 208, 209, 212)),                //
             timeRange("since-beginning-of-forecast", "accumul"),                           //
             typeOfLevel("nominalTop")),                                                    //
        rule(all(matchLevType("sfc"), matchParams(235039, 235040)),                         //
             timeRange("since-last-post-processing-step", "average"),                       //
             typeOfLevel("nominalTop")),                                                    //
        rule(all(matchLevType("sfc"), matchParams(228045)),                                 //
             pointInTime(),                                                                 //
             typeOfLevel("tropopause")),                                                    //
        rule(all(matchLevType("sfc"),                                                       //
                 matchParams(228080, 228081, 228082, paramRange(233032, 233035), 235062, 235063, 235064),
                 matchChemical()),                                               //
             timeRange("since-beginning-of-forecast", "accumul"),                //
             chemical(),                                                         //
             typeOfLevel("surface"),                                             //
             tablesConfig("custom"), localTablesVersion(0), tablesVersion(30)),  //
        rule(all(matchLevType("sfc"),                                            //
                 matchParams(                                                    //
                     8, 9, 20, 44, 45, 47, 50, 57, 58, paramRange(142, 147), 169, 175, 176, 177, 180, 181, 182, 189,
                     195, 196, 197, 205, 210, 211, 213, 228, 239, 240, 3062, 3099, paramRange(162100, 162113),
                     paramRange(222001, 222256), 228021, 228022, 228129, 228130, 228143, 228144, 228216, 228228, 228251,
                     231001, 231002, 231003, 231005, 231010, 231012, 231057, 231058, paramRange(233000, 233031),
                     260259)),                                                                     //
             timeRange("since-beginning-of-forecast", "accumul"), overallLengthOfTimeRange("1h"),  //
             typeOfLevel("surface")),                                                              //
        rule(all(matchLevType("sfc"),                                                              //
                 matchParams(228051, 228053)),                                                     //
             timeRange("fixed-timerange", "average"), overallLengthOfTimeRange("1h"),              //
             typeOfLevel("surface")),                                                              //
        rule(all(matchLevType("sfc"),                                                              //
                 matchParams(228057, 228059)),                                                     //
             timeRange("fixed-timerange", "average"), overallLengthOfTimeRange("3h"),              //
             typeOfLevel("surface")),                                                              //
        rule(all(matchLevType("sfc"),                                                              //
                 matchParams(228058, 228060)),                                                     //
             timeRange("fixed-timerange", "average"), overallLengthOfTimeRange("6h"),              //
             typeOfLevel("surface")),                                                              //
        rule(all(matchLevType("sfc"),                                                              //
                 matchParams(228026, 228222)),                                                     //
             timeRange("fixed-timerange", "max"), overallLengthOfTimeRange("3h"),                  //
             typeOfLevel("surface")),                                                              //
        rule(all(matchLevType("sfc"),                                                              //
                 matchParams(228224, 228035, 228036)),                                             //
             timeRange("fixed-timerange", "max"), overallLengthOfTimeRange("6h"),                  //
             typeOfLevel("surface")),                                                              //
        rule(all(matchLevType("sfc"),                                                              //
                 matchParams(228027, 228223)),                                                     //
             timeRange("fixed-timerange", "min"), overallLengthOfTimeRange("3h"),                  //
             typeOfLevel("surface")),                                                              //
        rule(all(matchLevType("sfc"),                                                              //
                 matchParams(228225)),                                                             //
             timeRange("fixed-timerange", "min"), overallLengthOfTimeRange("6h"),                  //
             typeOfLevel("surface")),                                                              //
        rule(all(matchLevType("sfc"),                                                              //
                 matchParams(paramRange(235033, 235038), 235189)),                                 //
             timeRange("since-last-post-processing-step", "average"),                              //
             typeOfLevel("surface")),                                                              //
        rule(all(matchLevType("sfc"),                                                              //
                 matchParams(260320)),                                                             //
             timeRange("fixed-timerange", "mode"), overallLengthOfTimeRange("1h"),                 //
             typeOfLevel("surface")),                                                              //
        rule(all(matchLevType("sfc"),                                                              //
                 matchParams(260321)),                                                             //
             timeRange("fixed-timerange", "mode"), overallLengthOfTimeRange("3h"),                 //
             typeOfLevel("surface")),                                                              //
        rule(all(matchLevType("sfc"),                                                              //
                 matchParams(260339)),                                                             //
             timeRange("fixed-timerange", "mode"), overallLengthOfTimeRange("6h"),                 //
             typeOfLevel("surface")),                                                              //
        rule(all(matchLevType("sfc"),                                                              //
                 matchParams(260318)),                                                             //
             timeRange("fixed-timerange", "severity"), overallLengthOfTimeRange("1h"),             //
             typeOfLevel("surface")),                                                              //
        rule(all(matchLevType("sfc"),                                                              //
                 matchParams(260319)),                                                             //
             timeRange("fixed-timerange", "severity"), overallLengthOfTimeRange("3h"),             //
             typeOfLevel("surface")),                                                              //
        rule(all(matchLevType("sfc"),                                                              //
                 matchParams(260338)),                                                             //
             timeRange("fixed-timerange", "severity"), overallLengthOfTimeRange("6h"),             //
             typeOfLevel("surface")),                                                              //
        rule(all(matchLevType("sfc"),                                                              //
                 matchParams(                                                                      //
                     paramRange(15, 18), paramRange(26, 32), 33, paramRange(34, 43), paramRange(66, 67), 74, 129, 134,
                     139, 141, 148, 159, paramRange(160, 163), 170, paramRange(172, 174), paramRange(186, 188), 198,
                     paramRange(229, 232), paramRange(234, 236), 238, paramRange(243, 245), 3020, 3067, 160198, 200199,
                     210200, 210201, 210202, 228003, 228012, paramRange(210186, 210191), 210262, 210263, 210264,
                     paramRange(228015, 228020), 228024, 228032, paramRange(228046, 228048), 228141,
                     paramRange(228217, 228221), 260004, 260005, 260015, 260048, 260109, 260121, 260123, 260255, 260289,
                     260509, 261001, 261002, 261014, 261015, 261016, 261018, 262000, 262100, 262139, 262140, 262144,
                     262124)),                             //
             pointInTime(),                                //
             typeOfLevel("surface")),                      //
        rule(all(matchLevType("sfc"),                      //
                 matchParams(paramRange(228083, 228085)),  //
                 matchChemical()),                         //
             pointInTime(),                                //
             chemical(),
             typeOfLevel("surface")),  //
        rule(all(matchLevType("sfc"),  //
                 matchParams(          //
                     paramRange(140098, 140105), paramRange(140112, 140113), paramRange(140121, 140129),
                     paramRange(140207, 140209), paramRange(140211, 140212), paramRange(140214, 140232),
                     paramRange(140234, 140239), 140244, paramRange(140252, 140254))),  //
             pointInTime(),                                                             //
             typeOfLevel("surface")),                                                   //
        rule(all(matchLevType("sfc"),                                                   //
                 matchParams(paramRange(140114, 140120))),                              //
             pointInTime(),                                                             //
             periodRange(),
             typeOfLevel("surface")),                              //
        rule(all(matchLevType("sfc"),                              //
                 matchParams(paramRange(228226, 237055))),         //
             timeRange("since-last-post-processing-step", "max"),  //
             typeOfLevel("surface")),                              //
        rule(all(matchLevType("sfc"),                              //
                 matchParams(paramRange(228227, 238055))),         //
             timeRange("since-last-post-processing-step", "min"),  //
             typeOfLevel("surface")),                              //
        rule(all(matchLevType("sfc"),                              //
                 matchParams(140251)),                             //
             pointInTime(),                                        //
             dirFreq(),                                            //
             typeOfLevel("surface")),                              //
        rule(all(matchLevType("sfc"),                              //
                 matchParams(262104)),                             //
             pointInTime(),                                        //
             typeOfLevel("isothermal"))                            //
    );
}


auto paramHLRules() {
    return exclusiveRuleList(                                                                   //
        rule(all(matchLevType("hl"), matchParams(10, 54, 130, 131, 132, 157, 246, 247, 3031)),  //
             pointInTime(),                                                                     //
             typeOfLevel("heightAboveGround")),                                                 //
        rule(all(matchLevType("hl"), matchParams(235131, 235132)),                              //
             timeRange("since-last-post-processing-step", "average"),                           //
             typeOfLevel("heightAboveGround"))                                                  //
    );
}


//-----------------------------------------------------------------------------
// ML
//-----------------------------------------------------------------------------

auto paramMLRules() {
    return exclusiveRuleList(                                                                //
        rule(all(matchLevType("ml"), matchParams(75, 76, 133, 203, 246, 247, 248, 260290)),  //
             pointInTime(),                                                                  //
             typeOfLevel("hybrid")),                                                         //
        rule(all(matchLevType("ml"), matchParams(paramRange(162100, 162113))),               //
             timeRange("since-beginning-of-forecast", "accumul"),                            //
             typeOfLevel("hybrid"))                                                          //
    );
}

auto paramMLRulesSH() {
    return exclusiveRuleList(                                                                   //
        rule(all(matchLevType("ml"), matchParams(77, 129, 130, 131, 132, 135, 138, 152, 155)),  //
             pointInTime(),                                                                     //
             typeOfLevel("hybrid"))                                                             //
    );
}


//-----------------------------------------------------------------------------
// PL
//-----------------------------------------------------------------------------

// Special composer to handle pressure units differently
// The exclusion list should be the outermost - hence we are passing in the specializations
template <typename MkTail>
auto plLevelRules(MkTail&& mkTail) {
    return exclusiveRuleList(                                                     //
        chainedRuleList(                                                          //
            rule(all(matchLevType("pl"), GreaterEqual<MarsKeys::LEVELIST>{100}),  //
                 typeOfLevel("isobaricinhpa")),
            mkTail()),                                                        //
        chainedRuleList(                                                      //
            rule(all(matchLevType("pl"), LessThan<MarsKeys::LEVELIST>{100}),  //
                 typeOfLevel("isobaricinpa")),
            mkTail())  //
    );
}

auto paramPLRules() {
    return plLevelRules([]() {
        return exclusiveRuleList(                                                                 //
            rule(matchParams(60, 75, 76, paramRange(129, 135), 203, 246, 247, 248, 157, 260290),  //
                 pointInTime()),                                                                  //
            rule(matchParams(235100, paramRange(235129, 235133), 235135, 235157, 235246),         //
                 timeRange("since-last-post-processing-step", "average"))                         //
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
    return exclusiveRuleList(                                                                 //
        rule(all(matchLevType("pt"), matchParams(53, 54, 60, 131, 132, 133, 138, 155, 203)),  //
             pointInTime(),                                                                   //
             typeOfLevel("theta")),                                                           //
        rule(all(matchLevType("pt"), matchParams(235203)),                                    //
             timeRange("since-last-post-processing-step", "average"),                         //
             typeOfLevel("theta")),                                                           //
        rule(all(matchLevType("pt"), matchParams(237203)),                                    //
             timeRange("since-last-post-processing-step", "max"),                             //
             typeOfLevel("theta")),                                                           //
        rule(all(matchLevType("pt"), matchParams(238203)),                                    //
             timeRange("since-last-post-processing-step", "min"),                             //
             typeOfLevel("theta")),                                                           //
        rule(all(matchLevType("pt"), matchParams(239203)),                                    //
             timeRange("since-last-post-processing-step", "stddev"),                          //
             typeOfLevel("theta"))                                                            //
    );
}

auto paramPTRulesSH() {
    return exclusiveRuleList(                                                        //
        rule(all(matchLevType("pt"), matchParams(53, 54, 131, 132, 133, 138, 155)),  //
             pointInTime(),                                                          //
             typeOfLevel("theta"))                                                   //
    );
}

//-----------------------------------------------------------------------------
// PV
//-----------------------------------------------------------------------------

auto paramPVRules() {
    return exclusiveRuleList(                                                       //
        rule(all(matchLevType("pv"), matchParams(3, 54, 129, 131, 132, 133, 203)),  //
             pointInTime(),                                                         //
             typeOfLevel("potentialVorticity"))                                     //
    );
}

auto paramPVRulesSH() {
    return exclusiveRuleList(                                   //
        rule(all(matchLevType("pv"), matchParams(3, 54, 129)),  //
             pointInTime(),                                     //
             typeOfLevel("potentialVorticity"))                 //
    );
}


//-----------------------------------------------------------------------------
// Soil
//-----------------------------------------------------------------------------

auto paramSOLRules() {
    return exclusiveRuleList(                                             //
        rule(all(matchLevType("sol"), matchParams(262000, 262024)),       //
             pointInTime(),                                               //
             typeOfLevel("seaIceLayer")),                                 //
        rule(all(matchLevType("sol"), matchParams(33, 74, 238, 228038)),  //
             pointInTime(),                                               //
             typeOfLevel("snowLayer")),                                   //
        rule(all(matchLevType("sol"), matchParams(228141)),               //
             pointInTime(),                                               //
             typeOfLevel("snow")),                                        //
        rule(all(matchLevType("sol"), matchParams(260360, 260199, 183)),  //
             pointInTime(),                                               //
             typeOfLevel("soilLayer")),                                   //
        rule(all(matchLevType("sol"), matchParams(235077)),               //
             timeRange("since-last-post-processing-step", "average"),     //
             typeOfLevel("soilLayer")),                                   //
        rule(all(matchLevType("sol"), matchParams(235078)),               //
             timeRange("since-last-post-processing-step", "average"),     //
             typeOfLevel("snow"))                                         //
    );
}


//-----------------------------------------------------------------------------
// Al
//-----------------------------------------------------------------------------

auto paramAlRules() {
    return exclusiveRuleList(                                                   //
        rule(all(matchLevType("al"), matchParams(paramRange(213101, 213160))),  //
             pointInTime(),                                                     //
             randomPattern(),                                                   //
             typeOfLevel("abstractSingleLevel"))                                //
    );
}


//-----------------------------------------------------------------------------
// Satellite
//-----------------------------------------------------------------------------

auto paramSatelliteRules() {
    return exclusiveRuleList(                                                    //
        rule(all(matchLevType("sfc"), matchParams(paramRange(260510, 260513))),  //
             pointInTime(),                                                      //
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
        chainedRuleList(                                                          //
            rule(all(Has<MarsKeys::GRID>{}, NoneOf<MarsKeys::LEVTYPE>{{"al"}})),  //
            gridRules(),                                                          //
            localSectionRules(),                                                  //
            processTypesRules(),                                                  //
            paramRules(),                                                         //
            packingRules()                                                        //
            ),

        // Branch for spherical harmonics
        chainedRuleList(                                                                //
            rule(all(Has<MarsKeys::TRUNCATION>{}, NoneOf<MarsKeys::LEVTYPE>{{"al"}})),  //
            gridRuleSH(),                                                               //
            localSectionRules(),                                                        //
            processTypesRules(),                                                        //
            paramRulesSH(),                                                             //
            packingRulesSH()                                                            //
            ),

        // Branch for abstract level
        chainedRuleList(               //
            rule(matchLevType("al")),  //
            gridRules(),               //
            localSectionRules(),       //
            processTypesRulesAl(),     //
            paramAlRules(),            //
            packingRules()             //
            ));
    return all_;
}

}  // namespace multio::action::rules

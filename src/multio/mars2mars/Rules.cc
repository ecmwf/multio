#include "multio/datamod/MarsMiscGeo.h"

#include "multio/mars2mars/Rules.h"
#include "multio/mars2grib/rules/Matcher.h"
#include "multio/mars2grib/rules/ParamMatcher.h"
#include "multio/mars2grib/rules/Rule.h"
#include "multio/mars2mars/rules/Rule.h"
#include "multio/mars2mars/rules/Setter.h"


namespace multio::mars2mars {

using namespace rules;
using namespace multio::mars2grib::matcher;

//-----------------------------------------------------------------------------
// SOL fixes
//-----------------------------------------------------------------------------

// To param 262024

auto fixParam35ToSol1() {
    return rule(all(matchParams(35), OneOf{dm::LEVTYPE, {dm::LevType::SFC}}),            //
                setMarsKey(dm::LEVTYPE, dm::LevType::SOL), setMarsKey(dm::LEVELIST, 1),  //
                setMarsKey(dm::PARAM, 262024));                                          //
}
auto fixParam36ToSol2() {
    return rule(all(matchParams(36), OneOf{dm::LEVTYPE, {dm::LevType::SFC}}),            //
                setMarsKey(dm::LEVTYPE, dm::LevType::SOL), setMarsKey(dm::LEVELIST, 2),  //
                setMarsKey(dm::PARAM, 262024));                                          //
}
auto fixParam37ToSol3() {
    return rule(all(matchParams(37), OneOf{dm::LEVTYPE, {dm::LevType::SFC}}),            //
                setMarsKey(dm::LEVTYPE, dm::LevType::SOL), setMarsKey(dm::LEVELIST, 3),  //
                setMarsKey(dm::PARAM, 262024));                                          //
}
auto fixParam38ToSol4() {
    return rule(all(matchParams(38), OneOf{dm::LEVTYPE, {dm::LevType::SFC}}),            //
                setMarsKey(dm::LEVTYPE, dm::LevType::SOL), setMarsKey(dm::LEVELIST, 4),  //
                setMarsKey(dm::PARAM, 262024));                                          //
}

// To param 260199

auto fixParam39ToSol1() {
    return rule(all(matchParams(39), OneOf{dm::LEVTYPE, {dm::LevType::SFC}}),            //
                setMarsKey(dm::LEVTYPE, dm::LevType::SOL), setMarsKey(dm::LEVELIST, 1),  //
                setMarsKey(dm::PARAM, 260199));                                          //
}
auto fixParam40ToSol2() {
    return rule(all(matchParams(40), OneOf{dm::LEVTYPE, {dm::LevType::SFC}}),            //
                setMarsKey(dm::LEVTYPE, dm::LevType::SOL), setMarsKey(dm::LEVELIST, 2),  //
                setMarsKey(dm::PARAM, 260199));                                          //
}
auto fixParam41ToSol3() {
    return rule(all(matchParams(41), OneOf{dm::LEVTYPE, {dm::LevType::SFC}}),            //
                setMarsKey(dm::LEVTYPE, dm::LevType::SOL), setMarsKey(dm::LEVELIST, 3),  //
                setMarsKey(dm::PARAM, 260199));                                          //
}
auto fixParam42ToSol4() {
    return rule(all(matchParams(42), OneOf{dm::LEVTYPE, {dm::LevType::SFC}}),            //
                setMarsKey(dm::LEVTYPE, dm::LevType::SOL), setMarsKey(dm::LEVELIST, 4),  //
                setMarsKey(dm::PARAM, 260199));                                          //
}


// To param 260360

auto fixParam139ToSol1() {
    return rule(all(matchParams(139), OneOf{dm::LEVTYPE, {dm::LevType::SFC}}),           //
                setMarsKey(dm::LEVTYPE, dm::LevType::SOL), setMarsKey(dm::LEVELIST, 1),  //
                setMarsKey(dm::PARAM, 260360));                                          //
}
auto fixParam170ToSol2() {
    return rule(all(matchParams(170), OneOf{dm::LEVTYPE, {dm::LevType::SFC}}),           //
                setMarsKey(dm::LEVTYPE, dm::LevType::SOL), setMarsKey(dm::LEVELIST, 2),  //
                setMarsKey(dm::PARAM, 260360));                                          //
}
auto fixParam183ToSol3() {
    return rule(all(matchParams(183), OneOf{dm::LEVTYPE, {dm::LevType::SFC}}),           //
                setMarsKey(dm::LEVTYPE, dm::LevType::SOL), setMarsKey(dm::LEVELIST, 3),  //
                setMarsKey(dm::PARAM, 260360));                                          //
}
auto fixParam236ToSol4() {
    return rule(all(matchParams(236), OneOf{dm::LEVTYPE, {dm::LevType::SFC}}),           //
                setMarsKey(dm::LEVTYPE, dm::LevType::SOL), setMarsKey(dm::LEVELIST, 4),  //
                setMarsKey(dm::PARAM, 260360));                                          //
}


//-----------------------------------------------------------------------------
// Tables Version fixes
//-----------------------------------------------------------------------------

auto fixTablesVersionChemAccum() {
    return rule(all(matchParams(228080, 228081, 228082, 233032, 233033, 233034, 233035, 235062, 235063, 235064)),  //
                setMiscKey(dm::TablesVersion, 30));                                                                //
}
auto fixTablesVersionChemInstant() {
    return rule(all(matchParams(228083, 228084, 228085)),  //
                setMiscKey(dm::TablesVersion, 30));        //
}


//-----------------------------------------------------------------------------
// Cloud cover
//-----------------------------------------------------------------------------

auto fixCloudParam164() {
    return rule(all(matchParams(164)),                                  //
                setMarsKey(dm::PARAM, 228164), SetScaleFactor{100.0});  //
}
auto fixCloudParam186() {
    return rule(all(matchParams(186)),                                //
                setMarsKey(dm::PARAM, 3073), SetScaleFactor{100.0});  //
}
auto fixCloudParam187() {
    return rule(all(matchParams(187)),                                //
                setMarsKey(dm::PARAM, 3074), SetScaleFactor{100.0});  //
}
auto fixCloudParam188() {
    return rule(all(matchParams(188)),                                //
                setMarsKey(dm::PARAM, 3075), SetScaleFactor{100.0});  //
}


//-----------------------------------------------------------------------------
// Precipitation
//-----------------------------------------------------------------------------

auto fixConvectivePrecip143() {
    return rule(all(matchParams(143)),                                   //
                setMarsKey(dm::PARAM, 228143), SetScaleFactor{1000.0});  //
}

auto fixTotalPrecip228() {
    return rule(all(matchParams(228)),                                   //
                setMarsKey(dm::PARAM, 228228), SetScaleFactor{1000.0});  //
}


//-----------------------------------------------------------------------------
// Height above
//-----------------------------------------------------------------------------

auto fixHeightAboveGround2m() {
    return rule(all(OneOf{dm::LEVTYPE, {dm::LevType::SFC}}, OneOf{dm::LEVELIST, {0}},
                    matchParams(167, 168, 174096, 228037)),  //
                setMarsKey(dm::LEVELIST, 2));                //
}

auto fixHeightAboveGround10m() {
    return rule(all(OneOf{dm::LEVTYPE, {dm::LevType::SFC}}, OneOf{dm::LEVELIST, {0}},
                    matchParams(228029, 228131, 228132, 165, 166, 207)),  //
                setMarsKey(dm::LEVELIST, 10));                            //
}

auto fixHeightAboveSea() {
    return rule(all(matchParams(140233, 140245, 140249)),  //
                setMarsKey(dm::LEVELIST, 10));             //
}


//-----------------------------------------------------------------------------
// Misc level fixes
//-----------------------------------------------------------------------------

auto fixMapToSol() {
    return rule(all(OneOf{dm::LEVTYPE, {dm::LevType::SFC}}, Has{dm::LEVELIST}, NoneOf{dm::LEVELIST, {0}},
                    matchParams(33, 238, 228038, 228141, 260360, 262000, 262024, 260199)),  //
                setMarsKey(dm::LEVTYPE, dm::LevType::SOL));                                 //
}


auto fixRunOffWaterParam205() {
    return rule(all(matchParams(205)),                                   //
                setMarsKey(dm::PARAM, 231002), SetScaleFactor{1000.0});  //
}

auto fixSnowfallWaterEquivParam() {
    return rule(all(matchParams(144)),                                   //
                setMarsKey(dm::PARAM, 228144), SetScaleFactor{1000.0});  //
}


auto fixSnowCover() {
    return rule(all(matchParams(260289)),                               //
                setMarsKey(dm::PARAM, 260038), SetScaleFactor{100.0});  //
}


//-----------------------------------------------------------------------------
// Fix Timespan
//-----------------------------------------------------------------------------

auto fixTimespanMax2T() {
    return rule(all(matchParams(121, 228026, 201)),  //
                setMarsKey(dm::PARAM, 237167));      //
}

auto fixTimespanMaxCape() {
    return rule(all(matchParams(228035)),        //
                setMarsKey(dm::PARAM, 237117));  //
}

auto fixTimespanMaxMuCapes() {
    return rule(all(matchParams(228036)),        //
                setMarsKey(dm::PARAM, 237321));  //
}

auto fixTimespanMaxPrecipRate() {
    return rule(all(matchParams(228222, 228224, 228226)),  //
                setMarsKey(dm::PARAM, 237055));            //
}

auto fixTimespanMaxWindGust() {
    return rule(all(matchParams(123, 228028, 49)),  //
                setMarsKey(dm::PARAM, 237318));     //
}

auto fixTimespanMeanFlashDensity() {
    return rule(all(matchParams(228051, 228057, 228058)),  //
                setMarsKey(dm::PARAM, 235326));            //
}

auto fixTimespanMin2T() {
    return rule(all(matchParams(122, 228027, 202)),  //
                setMarsKey(dm::PARAM, 238167));      //
}

auto fixTimespanMinPrecipRate() {
    return rule(all(matchParams(228223, 228225, 228227)),  //
                setMarsKey(dm::PARAM, 238055));            //
}

auto fixTimespanModePrecip() {
    return rule(all(matchParams(260320, 260321, 260339)),  //
                setMarsKey(dm::PARAM, 260683));            //
}

auto fixTimespanSeverityPrecip() {
    return rule(all(matchParams(260318, 260319, 260338)),  //
                setMarsKey(dm::PARAM, 260682));            //
}


//-----------------------------------------------------------------------------
// Fix Windspeed
//-----------------------------------------------------------------------------

auto fixWindspeedU100m() {
    return rule(all(OneOf{dm::LEVTYPE, {dm::LevType::SFC}}, matchParams(228246)),  //
                setMarsKey(dm::LEVTYPE, dm::LevType::HL), setMarsKey(dm::LEVELIST, 100),
                setMarsKey(dm::PARAM, 131));  //
}

auto fixWindspeedU200m() {
    return rule(all(OneOf{dm::LEVTYPE, {dm::LevType::SFC}}, matchParams(228239)),  //
                setMarsKey(dm::LEVTYPE, dm::LevType::HL), setMarsKey(dm::LEVELIST, 200),
                setMarsKey(dm::PARAM, 131));  //
}

auto fixWindspeedV100m() {
    return rule(all(OneOf{dm::LEVTYPE, {dm::LevType::SFC}}, matchParams(228247)),  //
                setMarsKey(dm::LEVTYPE, dm::LevType::HL), setMarsKey(dm::LEVELIST, 100),
                setMarsKey(dm::PARAM, 132));  //
}

auto fixWindspeedV200m() {
    return rule(all(OneOf{dm::LEVTYPE, {dm::LevType::SFC}}, matchParams(228240)),  //
                setMarsKey(dm::LEVTYPE, dm::LevType::HL), setMarsKey(dm::LEVELIST, 200),
                setMarsKey(dm::PARAM, 132));  //
}

auto fixWindspeed100m() {
    return rule(all(OneOf{dm::LEVTYPE, {dm::LevType::SFC}}, matchParams(228241)),                                     //
                setMarsKey(dm::LEVTYPE, dm::LevType::HL), setMarsKey(dm::LEVELIST, 200), setMarsKey(dm::PARAM, 10));  //
}

auto fixWindspeed200m() {
    return rule(all(OneOf{dm::LEVTYPE, {dm::LevType::SFC}}, matchParams(228249)),                                     //
                setMarsKey(dm::LEVTYPE, dm::LevType::HL), setMarsKey(dm::LEVELIST, 100), setMarsKey(dm::PARAM, 10));  //
}


//-----------------------------------------------------------------------------
// Wave rules bits per value
//-----------------------------------------------------------------------------


auto ruleWaveBitsPerValueS1() {
    return rule(all(greaterThan(dm::STEP, std::chrono::hours{1}),
                    matchParams(22824)),            //
                setMiscKey(dm::BitsPerValue, 24));  //
}

auto ruleWaveBitsPerValue() {
    return rule(all(matchParams(paramRange(140098, 140105), paramRange(140112, 140129), paramRange(140207, 140209),
                                paramRange(140211, 140212), paramRange(140214, 140232), paramRange(140234, 140239),
                                140244, paramRange(140251, 140254))),  //
                setMiscKey(dm::BitsPerValue, 24));                     //
}


//-----------------------------------------------------------------------------
// Map incremental fields for type=4i
//-----------------------------------------------------------------------------

const RuleList& incrementalType4IRules() {
    static auto type4i_ = ruleList(
        rule(matchParams(200130), setMarsKey(dm::PARAM, 130)), rule(matchParams(200133), setMarsKey(dm::PARAM, 133)),
        rule(matchParams(200138), setMarsKey(dm::PARAM, 138)), rule(matchParams(200152), setMarsKey(dm::PARAM, 152)),
        rule(matchParams(200155), setMarsKey(dm::PARAM, 155)), rule(matchParams(200203), setMarsKey(dm::PARAM, 203)));
    return type4i_;
}


//-----------------------------------------------------------------------------
// All rules
//-----------------------------------------------------------------------------

const RuleList& wmoUnitMapping() {
    static auto wmo_ = ruleList(       //
        fixCloudParam164(),            //
        fixCloudParam186(),            //
        fixCloudParam187(),            //
        fixCloudParam188(),            //
        fixConvectivePrecip143(),      //
        fixTotalPrecip228(),           //
        fixRunOffWaterParam205(),      //
        fixSnowfallWaterEquivParam(),  //
        fixSnowCover()                 //
    );
    return wmo_;
}


const RuleList& fixIFSOutput() {
    static auto fixIfs_ = ruleList(     //
        fixTablesVersionChemAccum(),    //
        fixTablesVersionChemInstant(),  //
        fixHeightAboveGround2m(),       //
        fixHeightAboveGround10m(),      //
        fixHeightAboveSea(),            //
        fixMapToSol(),                  //
        fixWindspeedU100m(),            //
        fixWindspeedU200m(),            //
        fixWindspeedV100m(),            //
        fixWindspeedV200m(),            //
        fixWindspeed100m(),             //
        fixWindspeed200m()              //
    );
    return fixIfs_;
}

const RuleList& mapDeprecatedGrib1ToGrib2() {
    static auto dep_ = ruleList(        //
        fixParam35ToSol1(),             //
        fixParam36ToSol2(),             //
        fixParam37ToSol3(),             //
        fixParam38ToSol4(),             //
        fixParam39ToSol1(),             //
        fixParam40ToSol2(),             //
        fixParam41ToSol3(),             //
        fixParam42ToSol4(),             //
        fixParam139ToSol1(),            //
        fixParam170ToSol2(),            //
        fixParam183ToSol3(),            //
        fixParam236ToSol4(),            //
        fixTimespanMax2T(),             //
        fixTimespanMaxCape(),           //
        fixTimespanMaxMuCapes(),        //
        fixTimespanMaxPrecipRate(),     //
        fixTimespanMaxWindGust(),       //
        fixTimespanMeanFlashDensity(),  //
        fixTimespanMin2T(),             //
        fixTimespanMinPrecipRate(),     //
        fixTimespanModePrecip(),        //
        fixTimespanSeverityPrecip()     //
    );
    return dep_;
}

//-----------------------------------------------------------------------------
// All rules ... may be reordered...
//-----------------------------------------------------------------------------

// Bits per value are always separated
const RuleList& mapBitsPerValue() {
    static auto all_ = ruleList(   //
        ruleWaveBitsPerValueS1(),  //
        ruleWaveBitsPerValue()     //
    );
    return all_;
}


const RuleList& allRulesNoWMOMapping() {
    static auto all_ = ruleList(incrementalType4IRules(), fixIFSOutput(), mapDeprecatedGrib1ToGrib2());
    return all_;
}

const RuleList& allRules() {
    static auto all_
        = ruleList(wmoUnitMapping(), incrementalType4IRules(), fixIFSOutput(), mapDeprecatedGrib1ToGrib2());
    return all_;
}

std::optional<MappingResult> applyMappings(const RuleList& rules, dm::FullMarsRecord& mars, dm::MiscRecord& misc) {
    return rules(mars, misc);
}

}  // namespace multio::mars2mars

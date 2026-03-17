#include "multio/datamod/MarsMiscGeo.h"

#include "multio/mars2mars/rules/Matcher.h"
#include "multio/mars2mars/rules/ParamMatcher.h"

#include "multio/mars2mars/Rules.h"
#include "multio/mars2mars/rules/Rule.h"
#include "multio/mars2mars/rules/Setter.h"


namespace multio::mars2mars {

using namespace rules;
using namespace multio::mars2mars::matcher;

//-----------------------------------------------------------------------------
// SOL fixes
//-----------------------------------------------------------------------------

// To param 262024

auto fixParam35ToSol1() {
    return rule(all(matchParams(35), OneOf{&dm::FullMarsRecord::levtype, {dm::LevType::SFC}}),                     //
                setKey(&dm::FullMarsRecord::levtype, dm::LevType::SOL), setKey(&dm::FullMarsRecord::levelist, 1),  //
                setKey(&dm::FullMarsRecord::param, 262024));                                                       //
}
auto fixParam36ToSol2() {
    return rule(all(matchParams(36), OneOf{&dm::FullMarsRecord::levtype, {dm::LevType::SFC}}),                     //
                setKey(&dm::FullMarsRecord::levtype, dm::LevType::SOL), setKey(&dm::FullMarsRecord::levelist, 2),  //
                setKey(&dm::FullMarsRecord::param, 262024));                                                       //
}
auto fixParam37ToSol3() {
    return rule(all(matchParams(37), OneOf{&dm::FullMarsRecord::levtype, {dm::LevType::SFC}}),                     //
                setKey(&dm::FullMarsRecord::levtype, dm::LevType::SOL), setKey(&dm::FullMarsRecord::levelist, 3),  //
                setKey(&dm::FullMarsRecord::param, 262024));                                                       //
}
auto fixParam38ToSol4() {
    return rule(all(matchParams(38), OneOf{&dm::FullMarsRecord::levtype, {dm::LevType::SFC}}),                     //
                setKey(&dm::FullMarsRecord::levtype, dm::LevType::SOL), setKey(&dm::FullMarsRecord::levelist, 4),  //
                setKey(&dm::FullMarsRecord::param, 262024));                                                       //
}

// To param 260199

auto fixParam39ToSol1() {
    return rule(all(matchParams(39), OneOf{&dm::FullMarsRecord::levtype, {dm::LevType::SFC}}),                     //
                setKey(&dm::FullMarsRecord::levtype, dm::LevType::SOL), setKey(&dm::FullMarsRecord::levelist, 1),  //
                setKey(&dm::FullMarsRecord::param, 260199));                                                       //
}
auto fixParam40ToSol2() {
    return rule(all(matchParams(40), OneOf{&dm::FullMarsRecord::levtype, {dm::LevType::SFC}}),                     //
                setKey(&dm::FullMarsRecord::levtype, dm::LevType::SOL), setKey(&dm::FullMarsRecord::levelist, 2),  //
                setKey(&dm::FullMarsRecord::param, 260199));                                                       //
}
auto fixParam41ToSol3() {
    return rule(all(matchParams(41), OneOf{&dm::FullMarsRecord::levtype, {dm::LevType::SFC}}),                     //
                setKey(&dm::FullMarsRecord::levtype, dm::LevType::SOL), setKey(&dm::FullMarsRecord::levelist, 3),  //
                setKey(&dm::FullMarsRecord::param, 260199));                                                       //
}
auto fixParam42ToSol4() {
    return rule(all(matchParams(42), OneOf{&dm::FullMarsRecord::levtype, {dm::LevType::SFC}}),                     //
                setKey(&dm::FullMarsRecord::levtype, dm::LevType::SOL), setKey(&dm::FullMarsRecord::levelist, 4),  //
                setKey(&dm::FullMarsRecord::param, 260199));                                                       //
}


// To param 260360

auto fixParam139ToSol1() {
    return rule(all(matchParams(139), OneOf{&dm::FullMarsRecord::levtype, {dm::LevType::SFC}}),                    //
                setKey(&dm::FullMarsRecord::levtype, dm::LevType::SOL), setKey(&dm::FullMarsRecord::levelist, 1),  //
                setKey(&dm::FullMarsRecord::param, 260360));                                                       //
}
auto fixParam170ToSol2() {
    return rule(all(matchParams(170), OneOf{&dm::FullMarsRecord::levtype, {dm::LevType::SFC}}),                    //
                setKey(&dm::FullMarsRecord::levtype, dm::LevType::SOL), setKey(&dm::FullMarsRecord::levelist, 2),  //
                setKey(&dm::FullMarsRecord::param, 260360));                                                       //
}
auto fixParam183ToSol3() {
    return rule(all(matchParams(183), OneOf{&dm::FullMarsRecord::levtype, {dm::LevType::SFC}}),                    //
                setKey(&dm::FullMarsRecord::levtype, dm::LevType::SOL), setKey(&dm::FullMarsRecord::levelist, 3),  //
                setKey(&dm::FullMarsRecord::param, 260360));                                                       //
}
auto fixParam236ToSol4() {
    return rule(all(matchParams(236), OneOf{&dm::FullMarsRecord::levtype, {dm::LevType::SFC}}),                    //
                setKey(&dm::FullMarsRecord::levtype, dm::LevType::SOL), setKey(&dm::FullMarsRecord::levelist, 4),  //
                setKey(&dm::FullMarsRecord::param, 260360));                                                       //
}


//-----------------------------------------------------------------------------
// Tables Version fixes
//-----------------------------------------------------------------------------

auto fixTablesVersionChemAccum() {
    return rule(all(matchParams(228080, 228081, 228082, 233032, 233033, 233034, 233035, 235062, 235063, 235064)),  //
                setKey(&dm::MiscRecord::tablesVersion, 30));                                                       //
}
auto fixTablesVersionChemInstant() {
    return rule(all(matchParams(228083, 228084, 228085)),     //
                setKey(&dm::MiscRecord::tablesVersion, 30));  //
}


//-----------------------------------------------------------------------------
// Cloud cover
//-----------------------------------------------------------------------------

auto fixCloudParam164() {
    return rule(all(matchParams(164)),                                               //
                setKey(&dm::FullMarsRecord::param, 228164), SetScaleFactor{100.0});  //
}
auto fixCloudParam186() {
    return rule(all(matchParams(186)),                                             //
                setKey(&dm::FullMarsRecord::param, 3073), SetScaleFactor{100.0});  //
}
auto fixCloudParam187() {
    return rule(all(matchParams(187)),                                             //
                setKey(&dm::FullMarsRecord::param, 3074), SetScaleFactor{100.0});  //
}
auto fixCloudParam188() {
    return rule(all(matchParams(188)),                                             //
                setKey(&dm::FullMarsRecord::param, 3075), SetScaleFactor{100.0});  //
}


//-----------------------------------------------------------------------------
// Precipitation
//-----------------------------------------------------------------------------

auto fixConvectivePrecip143() {
    return rule(all(matchParams(143)),                                                //
                setKey(&dm::FullMarsRecord::param, 228143), SetScaleFactor{1000.0});  //
}

auto fixTotalPrecip228() {
    return rule(all(matchParams(228)),                                                //
                setKey(&dm::FullMarsRecord::param, 228228), SetScaleFactor{1000.0});  //
}


//-----------------------------------------------------------------------------
// Height above
//-----------------------------------------------------------------------------

auto fixHeightAboveGround2m() {
    return rule(all(OneOf{&dm::FullMarsRecord::levtype, {dm::LevType::SFC}}, OneOf{&dm::FullMarsRecord::levelist, {0}},
                    matchParams(167, 168, 174096, 228037)),  //
                setKey(&dm::FullMarsRecord::levelist, 2));   //
}

auto fixHeightAboveGround10m() {
    return rule(all(OneOf{&dm::FullMarsRecord::levtype, {dm::LevType::SFC}}, OneOf{&dm::FullMarsRecord::levelist, {0}},
                    matchParams(228029, 228131, 228132, 165, 166, 207)),  //
                setKey(&dm::FullMarsRecord::levelist, 10));               //
}

auto fixHeightAboveSea() {
    return rule(all(matchParams(140233, 140245, 140249)),    //
                setKey(&dm::FullMarsRecord::levelist, 10));  //
}


//-----------------------------------------------------------------------------
// Misc level fixes
//-----------------------------------------------------------------------------

auto fixMapToSol() {
    return rule(all(OneOf{&dm::FullMarsRecord::levtype, {dm::LevType::SFC}}, Has{&dm::FullMarsRecord::levelist},
                    NoneOf{&dm::FullMarsRecord::levelist, {0}},
                    matchParams(33, 238, 228038, 228141, 235080, 237080, 238080, 239080, 260360, 262000, 262024, 260199)),  //
                setKey(&dm::FullMarsRecord::levtype, dm::LevType::SOL));                                          //
}


auto fixRunOffWaterParam205() {
    return rule(all(matchParams(205)),                                                //
                setKey(&dm::FullMarsRecord::param, 231002), SetScaleFactor{1000.0});  //
}

auto fixSnowfallWaterEquivParam() {
    return rule(all(matchParams(144)),                                                //
                setKey(&dm::FullMarsRecord::param, 228144), SetScaleFactor{1000.0});  //
}


auto fixSnowCover() {
    return rule(all(matchParams(260289)),                                            //
                setKey(&dm::FullMarsRecord::param, 260038), SetScaleFactor{100.0});  //
}


//-----------------------------------------------------------------------------
// Fix Timespan
//-----------------------------------------------------------------------------

auto fixTimespanMax2T() {
    return rule(all(matchParams(121, 228026, 201)),           //
                setKey(&dm::FullMarsRecord::param, 237167));  //
}

auto fixTimespanMaxCape() {
    return rule(all(matchParams(228035)),                     //
                setKey(&dm::FullMarsRecord::param, 237117));  //
}

auto fixTimespanMaxMuCapes() {
    return rule(all(matchParams(228036)),                     //
                setKey(&dm::FullMarsRecord::param, 237321));  //
}

auto fixTimespanMaxPrecipRate() {
    return rule(all(matchParams(228222, 228224, 228226)),     //
                setKey(&dm::FullMarsRecord::param, 237055));  //
}

auto fixTimespanMaxWindGust() {
    return rule(all(matchParams(123, 228028, 49)),            //
                setKey(&dm::FullMarsRecord::param, 237318));  //
}

auto fixTimespanMeanFlashDensity() {
    return rule(all(matchParams(228051, 228057, 228058)),     //
                setKey(&dm::FullMarsRecord::param, 235326));  //
}

auto fixTimespanMin2T() {
    return rule(all(matchParams(122, 228027, 202)),           //
                setKey(&dm::FullMarsRecord::param, 238167));  //
}

auto fixTimespanMinPrecipRate() {
    return rule(all(matchParams(228223, 228225, 228227)),     //
                setKey(&dm::FullMarsRecord::param, 238055));  //
}

auto fixTimespanModePrecip() {
    return rule(all(matchParams(260320, 260321, 260339)),     //
                setKey(&dm::FullMarsRecord::param, 260683));  //
}

auto fixTimespanSeverityPrecip() {
    return rule(all(matchParams(260318, 260319, 260338)),     //
                setKey(&dm::FullMarsRecord::param, 260682));  //
}


//-----------------------------------------------------------------------------
// Fix Windspeed
//-----------------------------------------------------------------------------

auto fixWindspeedU100m() {
    return rule(all(OneOf{&dm::FullMarsRecord::levtype, {dm::LevType::SFC}}, matchParams(228246)),  //
                setKey(&dm::FullMarsRecord::levtype, dm::LevType::HL), setKey(&dm::FullMarsRecord::levelist, 100),
                setKey(&dm::FullMarsRecord::param, 131));  //
}

auto fixWindspeedU200m() {
    return rule(all(OneOf{&dm::FullMarsRecord::levtype, {dm::LevType::SFC}}, matchParams(228239)),  //
                setKey(&dm::FullMarsRecord::levtype, dm::LevType::HL), setKey(&dm::FullMarsRecord::levelist, 200),
                setKey(&dm::FullMarsRecord::param, 131));  //
}

auto fixWindspeedV100m() {
    return rule(all(OneOf{&dm::FullMarsRecord::levtype, {dm::LevType::SFC}}, matchParams(228247)),  //
                setKey(&dm::FullMarsRecord::levtype, dm::LevType::HL), setKey(&dm::FullMarsRecord::levelist, 100),
                setKey(&dm::FullMarsRecord::param, 132));  //
}

auto fixWindspeedV200m() {
    return rule(all(OneOf{&dm::FullMarsRecord::levtype, {dm::LevType::SFC}}, matchParams(228240)),  //
                setKey(&dm::FullMarsRecord::levtype, dm::LevType::HL), setKey(&dm::FullMarsRecord::levelist, 200),
                setKey(&dm::FullMarsRecord::param, 132));  //
}

auto fixWindspeed100m() {
    return rule(all(OneOf{&dm::FullMarsRecord::levtype, {dm::LevType::SFC}}, matchParams(228241)),  //
                setKey(&dm::FullMarsRecord::levtype, dm::LevType::HL), setKey(&dm::FullMarsRecord::levelist, 200),
                setKey(&dm::FullMarsRecord::param, 10));  //
}

auto fixWindspeed200m() {
    return rule(all(OneOf{&dm::FullMarsRecord::levtype, {dm::LevType::SFC}}, matchParams(228249)),  //
                setKey(&dm::FullMarsRecord::levtype, dm::LevType::HL), setKey(&dm::FullMarsRecord::levelist, 100),
                setKey(&dm::FullMarsRecord::param, 10));  //
}


//-----------------------------------------------------------------------------
// Wave rules bits per value
//-----------------------------------------------------------------------------


auto ruleWaveBitsPerValueS1() {
    return rule(all(greaterThan(&dm::FullMarsRecord::step, std::chrono::hours{1}),
                    matchParams(22824)),                     //
                setKey(&dm::MiscRecord::bitsPerValue, 24));  //
}

auto ruleWaveBitsPerValue() {
    return rule(all(matchParams(paramRange(140098, 140105), paramRange(140112, 140129), paramRange(140207, 140209),
                                paramRange(140211, 140212), paramRange(140214, 140232), paramRange(140234, 140239),
                                140244, paramRange(140251, 140254))),  //
                setKey(&dm::MiscRecord::bitsPerValue, 24));            //
}


//-----------------------------------------------------------------------------
// Atmospheric Composition parameter mappings
//-----------------------------------------------------------------------------

// clang-format off
const RuleList& mapAtmosphericComposition() {
    static auto rules_ = ruleList(
        rule(all(matchParams(210207)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 550)),
        rule(all(matchParams(210213)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 469)),
        rule(all(matchParams(210214)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 670)),
        rule(all(matchParams(210215)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 865)),
        rule(all(matchParams(210216)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 1240)),
        rule(all(matchParams(210217)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 340)),
        rule(all(matchParams(210218)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 355)),
        rule(all(matchParams(210219)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 380)),
        rule(all(matchParams(210220)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 400)),
        rule(all(matchParams(210221)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 440)),
        rule(all(matchParams(210222)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 500)),
        rule(all(matchParams(210223)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 532)),
        rule(all(matchParams(210224)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 645)),
        rule(all(matchParams(210225)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 800)),
        rule(all(matchParams(210226)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 858)),
        rule(all(matchParams(210227)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 1020)),
        rule(all(matchParams(210228)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 1064)),
        rule(all(matchParams(210229)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 1640)),
        rule(all(matchParams(210230)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 2130)),
        rule(all(matchParams(215096)), setKey(&dm::FullMarsRecord::param, 472000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 340)),
        rule(all(matchParams(215097)), setKey(&dm::FullMarsRecord::param, 472000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 355)),
        rule(all(matchParams(215098)), setKey(&dm::FullMarsRecord::param, 472000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 380)),
        rule(all(matchParams(215099)), setKey(&dm::FullMarsRecord::param, 472000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 400)),
        rule(all(matchParams(215100)), setKey(&dm::FullMarsRecord::param, 472000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 440)),
        rule(all(matchParams(215101)), setKey(&dm::FullMarsRecord::param, 472000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 469)),
        rule(all(matchParams(215102)), setKey(&dm::FullMarsRecord::param, 472000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 500)),
        rule(all(matchParams(215103)), setKey(&dm::FullMarsRecord::param, 472000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 532)),
        rule(all(matchParams(215104)), setKey(&dm::FullMarsRecord::param, 472000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 550)),
        rule(all(matchParams(215105)), setKey(&dm::FullMarsRecord::param, 472000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 645)),
        rule(all(matchParams(215106)), setKey(&dm::FullMarsRecord::param, 472000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 670)),
        rule(all(matchParams(215107)), setKey(&dm::FullMarsRecord::param, 472000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 800)),
        rule(all(matchParams(215108)), setKey(&dm::FullMarsRecord::param, 472000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 858)),
        rule(all(matchParams(215109)), setKey(&dm::FullMarsRecord::param, 472000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 865)),
        rule(all(matchParams(215110)), setKey(&dm::FullMarsRecord::param, 472000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 1020)),
        rule(all(matchParams(215111)), setKey(&dm::FullMarsRecord::param, 472000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 1064)),
        rule(all(matchParams(215112)), setKey(&dm::FullMarsRecord::param, 472000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 1240)),
        rule(all(matchParams(215113)), setKey(&dm::FullMarsRecord::param, 472000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 1640)),
        rule(all(matchParams(215114)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 923), setKey(&dm::FullMarsRecord::wavelength, 340)),
        rule(all(matchParams(215115)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 923), setKey(&dm::FullMarsRecord::wavelength, 355)),
        rule(all(matchParams(215116)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 923), setKey(&dm::FullMarsRecord::wavelength, 380)),
        rule(all(matchParams(215117)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 923), setKey(&dm::FullMarsRecord::wavelength, 400)),
        rule(all(matchParams(215118)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 923), setKey(&dm::FullMarsRecord::wavelength, 440)),
        rule(all(matchParams(215119)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 923), setKey(&dm::FullMarsRecord::wavelength, 469)),
        rule(all(matchParams(215120)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 923), setKey(&dm::FullMarsRecord::wavelength, 500)),
        rule(all(matchParams(215121)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 923), setKey(&dm::FullMarsRecord::wavelength, 532)),
        rule(all(matchParams(215122)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 923), setKey(&dm::FullMarsRecord::wavelength, 550)),
        rule(all(matchParams(215123)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 923), setKey(&dm::FullMarsRecord::wavelength, 645)),
        rule(all(matchParams(215124)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 923), setKey(&dm::FullMarsRecord::wavelength, 670)),
        rule(all(matchParams(215125)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 923), setKey(&dm::FullMarsRecord::wavelength, 800)),
        rule(all(matchParams(215126)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 923), setKey(&dm::FullMarsRecord::wavelength, 858)),
        rule(all(matchParams(215127)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 923), setKey(&dm::FullMarsRecord::wavelength, 865)),
        rule(all(matchParams(215128)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 923), setKey(&dm::FullMarsRecord::wavelength, 1020)),
        rule(all(matchParams(215129)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 923), setKey(&dm::FullMarsRecord::wavelength, 1064)),
        rule(all(matchParams(215130)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 923), setKey(&dm::FullMarsRecord::wavelength, 1240)),
        rule(all(matchParams(215131)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 923), setKey(&dm::FullMarsRecord::wavelength, 1640)),
        rule(all(matchParams(215132)), setKey(&dm::FullMarsRecord::param, 458000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 340)),
        rule(all(matchParams(215133)), setKey(&dm::FullMarsRecord::param, 458000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 355)),
        rule(all(matchParams(215134)), setKey(&dm::FullMarsRecord::param, 458000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 380)),
        rule(all(matchParams(215135)), setKey(&dm::FullMarsRecord::param, 458000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 400)),
        rule(all(matchParams(215136)), setKey(&dm::FullMarsRecord::param, 458000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 440)),
        rule(all(matchParams(215137)), setKey(&dm::FullMarsRecord::param, 458000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 469)),
        rule(all(matchParams(215138)), setKey(&dm::FullMarsRecord::param, 458000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 500)),
        rule(all(matchParams(215139)), setKey(&dm::FullMarsRecord::param, 458000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 532)),
        rule(all(matchParams(215140)), setKey(&dm::FullMarsRecord::param, 458000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 550)),
        rule(all(matchParams(215141)), setKey(&dm::FullMarsRecord::param, 458000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 645)),
        rule(all(matchParams(215142)), setKey(&dm::FullMarsRecord::param, 458000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 670)),
        rule(all(matchParams(215143)), setKey(&dm::FullMarsRecord::param, 458000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 800)),
        rule(all(matchParams(215144)), setKey(&dm::FullMarsRecord::param, 458000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 858)),
        rule(all(matchParams(215145)), setKey(&dm::FullMarsRecord::param, 458000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 865)),
        rule(all(matchParams(215146)), setKey(&dm::FullMarsRecord::param, 458000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 1020)),
        rule(all(matchParams(215147)), setKey(&dm::FullMarsRecord::param, 458000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 1064)),
        rule(all(matchParams(215148)), setKey(&dm::FullMarsRecord::param, 458000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 1240)),
        rule(all(matchParams(215149)), setKey(&dm::FullMarsRecord::param, 458000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 1640)),
        rule(all(matchParams(215150)), setKey(&dm::FullMarsRecord::param, 459000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 340)),
        rule(all(matchParams(215151)), setKey(&dm::FullMarsRecord::param, 459000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 355)),
        rule(all(matchParams(215152)), setKey(&dm::FullMarsRecord::param, 459000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 380)),
        rule(all(matchParams(215153)), setKey(&dm::FullMarsRecord::param, 459000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 400)),
        rule(all(matchParams(215154)), setKey(&dm::FullMarsRecord::param, 459000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 440)),
        rule(all(matchParams(215155)), setKey(&dm::FullMarsRecord::param, 459000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 469)),
        rule(all(matchParams(215156)), setKey(&dm::FullMarsRecord::param, 459000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 500)),
        rule(all(matchParams(215157)), setKey(&dm::FullMarsRecord::param, 459000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 532)),
        rule(all(matchParams(215158)), setKey(&dm::FullMarsRecord::param, 459000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 550)),
        rule(all(matchParams(215159)), setKey(&dm::FullMarsRecord::param, 459000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 645)),
        rule(all(matchParams(215160)), setKey(&dm::FullMarsRecord::param, 459000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 670)),
        rule(all(matchParams(215161)), setKey(&dm::FullMarsRecord::param, 459000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 800)),
        rule(all(matchParams(215162)), setKey(&dm::FullMarsRecord::param, 459000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 858)),
        rule(all(matchParams(215163)), setKey(&dm::FullMarsRecord::param, 459000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 865)),
        rule(all(matchParams(215164)), setKey(&dm::FullMarsRecord::param, 459000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 1020)),
        rule(all(matchParams(215165)), setKey(&dm::FullMarsRecord::param, 459000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 1064)),
        rule(all(matchParams(215166)), setKey(&dm::FullMarsRecord::param, 459000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 1240)),
        rule(all(matchParams(215167)), setKey(&dm::FullMarsRecord::param, 459000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 1640)),
        rule(all(matchParams(215176)), setKey(&dm::FullMarsRecord::param, 472000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 2130)),
        rule(all(matchParams(215177)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 923), setKey(&dm::FullMarsRecord::wavelength, 2130)),
        rule(all(matchParams(215178)), setKey(&dm::FullMarsRecord::param, 458000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 2130)),
        rule(all(matchParams(215179)), setKey(&dm::FullMarsRecord::param, 459000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 2130)),
        rule(all(matchParams(216008)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 10000)),
        rule(all(matchParams(216009)), setKey(&dm::FullMarsRecord::param, 472000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 10000)),
        rule(all(matchParams(216010)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 923), setKey(&dm::FullMarsRecord::wavelength, 10000)),
        rule(all(matchParams(216011)), setKey(&dm::FullMarsRecord::param, 458000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 10000)),
        rule(all(matchParams(216012)), setKey(&dm::FullMarsRecord::param, 459000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 10000))
    );
    return rules_;
}
// clang-format on


//-----------------------------------------------------------------------------
// Map incremental fields for type=4i
//-----------------------------------------------------------------------------

const RuleList& incrementalType4IRules() {
    static auto type4i_ = ruleList(rule(matchParams(200130), setKey(&dm::FullMarsRecord::param, 130)),
                                   rule(matchParams(200133), setKey(&dm::FullMarsRecord::param, 133)),
                                   rule(matchParams(200138), setKey(&dm::FullMarsRecord::param, 138)),
                                   rule(matchParams(200152), setKey(&dm::FullMarsRecord::param, 152)),
                                   rule(matchParams(200155), setKey(&dm::FullMarsRecord::param, 155)),
                                   rule(matchParams(200203), setKey(&dm::FullMarsRecord::param, 203)));
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
    static auto all_
        = ruleList(incrementalType4IRules(), fixIFSOutput(), mapDeprecatedGrib1ToGrib2(), mapAtmosphericComposition());
    return all_;
}

const RuleList& allRules() {
    static auto all_ = ruleList(wmoUnitMapping(), incrementalType4IRules(), fixIFSOutput(), mapDeprecatedGrib1ToGrib2(),
                                mapAtmosphericComposition());
    return all_;
}

std::optional<MappingResult> applyMappings(const RuleList& rules, dm::FullMarsRecord& mars, dm::MiscRecord& misc) {
    return rules(mars, misc);
}

}  // namespace multio::mars2mars

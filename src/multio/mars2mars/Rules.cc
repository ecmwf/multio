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
    return rule(
        all(OneOf{&dm::FullMarsRecord::levtype, {dm::LevType::SFC}}, Has{&dm::FullMarsRecord::levelist},
            NoneOf{&dm::FullMarsRecord::levelist, {0}},
            matchParams(33, 238, 228038, 235080, 237080, 238080, 239080, 260360, 262000, 262024, 260199)),  //
        setKey(&dm::FullMarsRecord::levtype, dm::LevType::SOL));                                             //
}


auto fixRunOffWaterParam205() {
    return rule(all(matchParams(205)),                                                //
                setKey(&dm::FullMarsRecord::param, 231002), SetScaleFactor{1000.0});  //
}

auto fixSnowfallWaterEquivParam() {
    return rule(all(matchParams(144)),                                                //
                setKey(&dm::FullMarsRecord::param, 228144), SetScaleFactor{1000.0});  //
}


//-----------------------------------------------------------------------------
// Runoff and water cycle (m -> mm)
//-----------------------------------------------------------------------------

// Surface runoff: sro -> surfro
auto fixSurfaceRunoff8() {
    return rule(all(matchParams(8)),                                                  //
                setKey(&dm::FullMarsRecord::param, 231010), SetScaleFactor{1000.0});  //
}

// Sub-surface runoff: ssro -> ssurfro
auto fixSubSurfaceRunoff9() {
    return rule(all(matchParams(9)),                                                  //
                setKey(&dm::FullMarsRecord::param, 231012), SetScaleFactor{1000.0});  //
}

// Snow depth: sd -> sd (water equivalent)
auto fixSnowDepth141() {
    return rule(all(matchParams(141)),                                                //
                setKey(&dm::FullMarsRecord::param, 228141), SetScaleFactor{1000.0});  //
}

// Large-scale precipitation: lsp -> lsp
auto fixLargeScalePrecip142() {
    return rule(all(matchParams(142)),                                              //
                setKey(&dm::FullMarsRecord::param, 3062), SetScaleFactor{1000.0});  //
}

// Evaporation: e -> eva
auto fixEvaporation182() {
    return rule(all(matchParams(182)),                                                //
                setKey(&dm::FullMarsRecord::param, 260259), SetScaleFactor{1000.0});  //
}

// Skin reservoir content: src -> srcon
auto fixSkinReservoirContent198() {
    return rule(all(matchParams(198)),                                                //
                setKey(&dm::FullMarsRecord::param, 160198), SetScaleFactor{1000.0});  //
}

// Convective snowfall: csf -> csfwe
auto fixConvectiveSnowfall239() {
    return rule(all(matchParams(239)),                                                //
                setKey(&dm::FullMarsRecord::param, 231057), SetScaleFactor{1000.0});  //
}

// Large-scale snowfall: lsf -> lsfwe
auto fixLargeScaleSnowfall240() {
    return rule(all(matchParams(240)),                                                //
                setKey(&dm::FullMarsRecord::param, 231058), SetScaleFactor{1000.0});  //
}

// Accumulated freezing rain: fzra -> fzrawe
auto fixFreezingRain228216() {
    return rule(all(matchParams(228216)),                                             //
                setKey(&dm::FullMarsRecord::param, 231001), SetScaleFactor{1000.0});  //
}


//-----------------------------------------------------------------------------
// Albedo (fraction -> %)
// Required for ERA
//-----------------------------------------------------------------------------

// UV visible albedo for direct radiation: aluvp -> aluvp_p
auto fixAlbedoUvDirect15() {
    return rule(all(matchParams(15)),                                                //
                setKey(&dm::FullMarsRecord::param, 210199), SetScaleFactor{100.0});  //
}

// UV visible albedo for diffuse radiation: aluvd -> aluvd_p
auto fixAlbedoUvDiffuse16() {
    return rule(all(matchParams(16)),                                                //
                setKey(&dm::FullMarsRecord::param, 210198), SetScaleFactor{100.0});  //
}

// Near IR albedo for direct radiation: alnip -> alnip_p
auto fixAlbedoNirDirect17() {
    return rule(all(matchParams(17)),                                                //
                setKey(&dm::FullMarsRecord::param, 210261), SetScaleFactor{100.0});  //
}

// Near IR albedo for diffuse radiation: alnid -> alnid_p
auto fixAlbedoNirDiffuse18() {
    return rule(all(matchParams(18)),                                                //
                setKey(&dm::FullMarsRecord::param, 210260), SetScaleFactor{100.0});  //
}

// Snow albedo: asn -> asn
auto fixSnowAlbedo32() {
    return rule(all(matchParams(32)),                                                //
                setKey(&dm::FullMarsRecord::param, 228032), SetScaleFactor{100.0});  //
}

// Albedo (climatological): al -> al
auto fixAlbedoClimatological174() {
    return rule(all(matchParams(174)),                                               //
                setKey(&dm::FullMarsRecord::param, 260509), SetScaleFactor{100.0});  //
}

// Forecast albedo: fal -> al
auto fixForecastAlbedo243() {
    return rule(all(matchParams(243)),                                               //
                setKey(&dm::FullMarsRecord::param, 260509), SetScaleFactor{100.0});  //
}

// UV visible albedo for direct radiation, isotropic: aluvpi -> aluvpi_p
auto fixAlbedoUvDirectIso210186() {
    return rule(all(matchParams(210186)),                                            //
                setKey(&dm::FullMarsRecord::param, 210201), SetScaleFactor{100.0});  //
}

// UV visible albedo for direct radiation, volumetric: aluvpv -> aluvpv_p
auto fixAlbedoUvDirectVol210187() {
    return rule(all(matchParams(210187)),                                            //
                setKey(&dm::FullMarsRecord::param, 210202), SetScaleFactor{100.0});  //
}

// UV visible albedo for direct radiation, geometric: aluvpg -> aluvpg_p
auto fixAlbedoUvDirectGeo210188() {
    return rule(all(matchParams(210188)),                                            //
                setKey(&dm::FullMarsRecord::param, 210200), SetScaleFactor{100.0});  //
}

// Near IR albedo for direct radiation, isotropic: alnipi -> alnipi_p
auto fixAlbedoNirDirectIso210189() {
    return rule(all(matchParams(210189)),                                            //
                setKey(&dm::FullMarsRecord::param, 210263), SetScaleFactor{100.0});  //
}

// Near IR albedo for direct radiation, volumetric: alnipv -> alnipv_p
auto fixAlbedoNirDirectVol210190() {
    return rule(all(matchParams(210190)),                                            //
                setKey(&dm::FullMarsRecord::param, 210264), SetScaleFactor{100.0});  //
}

// Near IR albedo for direct radiation, geometric: alnipg -> alnipg_p
auto fixAlbedoNirDirectGeo210191() {
    return rule(all(matchParams(210191)),                                            //
                setKey(&dm::FullMarsRecord::param, 210262), SetScaleFactor{100.0});  //
}


//-----------------------------------------------------------------------------
// ERA other water-cycle parameters
//-----------------------------------------------------------------------------

// Snow evaporation: es -> eswe
auto fixSnowEvaporation44() {
    return rule(all(matchParams(44)),                                                 //
                setKey(&dm::FullMarsRecord::param, 231003), SetScaleFactor{1000.0});  //
}

// Snowmelt: smlt -> snom
auto fixSnowmelt45() {
    return rule(all(matchParams(45)),                                               //
                setKey(&dm::FullMarsRecord::param, 3099), SetScaleFactor{1000.0});  //
}

// Total column ozone: tco3 -> tcioz (kg m-2 -> DU, 1 DU = 2.1415e-5 kg m-2)
auto fixTotalColumnOzone206() {
    return rule(all(matchParams(206)),                                                  //
                setKey(&dm::FullMarsRecord::param, 260132), SetScaleFactor{46698.05});  //
}

// Potential evaporation: pev -> peva
auto fixPotentialEvaporation228251() {
    return rule(all(matchParams(228251)),                                             //
                setKey(&dm::FullMarsRecord::param, 231005), SetScaleFactor{1000.0});  //
}


//-----------------------------------------------------------------------------
// Time-mean rate parameters (m/s -> mm/s)
//-----------------------------------------------------------------------------

// Time-mean surface runoff rate: msror -> avg_surfror
auto fixTimeMeanSurfaceRunoffRate172008() {
    return rule(all(matchParams(172008)),                                             //
                setKey(&dm::FullMarsRecord::param, 235020), SetScaleFactor{1000.0});  //
}

// Time-mean sub-surface runoff rate: mssror -> avg_ssurfror
auto fixTimeMeanSubSurfaceRunoffRate172009() {
    return rule(all(matchParams(172009)),                                             //
                setKey(&dm::FullMarsRecord::param, 235021), SetScaleFactor{1000.0});  //
}

// Time-mean snow evaporation rate: esrate -> avg_esrwe
auto fixTimeMeanSnowEvapRate172044() {
    return rule(all(matchParams(172044)),                                             //
                setKey(&dm::FullMarsRecord::param, 235023), SetScaleFactor{1000.0});  //
}

// Time-mean snowmelt rate -> avg_smr
auto fixTimeMeanSnowmeltRate172045() {
    return rule(all(matchParams(172045)),                                             //
                setKey(&dm::FullMarsRecord::param, 235024), SetScaleFactor{1000.0});  //
}

// Time-mean large-scale precipitation rate: mlsprt -> avg_lsprate
auto fixTimeMeanLargeScalePrecipRate172142() {
    return rule(all(matchParams(172142)),                                             //
                setKey(&dm::FullMarsRecord::param, 235029), SetScaleFactor{1000.0});  //
}

// Time-mean convective precipitation rate: cprate -> avg_cpr
auto fixTimeMeanConvPrecipRate172143() {
    return rule(all(matchParams(172143)),                                             //
                setKey(&dm::FullMarsRecord::param, 235030), SetScaleFactor{1000.0});  //
}

// Time-mean total snowfall rate: mtsfr -> avg_tsrwe
auto fixTimeMeanTotalSnowfallRate172144() {
    return rule(all(matchParams(172144)),                                             //
                setKey(&dm::FullMarsRecord::param, 235031), SetScaleFactor{1000.0});  //
}

// Time-mean evaporation rate: erate -> avg_ie
auto fixTimeMeanEvaporationRate172182() {
    return rule(all(matchParams(172182)),                                             //
                setKey(&dm::FullMarsRecord::param, 235043), SetScaleFactor{1000.0});  //
}

// Time-mean runoff rate: mrort -> avg_rorwe
auto fixTimeMeanRunoffRate172205() {
    return rule(all(matchParams(172205)),                                             //
                setKey(&dm::FullMarsRecord::param, 235048), SetScaleFactor{1000.0});  //
}

// Time-mean total precipitation rate: tprate -> avg_tprate
auto fixTimeMeanTotalPrecipRate172228() {
    return rule(all(matchParams(172228)),                                             //
                setKey(&dm::FullMarsRecord::param, 235055), SetScaleFactor{1000.0});  //
}

// Time-mean snow depth: avg_sd_m -> avg_sd
auto fixTimeMeanSnowDepth235141() {
    return rule(all(matchParams(235141)),                                             //
                setKey(&dm::FullMarsRecord::param, 235078), SetScaleFactor{1000.0});  //
}


//-----------------------------------------------------------------------------
// Time-mean cloud cover and albedo (fraction -> %)
// New in ecCodes 2.38
//-----------------------------------------------------------------------------

// Time-mean low cloud cover: avg_lcc_frac -> avg_lcc
auto fixTimeMeanLowCloudCover235186() {
    return rule(all(matchParams(235186)),                                            //
                setKey(&dm::FullMarsRecord::param, 235108), SetScaleFactor{100.0});  //
}

// Time-mean medium cloud cover: avg_mcc_frac -> avg_mcc
auto fixTimeMeanMedCloudCover235187() {
    return rule(all(matchParams(235187)),                                            //
                setKey(&dm::FullMarsRecord::param, 235109), SetScaleFactor{100.0});  //
}

// Time-mean high cloud cover: avg_hcc_frac -> avg_hcc
auto fixTimeMeanHighCloudCover235188() {
    return rule(all(matchParams(235188)),                                            //
                setKey(&dm::FullMarsRecord::param, 235110), SetScaleFactor{100.0});  //
}

// Time-mean forecast albedo: avg_fal_frac -> avg_al
auto fixTimeMeanForecastAlbedo235243() {
    return rule(all(matchParams(235243)),                                            //
                setKey(&dm::FullMarsRecord::param, 235263), SetScaleFactor{100.0});  //
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
    return rule(all(greaterThan(&dm::FullMarsRecord::step, dm::TimeDuration{std::chrono::hours{1}}),
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
        rule(all(matchParams(210001)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 901)),
        rule(all(matchParams(210002)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 902)),
        rule(all(matchParams(210003)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 903)),
        rule(all(matchParams(210004)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 904)),
        rule(all(matchParams(210005)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 905)),
        rule(all(matchParams(210006)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 906)),
        rule(all(matchParams(210007)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 907)),
        rule(all(matchParams(210008)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 908)),
        rule(all(matchParams(210009)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 909)),
        rule(all(matchParams(210010)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 910)),
        rule(all(matchParams(210011)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 911)),
        rule(all(matchParams(210022)), setKey(&dm::FullMarsRecord::param, 453000), setKey(&dm::FullMarsRecord::chem, 922)),
        rule(all(matchParams(210025)), setKey(&dm::FullMarsRecord::param, 453000), setKey(&dm::FullMarsRecord::chem, 922)),
        rule(all(matchParams(210048)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 924)),
        rule(all(matchParams(210071)), setKey(&dm::FullMarsRecord::param, 479000), setKey(&dm::FullMarsRecord::chem, 404)),
        rule(all(matchParams(210072)), setKey(&dm::FullMarsRecord::param, 400000), setKey(&dm::FullMarsRecord::chem, 929)),
        rule(all(matchParams(210073)), setKey(&dm::FullMarsRecord::param, 400000), setKey(&dm::FullMarsRecord::chem, 930)),
        rule(all(matchParams(210074)), setKey(&dm::FullMarsRecord::param, 400000), setKey(&dm::FullMarsRecord::chem, 931)),
        rule(all(matchParams(210081)), setKey(&dm::FullMarsRecord::param, 469000), setKey(&dm::FullMarsRecord::chem, 2)),
        rule(all(matchParams(210085)), setKey(&dm::FullMarsRecord::param, 469000), setKey(&dm::FullMarsRecord::chem, 129)),
        rule(all(matchParams(210102)), setKey(&dm::FullMarsRecord::param, 469000), setKey(&dm::FullMarsRecord::chem, 233)),
        rule(all(matchParams(210103)), setKey(&dm::FullMarsRecord::param, 469000), setKey(&dm::FullMarsRecord::chem, 42)),
        rule(all(matchParams(210104)), setKey(&dm::FullMarsRecord::param, 469000), setKey(&dm::FullMarsRecord::chem, 46)),
        rule(all(matchParams(210105)), setKey(&dm::FullMarsRecord::param, 469000), setKey(&dm::FullMarsRecord::chem, 47)),
        rule(all(matchParams(210106)), setKey(&dm::FullMarsRecord::param, 469000), setKey(&dm::FullMarsRecord::chem, 10)),
        rule(all(matchParams(210107)), setKey(&dm::FullMarsRecord::param, 469000), setKey(&dm::FullMarsRecord::chem, 48)),
        rule(all(matchParams(210108)), setKey(&dm::FullMarsRecord::param, 469000), setKey(&dm::FullMarsRecord::chem, 16)),
        rule(all(matchParams(210113)), setKey(&dm::FullMarsRecord::param, 469000), setKey(&dm::FullMarsRecord::chem, 5)),
        rule(all(matchParams(210115)), setKey(&dm::FullMarsRecord::param, 469000), setKey(&dm::FullMarsRecord::chem, 52)),
        rule(all(matchParams(210116)), setKey(&dm::FullMarsRecord::param, 469000), setKey(&dm::FullMarsRecord::chem, 19)),
        rule(all(matchParams(210117)), setKey(&dm::FullMarsRecord::param, 469000), setKey(&dm::FullMarsRecord::chem, 18)),
        rule(all(matchParams(210118)), setKey(&dm::FullMarsRecord::param, 469000), setKey(&dm::FullMarsRecord::chem, 45)),
        rule(all(matchParams(210121)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 17)),
        rule(all(matchParams(210122)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 233)),
        rule(all(matchParams(210123)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 2)),
        rule(all(matchParams(210124)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 5)),
        rule(all(matchParams(210125)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 17)),
        rule(all(matchParams(210126)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 233)),
        rule(all(matchParams(210127)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 2)),
        rule(all(matchParams(210128)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 5)),
        rule(all(matchParams(210170)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 8)),
        rule(all(matchParams(210181)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 11)),
        rule(all(matchParams(210183)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 11)),
        rule(all(matchParams(210203)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 236)),
        rule(all(matchParams(210206)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 231)),
        rule(all(matchParams(210207)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 550)),
        rule(all(matchParams(210208)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 936), setKey(&dm::FullMarsRecord::wavelength, 550)),
        rule(all(matchParams(210209)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 935), setKey(&dm::FullMarsRecord::wavelength, 550)),
        rule(all(matchParams(210210)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 934), setKey(&dm::FullMarsRecord::wavelength, 550)),
        rule(all(matchParams(210211)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 933), setKey(&dm::FullMarsRecord::wavelength, 550)),
        rule(all(matchParams(210212)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 911), setKey(&dm::FullMarsRecord::wavelength, 550)),
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
        rule(all(matchParams(210247)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 912)),
        rule(all(matchParams(210248)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 913)),
        rule(all(matchParams(210249)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 21)),
        rule(all(matchParams(210250)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 900), setKey(&dm::FullMarsRecord::wavelength, 550)),
        rule(all(matchParams(210251)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 914), setKey(&dm::FullMarsRecord::wavelength, 550)),
        rule(all(matchParams(210252)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 915)),
        rule(all(matchParams(210253)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 916)),
        rule(all(matchParams(211001)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 901)),
        rule(all(matchParams(211002)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 902)),
        rule(all(matchParams(211003)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 903)),
        rule(all(matchParams(211004)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 904)),
        rule(all(matchParams(211005)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 905)),
        rule(all(matchParams(211006)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 906)),
        rule(all(matchParams(211007)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 907)),
        rule(all(matchParams(211008)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 908)),
        rule(all(matchParams(211009)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 909)),
        rule(all(matchParams(211010)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 910)),
        rule(all(matchParams(211011)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 911)),
        rule(all(matchParams(211048)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 924)),
        rule(all(matchParams(211121)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 17)),
        rule(all(matchParams(211122)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 233)),
        rule(all(matchParams(211123)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 2)),
        rule(all(matchParams(211124)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 5)),
        rule(all(matchParams(211170)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 8)),
        rule(all(matchParams(211203)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 236)),
        rule(all(matchParams(211247)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 900)),
        rule(all(matchParams(211248)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 903)),
        rule(all(matchParams(211249)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 914)),
        rule(all(matchParams(211252)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 915)),
        rule(all(matchParams(211253)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 916)),
        rule(all(matchParams(215001)), setKey(&dm::FullMarsRecord::param, 453000), setKey(&dm::FullMarsRecord::chem, 901)),
        rule(all(matchParams(215002)), setKey(&dm::FullMarsRecord::param, 453000), setKey(&dm::FullMarsRecord::chem, 902)),
        rule(all(matchParams(215003)), setKey(&dm::FullMarsRecord::param, 453000), setKey(&dm::FullMarsRecord::chem, 903)),
        rule(all(matchParams(215004)), setKey(&dm::FullMarsRecord::param, 406000), setKey(&dm::FullMarsRecord::chem, 901)),
        rule(all(matchParams(215005)), setKey(&dm::FullMarsRecord::param, 406000), setKey(&dm::FullMarsRecord::chem, 902)),
        rule(all(matchParams(215006)), setKey(&dm::FullMarsRecord::param, 406000), setKey(&dm::FullMarsRecord::chem, 903)),
        rule(all(matchParams(215007)), setKey(&dm::FullMarsRecord::param, 407000), setKey(&dm::FullMarsRecord::chem, 901)),
        rule(all(matchParams(215008)), setKey(&dm::FullMarsRecord::param, 407000), setKey(&dm::FullMarsRecord::chem, 902)),
        rule(all(matchParams(215009)), setKey(&dm::FullMarsRecord::param, 407000), setKey(&dm::FullMarsRecord::chem, 903)),
        rule(all(matchParams(215010)), setKey(&dm::FullMarsRecord::param, 410000), setKey(&dm::FullMarsRecord::chem, 901)),
        rule(all(matchParams(215011)), setKey(&dm::FullMarsRecord::param, 410000), setKey(&dm::FullMarsRecord::chem, 902)),
        rule(all(matchParams(215012)), setKey(&dm::FullMarsRecord::param, 410000), setKey(&dm::FullMarsRecord::chem, 903)),
        rule(all(matchParams(215013)), setKey(&dm::FullMarsRecord::param, 411000), setKey(&dm::FullMarsRecord::chem, 901)),
        rule(all(matchParams(215014)), setKey(&dm::FullMarsRecord::param, 411000), setKey(&dm::FullMarsRecord::chem, 902)),
        rule(all(matchParams(215015)), setKey(&dm::FullMarsRecord::param, 411000), setKey(&dm::FullMarsRecord::chem, 903)),
        rule(all(matchParams(215016)), setKey(&dm::FullMarsRecord::param, 451000), setKey(&dm::FullMarsRecord::chem, 901)),
        rule(all(matchParams(215017)), setKey(&dm::FullMarsRecord::param, 451000), setKey(&dm::FullMarsRecord::chem, 902)),
        rule(all(matchParams(215018)), setKey(&dm::FullMarsRecord::param, 451000), setKey(&dm::FullMarsRecord::chem, 903)),
        rule(all(matchParams(215019)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 901)),
        rule(all(matchParams(215020)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 902)),
        rule(all(matchParams(215021)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 903)),
        rule(all(matchParams(215022)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 901), setKey(&dm::FullMarsRecord::wavelength, 550)),
        rule(all(matchParams(215023)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 902), setKey(&dm::FullMarsRecord::wavelength, 550)),
        rule(all(matchParams(215024)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 903), setKey(&dm::FullMarsRecord::wavelength, 550)),
        rule(all(matchParams(215025)), setKey(&dm::FullMarsRecord::param, 453000), setKey(&dm::FullMarsRecord::chem, 904)),
        rule(all(matchParams(215026)), setKey(&dm::FullMarsRecord::param, 453000), setKey(&dm::FullMarsRecord::chem, 905)),
        rule(all(matchParams(215027)), setKey(&dm::FullMarsRecord::param, 453000), setKey(&dm::FullMarsRecord::chem, 906)),
        rule(all(matchParams(215028)), setKey(&dm::FullMarsRecord::param, 406000), setKey(&dm::FullMarsRecord::chem, 904)),
        rule(all(matchParams(215029)), setKey(&dm::FullMarsRecord::param, 406000), setKey(&dm::FullMarsRecord::chem, 905)),
        rule(all(matchParams(215030)), setKey(&dm::FullMarsRecord::param, 406000), setKey(&dm::FullMarsRecord::chem, 906)),
        rule(all(matchParams(215031)), setKey(&dm::FullMarsRecord::param, 407000), setKey(&dm::FullMarsRecord::chem, 904)),
        rule(all(matchParams(215032)), setKey(&dm::FullMarsRecord::param, 407000), setKey(&dm::FullMarsRecord::chem, 905)),
        rule(all(matchParams(215033)), setKey(&dm::FullMarsRecord::param, 407000), setKey(&dm::FullMarsRecord::chem, 906)),
        rule(all(matchParams(215034)), setKey(&dm::FullMarsRecord::param, 410000), setKey(&dm::FullMarsRecord::chem, 904)),
        rule(all(matchParams(215035)), setKey(&dm::FullMarsRecord::param, 410000), setKey(&dm::FullMarsRecord::chem, 905)),
        rule(all(matchParams(215036)), setKey(&dm::FullMarsRecord::param, 410000), setKey(&dm::FullMarsRecord::chem, 906)),
        rule(all(matchParams(215037)), setKey(&dm::FullMarsRecord::param, 411000), setKey(&dm::FullMarsRecord::chem, 904)),
        rule(all(matchParams(215038)), setKey(&dm::FullMarsRecord::param, 411000), setKey(&dm::FullMarsRecord::chem, 905)),
        rule(all(matchParams(215039)), setKey(&dm::FullMarsRecord::param, 411000), setKey(&dm::FullMarsRecord::chem, 906)),
        rule(all(matchParams(215040)), setKey(&dm::FullMarsRecord::param, 451000), setKey(&dm::FullMarsRecord::chem, 904)),
        rule(all(matchParams(215041)), setKey(&dm::FullMarsRecord::param, 451000), setKey(&dm::FullMarsRecord::chem, 905)),
        rule(all(matchParams(215042)), setKey(&dm::FullMarsRecord::param, 451000), setKey(&dm::FullMarsRecord::chem, 906)),
        rule(all(matchParams(215043)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 904)),
        rule(all(matchParams(215044)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 905)),
        rule(all(matchParams(215045)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 906)),
        rule(all(matchParams(215046)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 904), setKey(&dm::FullMarsRecord::wavelength, 550)),
        rule(all(matchParams(215047)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 905), setKey(&dm::FullMarsRecord::wavelength, 550)),
        rule(all(matchParams(215048)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 906), setKey(&dm::FullMarsRecord::wavelength, 550)),
        rule(all(matchParams(215049)), setKey(&dm::FullMarsRecord::param, 453000), setKey(&dm::FullMarsRecord::chem, 908)),
        rule(all(matchParams(215050)), setKey(&dm::FullMarsRecord::param, 453000), setKey(&dm::FullMarsRecord::chem, 907)),
        rule(all(matchParams(215051)), setKey(&dm::FullMarsRecord::param, 406000), setKey(&dm::FullMarsRecord::chem, 908)),
        rule(all(matchParams(215052)), setKey(&dm::FullMarsRecord::param, 406000), setKey(&dm::FullMarsRecord::chem, 907)),
        rule(all(matchParams(215053)), setKey(&dm::FullMarsRecord::param, 407000), setKey(&dm::FullMarsRecord::chem, 908)),
        rule(all(matchParams(215054)), setKey(&dm::FullMarsRecord::param, 407000), setKey(&dm::FullMarsRecord::chem, 907)),
        rule(all(matchParams(215055)), setKey(&dm::FullMarsRecord::param, 410000), setKey(&dm::FullMarsRecord::chem, 908)),
        rule(all(matchParams(215056)), setKey(&dm::FullMarsRecord::param, 410000), setKey(&dm::FullMarsRecord::chem, 907)),
        rule(all(matchParams(215057)), setKey(&dm::FullMarsRecord::param, 411000), setKey(&dm::FullMarsRecord::chem, 908)),
        rule(all(matchParams(215058)), setKey(&dm::FullMarsRecord::param, 411000), setKey(&dm::FullMarsRecord::chem, 907)),
        rule(all(matchParams(215059)), setKey(&dm::FullMarsRecord::param, 451000), setKey(&dm::FullMarsRecord::chem, 908)),
        rule(all(matchParams(215060)), setKey(&dm::FullMarsRecord::param, 451000), setKey(&dm::FullMarsRecord::chem, 907)),
        rule(all(matchParams(215061)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 908)),
        rule(all(matchParams(215062)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 907)),
        rule(all(matchParams(215063)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 908), setKey(&dm::FullMarsRecord::wavelength, 550)),
        rule(all(matchParams(215064)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 907), setKey(&dm::FullMarsRecord::wavelength, 550)),
        rule(all(matchParams(215065)), setKey(&dm::FullMarsRecord::param, 453000), setKey(&dm::FullMarsRecord::chem, 910)),
        rule(all(matchParams(215066)), setKey(&dm::FullMarsRecord::param, 453000), setKey(&dm::FullMarsRecord::chem, 909)),
        rule(all(matchParams(215067)), setKey(&dm::FullMarsRecord::param, 406000), setKey(&dm::FullMarsRecord::chem, 910)),
        rule(all(matchParams(215068)), setKey(&dm::FullMarsRecord::param, 406000), setKey(&dm::FullMarsRecord::chem, 909)),
        rule(all(matchParams(215069)), setKey(&dm::FullMarsRecord::param, 407000), setKey(&dm::FullMarsRecord::chem, 910)),
        rule(all(matchParams(215070)), setKey(&dm::FullMarsRecord::param, 407000), setKey(&dm::FullMarsRecord::chem, 909)),
        rule(all(matchParams(215071)), setKey(&dm::FullMarsRecord::param, 410000), setKey(&dm::FullMarsRecord::chem, 910)),
        rule(all(matchParams(215072)), setKey(&dm::FullMarsRecord::param, 410000), setKey(&dm::FullMarsRecord::chem, 909)),
        rule(all(matchParams(215073)), setKey(&dm::FullMarsRecord::param, 411000), setKey(&dm::FullMarsRecord::chem, 910)),
        rule(all(matchParams(215074)), setKey(&dm::FullMarsRecord::param, 411000), setKey(&dm::FullMarsRecord::chem, 909)),
        rule(all(matchParams(215075)), setKey(&dm::FullMarsRecord::param, 451000), setKey(&dm::FullMarsRecord::chem, 910)),
        rule(all(matchParams(215076)), setKey(&dm::FullMarsRecord::param, 451000), setKey(&dm::FullMarsRecord::chem, 909)),
        rule(all(matchParams(215077)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 910)),
        rule(all(matchParams(215078)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 909)),
        rule(all(matchParams(215079)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 910), setKey(&dm::FullMarsRecord::wavelength, 550)),
        rule(all(matchParams(215080)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 909), setKey(&dm::FullMarsRecord::wavelength, 550)),
        rule(all(matchParams(215081)), setKey(&dm::FullMarsRecord::param, 453000), setKey(&dm::FullMarsRecord::chem, 911)),
        rule(all(matchParams(215082)), setKey(&dm::FullMarsRecord::param, 406000), setKey(&dm::FullMarsRecord::chem, 911)),
        rule(all(matchParams(215083)), setKey(&dm::FullMarsRecord::param, 407000), setKey(&dm::FullMarsRecord::chem, 911)),
        rule(all(matchParams(215084)), setKey(&dm::FullMarsRecord::param, 410000), setKey(&dm::FullMarsRecord::chem, 911)),
        rule(all(matchParams(215085)), setKey(&dm::FullMarsRecord::param, 411000), setKey(&dm::FullMarsRecord::chem, 911)),
        rule(all(matchParams(215086)), setKey(&dm::FullMarsRecord::param, 451000), setKey(&dm::FullMarsRecord::chem, 911)),
        rule(all(matchParams(215087)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 911)),
        rule(all(matchParams(215088)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 911), setKey(&dm::FullMarsRecord::wavelength, 550)),
        rule(all(matchParams(215093)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 532)),
        rule(all(matchParams(215094)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 904), setKey(&dm::FullMarsRecord::wavelength, 532)),
        rule(all(matchParams(215095)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 910), setKey(&dm::FullMarsRecord::wavelength, 532)),
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
        rule(all(matchParams(215176)), setKey(&dm::FullMarsRecord::param, 472000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 2130)),
        rule(all(matchParams(215177)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 923), setKey(&dm::FullMarsRecord::wavelength, 2130)),
        rule(all(matchParams(215178)), setKey(&dm::FullMarsRecord::param, 458000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 2130)),
        rule(all(matchParams(215180)), setKey(&dm::FullMarsRecord::param, 462000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 355)),
        rule(all(matchParams(215181)), setKey(&dm::FullMarsRecord::param, 462000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 532)),
        rule(all(matchParams(215182)), setKey(&dm::FullMarsRecord::param, 462000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 1064)),
        rule(all(matchParams(215183)), setKey(&dm::FullMarsRecord::param, 460000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 355)),
        rule(all(matchParams(215184)), setKey(&dm::FullMarsRecord::param, 460000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 532)),
        rule(all(matchParams(215185)), setKey(&dm::FullMarsRecord::param, 460000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 1064)),
        rule(all(matchParams(215186)), setKey(&dm::FullMarsRecord::param, 461000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 355)),
        rule(all(matchParams(215187)), setKey(&dm::FullMarsRecord::param, 461000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 532)),
        rule(all(matchParams(215188)), setKey(&dm::FullMarsRecord::param, 461000), setKey(&dm::FullMarsRecord::chem, 922), setKey(&dm::FullMarsRecord::wavelength, 1064)),
        rule(all(matchParams(215189)), setKey(&dm::FullMarsRecord::param, 453000), setKey(&dm::FullMarsRecord::chem, 912)),
        rule(all(matchParams(215190)), setKey(&dm::FullMarsRecord::param, 453000), setKey(&dm::FullMarsRecord::chem, 913)),
        rule(all(matchParams(215191)), setKey(&dm::FullMarsRecord::param, 406000), setKey(&dm::FullMarsRecord::chem, 912)),
        rule(all(matchParams(215192)), setKey(&dm::FullMarsRecord::param, 406000), setKey(&dm::FullMarsRecord::chem, 913)),
        rule(all(matchParams(215193)), setKey(&dm::FullMarsRecord::param, 407000), setKey(&dm::FullMarsRecord::chem, 912)),
        rule(all(matchParams(215194)), setKey(&dm::FullMarsRecord::param, 407000), setKey(&dm::FullMarsRecord::chem, 913)),
        rule(all(matchParams(215195)), setKey(&dm::FullMarsRecord::param, 410000), setKey(&dm::FullMarsRecord::chem, 912)),
        rule(all(matchParams(215196)), setKey(&dm::FullMarsRecord::param, 410000), setKey(&dm::FullMarsRecord::chem, 913)),
        rule(all(matchParams(215197)), setKey(&dm::FullMarsRecord::param, 411000), setKey(&dm::FullMarsRecord::chem, 912)),
        rule(all(matchParams(215198)), setKey(&dm::FullMarsRecord::param, 411000), setKey(&dm::FullMarsRecord::chem, 913)),
        rule(all(matchParams(215199)), setKey(&dm::FullMarsRecord::param, 451000), setKey(&dm::FullMarsRecord::chem, 912)),
        rule(all(matchParams(215200)), setKey(&dm::FullMarsRecord::param, 451000), setKey(&dm::FullMarsRecord::chem, 913)),
        rule(all(matchParams(215201)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 912)),
        rule(all(matchParams(215202)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 913)),
        rule(all(matchParams(215203)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 912), setKey(&dm::FullMarsRecord::wavelength, 550)),
        rule(all(matchParams(215204)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 913), setKey(&dm::FullMarsRecord::wavelength, 550)),
        rule(all(matchParams(215205)), setKey(&dm::FullMarsRecord::param, 453000), setKey(&dm::FullMarsRecord::chem, 914)),
        rule(all(matchParams(215206)), setKey(&dm::FullMarsRecord::param, 406000), setKey(&dm::FullMarsRecord::chem, 914)),
        rule(all(matchParams(215207)), setKey(&dm::FullMarsRecord::param, 407000), setKey(&dm::FullMarsRecord::chem, 914)),
        rule(all(matchParams(215208)), setKey(&dm::FullMarsRecord::param, 410000), setKey(&dm::FullMarsRecord::chem, 914)),
        rule(all(matchParams(215209)), setKey(&dm::FullMarsRecord::param, 411000), setKey(&dm::FullMarsRecord::chem, 914)),
        rule(all(matchParams(215210)), setKey(&dm::FullMarsRecord::param, 451000), setKey(&dm::FullMarsRecord::chem, 914)),
        rule(all(matchParams(215211)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 914)),
        rule(all(matchParams(215212)), setKey(&dm::FullMarsRecord::param, 453000), setKey(&dm::FullMarsRecord::chem, 915)),
        rule(all(matchParams(215213)), setKey(&dm::FullMarsRecord::param, 406000), setKey(&dm::FullMarsRecord::chem, 915)),
        rule(all(matchParams(215214)), setKey(&dm::FullMarsRecord::param, 407000), setKey(&dm::FullMarsRecord::chem, 915)),
        rule(all(matchParams(215215)), setKey(&dm::FullMarsRecord::param, 410000), setKey(&dm::FullMarsRecord::chem, 915)),
        rule(all(matchParams(215216)), setKey(&dm::FullMarsRecord::param, 411000), setKey(&dm::FullMarsRecord::chem, 915)),
        rule(all(matchParams(215217)), setKey(&dm::FullMarsRecord::param, 451000), setKey(&dm::FullMarsRecord::chem, 915)),
        rule(all(matchParams(215218)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 915)),
        rule(all(matchParams(215219)), setKey(&dm::FullMarsRecord::param, 453000), setKey(&dm::FullMarsRecord::chem, 916)),
        rule(all(matchParams(215220)), setKey(&dm::FullMarsRecord::param, 406000), setKey(&dm::FullMarsRecord::chem, 916)),
        rule(all(matchParams(215221)), setKey(&dm::FullMarsRecord::param, 407000), setKey(&dm::FullMarsRecord::chem, 916)),
        rule(all(matchParams(215222)), setKey(&dm::FullMarsRecord::param, 410000), setKey(&dm::FullMarsRecord::chem, 916)),
        rule(all(matchParams(215223)), setKey(&dm::FullMarsRecord::param, 411000), setKey(&dm::FullMarsRecord::chem, 916)),
        rule(all(matchParams(215224)), setKey(&dm::FullMarsRecord::param, 451000), setKey(&dm::FullMarsRecord::chem, 916)),
        rule(all(matchParams(215225)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 916)),
        rule(all(matchParams(215226)), setKey(&dm::FullMarsRecord::param, 457000), setKey(&dm::FullMarsRecord::chem, 918), setKey(&dm::FullMarsRecord::wavelength, 550)),
        rule(all(matchParams(217003)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 3)),
        rule(all(matchParams(217004)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 4)),
        rule(all(matchParams(217006)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 6)),
        rule(all(matchParams(217007)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 7)),
        rule(all(matchParams(217009)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 9)),
        rule(all(matchParams(217010)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 10)),
        rule(all(matchParams(217011)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 311)),
        rule(all(matchParams(217012)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 12)),
        rule(all(matchParams(217013)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 13)),
        rule(all(matchParams(217014)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 14)),
        rule(all(matchParams(217015)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 15)),
        rule(all(matchParams(217016)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 16)),
        rule(all(matchParams(217018)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 18)),
        rule(all(matchParams(217019)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 19)),
        rule(all(matchParams(217020)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 20)),
        rule(all(matchParams(217021)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 21)),
        rule(all(matchParams(217022)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 22)),
        rule(all(matchParams(217023)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 23)),
        rule(all(matchParams(217024)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 24)),
        rule(all(matchParams(217026)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 26)),
        rule(all(matchParams(217027)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 27)),
        rule(all(matchParams(217028)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 28)),
        rule(all(matchParams(217029)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 29)),
        rule(all(matchParams(217030)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 30)),
        rule(all(matchParams(217032)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 32)),
        rule(all(matchParams(217033)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 33)),
        rule(all(matchParams(217034)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 34)),
        rule(all(matchParams(217035)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 35)),
        rule(all(matchParams(217036)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 36)),
        rule(all(matchParams(217037)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 37)),
        rule(all(matchParams(217038)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 38)),
        rule(all(matchParams(217039)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 39)),
        rule(all(matchParams(217040)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 40)),
        rule(all(matchParams(217041)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 41)),
        rule(all(matchParams(217042)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 42)),
        rule(all(matchParams(217043)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 43)),
        rule(all(matchParams(217044)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 44)),
        rule(all(matchParams(217045)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 45)),
        rule(all(matchParams(217046)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 46)),
        rule(all(matchParams(217047)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 47)),
        rule(all(matchParams(217048)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 48)),
        rule(all(matchParams(217049)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 49)),
        rule(all(matchParams(217050)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 50)),
        rule(all(matchParams(217052)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 52)),
        rule(all(matchParams(217053)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 53)),
        rule(all(matchParams(217054)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 54)),
        rule(all(matchParams(217055)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 55)),
        rule(all(matchParams(217056)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 56)),
        rule(all(matchParams(217057)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 57)),
        rule(all(matchParams(217058)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 58)),
        rule(all(matchParams(217059)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 59)),
        rule(all(matchParams(217063)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 63)),
        rule(all(matchParams(217064)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 64)),
        rule(all(matchParams(217065)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 65)),
        rule(all(matchParams(217066)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 66)),
        rule(all(matchParams(217067)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 67)),
        rule(all(matchParams(217068)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 68)),
        rule(all(matchParams(217069)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 69)),
        rule(all(matchParams(217070)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 70)),
        rule(all(matchParams(217071)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 71)),
        rule(all(matchParams(217072)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 72)),
        rule(all(matchParams(217073)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 73)),
        rule(all(matchParams(217074)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 74)),
        rule(all(matchParams(217075)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 75)),
        rule(all(matchParams(217076)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 76)),
        rule(all(matchParams(217077)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 77)),
        rule(all(matchParams(217078)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 78)),
        rule(all(matchParams(217079)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 79)),
        rule(all(matchParams(217080)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 80)),
        rule(all(matchParams(217082)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 82)),
        rule(all(matchParams(217083)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 83)),
        rule(all(matchParams(217085)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 85)),
        rule(all(matchParams(217086)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 86)),
        rule(all(matchParams(217099)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 99)),
        rule(all(matchParams(217100)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 100)),
        rule(all(matchParams(217101)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 101)),
        rule(all(matchParams(217107)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 107)),
        rule(all(matchParams(217118)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 118)),
        rule(all(matchParams(217159)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 159)),
        rule(all(matchParams(217161)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 161)),
        rule(all(matchParams(217169)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 169)),
        rule(all(matchParams(217173)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 173)),
        rule(all(matchParams(217174)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 174)),
        rule(all(matchParams(217175)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 175)),
        rule(all(matchParams(217176)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 176)),
        rule(all(matchParams(217177)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 177)),
        rule(all(matchParams(217178)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 178)),
        rule(all(matchParams(217186)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 186)),
        rule(all(matchParams(217187)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 187)),
        rule(all(matchParams(217188)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 188)),
        rule(all(matchParams(217189)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 189)),
        rule(all(matchParams(217190)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 190)),
        rule(all(matchParams(217191)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 191)),
        rule(all(matchParams(217192)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 192)),
        rule(all(matchParams(217193)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 193)),
        rule(all(matchParams(217194)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 194)),
        rule(all(matchParams(217195)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 195)),
        rule(all(matchParams(217196)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 196)),
        rule(all(matchParams(217197)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 197)),
        rule(all(matchParams(217198)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 198)),
        rule(all(matchParams(217199)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 199)),
        rule(all(matchParams(217200)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 200)),
        rule(all(matchParams(217201)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 201)),
        rule(all(matchParams(217202)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 202)),
        rule(all(matchParams(217203)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 203)),
        rule(all(matchParams(217204)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 204)),
        rule(all(matchParams(217206)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 206)),
        rule(all(matchParams(217222)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 222)),
        rule(all(matchParams(217224)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 224)),
        rule(all(matchParams(217225)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 225)),
        rule(all(matchParams(217226)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 226)),
        rule(all(matchParams(217227)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 227)),
        rule(all(matchParams(217228)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 228)),
        rule(all(matchParams(217229)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 229)),
        rule(all(matchParams(217230)), setKey(&dm::FullMarsRecord::param, 402000), setKey(&dm::FullMarsRecord::chem, 230)),
        rule(all(matchParams(218003)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 3)),
        rule(all(matchParams(218004)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 404)),
        rule(all(matchParams(218006)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 6)),
        rule(all(matchParams(218007)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 7)),
        rule(all(matchParams(218009)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 9)),
        rule(all(matchParams(218010)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 10)),
        rule(all(matchParams(218011)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 311)),
        rule(all(matchParams(218012)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 12)),
        rule(all(matchParams(218013)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 13)),
        rule(all(matchParams(218014)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 14)),
        rule(all(matchParams(218015)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 15)),
        rule(all(matchParams(218016)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 16)),
        rule(all(matchParams(218018)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 18)),
        rule(all(matchParams(218019)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 19)),
        rule(all(matchParams(218020)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 20)),
        rule(all(matchParams(218021)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 21)),
        rule(all(matchParams(218022)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 22)),
        rule(all(matchParams(218023)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 23)),
        rule(all(matchParams(218024)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 24)),
        rule(all(matchParams(218026)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 26)),
        rule(all(matchParams(218027)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 27)),
        rule(all(matchParams(218028)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 28)),
        rule(all(matchParams(218029)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 29)),
        rule(all(matchParams(218030)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 30)),
        rule(all(matchParams(218032)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 32)),
        rule(all(matchParams(218033)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 33)),
        rule(all(matchParams(218034)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 34)),
        rule(all(matchParams(218035)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 35)),
        rule(all(matchParams(218036)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 36)),
        rule(all(matchParams(218037)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 37)),
        rule(all(matchParams(218038)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 38)),
        rule(all(matchParams(218039)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 39)),
        rule(all(matchParams(218040)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 40)),
        rule(all(matchParams(218041)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 41)),
        rule(all(matchParams(218042)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 42)),
        rule(all(matchParams(218043)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 43)),
        rule(all(matchParams(218044)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 44)),
        rule(all(matchParams(218045)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 45)),
        rule(all(matchParams(218046)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 46)),
        rule(all(matchParams(218047)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 47)),
        rule(all(matchParams(218048)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 48)),
        rule(all(matchParams(218049)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 49)),
        rule(all(matchParams(218050)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 50)),
        rule(all(matchParams(218051)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 900)),
        rule(all(matchParams(218052)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 52)),
        rule(all(matchParams(218053)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 53)),
        rule(all(matchParams(218054)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 112)),
        rule(all(matchParams(218055)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 55)),
        rule(all(matchParams(218056)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 56)),
        rule(all(matchParams(218057)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 57)),
        rule(all(matchParams(218058)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 58)),
        rule(all(matchParams(218059)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 359)),
        rule(all(matchParams(218063)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 63)),
        rule(all(matchParams(218064)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 64)),
        rule(all(matchParams(218065)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 65)),
        rule(all(matchParams(218066)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 66)),
        rule(all(matchParams(218067)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 67)),
        rule(all(matchParams(218068)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 68)),
        rule(all(matchParams(218069)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 69)),
        rule(all(matchParams(218070)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 70)),
        rule(all(matchParams(218071)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 71)),
        rule(all(matchParams(218072)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 72)),
        rule(all(matchParams(218073)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 73)),
        rule(all(matchParams(218074)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 74)),
        rule(all(matchParams(218075)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 75)),
        rule(all(matchParams(218076)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 76)),
        rule(all(matchParams(218077)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 77)),
        rule(all(matchParams(218078)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 78)),
        rule(all(matchParams(218079)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 79)),
        rule(all(matchParams(218080)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 80)),
        rule(all(matchParams(218082)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 82)),
        rule(all(matchParams(218083)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 83)),
        rule(all(matchParams(218085)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 85)),
        rule(all(matchParams(218086)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 86)),
        rule(all(matchParams(218099)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 99)),
        rule(all(matchParams(218100)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 100)),
        rule(all(matchParams(218101)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 101)),
        rule(all(matchParams(218107)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 107)),
        rule(all(matchParams(218117)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 14)),
        rule(all(matchParams(218159)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 159)),
        rule(all(matchParams(218161)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 161)),
        rule(all(matchParams(218169)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 169)),
        rule(all(matchParams(218173)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 173)),
        rule(all(matchParams(218174)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 174)),
        rule(all(matchParams(218175)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 175)),
        rule(all(matchParams(218176)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 176)),
        rule(all(matchParams(218177)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 177)),
        rule(all(matchParams(218178)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 178)),
        rule(all(matchParams(218186)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 186)),
        rule(all(matchParams(218187)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 187)),
        rule(all(matchParams(218188)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 188)),
        rule(all(matchParams(218189)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 189)),
        rule(all(matchParams(218190)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 190)),
        rule(all(matchParams(218191)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 191)),
        rule(all(matchParams(218192)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 192)),
        rule(all(matchParams(218193)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 193)),
        rule(all(matchParams(218194)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 194)),
        rule(all(matchParams(218195)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 195)),
        rule(all(matchParams(218196)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 196)),
        rule(all(matchParams(218197)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 197)),
        rule(all(matchParams(218198)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 198)),
        rule(all(matchParams(218199)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 199)),
        rule(all(matchParams(218200)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 200)),
        rule(all(matchParams(218201)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 201)),
        rule(all(matchParams(218202)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 202)),
        rule(all(matchParams(218203)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 203)),
        rule(all(matchParams(218204)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 204)),
        rule(all(matchParams(218221)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 8)),
        rule(all(matchParams(218222)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 222)),
        rule(all(matchParams(218224)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 224)),
        rule(all(matchParams(218225)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 225)),
        rule(all(matchParams(218226)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 226)),
        rule(all(matchParams(218227)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 227)),
        rule(all(matchParams(218228)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 228)),
        rule(all(matchParams(218229)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 229)),
        rule(all(matchParams(218230)), setKey(&dm::FullMarsRecord::param, 401000), setKey(&dm::FullMarsRecord::chem, 230)),
        rule(all(matchParams(219207)), setKey(&dm::FullMarsRecord::param, 469000), setKey(&dm::FullMarsRecord::chem, 9)),
        rule(all(matchParams(219208)), setKey(&dm::FullMarsRecord::param, 469000), setKey(&dm::FullMarsRecord::chem, 311)),
        rule(all(matchParams(219209)), setKey(&dm::FullMarsRecord::param, 469000), setKey(&dm::FullMarsRecord::chem, 12)),
        rule(all(matchParams(219212)), setKey(&dm::FullMarsRecord::param, 469000), setKey(&dm::FullMarsRecord::chem, 99)),
        rule(all(matchParams(219213)), setKey(&dm::FullMarsRecord::param, 469000), setKey(&dm::FullMarsRecord::chem, 100)),
        rule(all(matchParams(219219)), setKey(&dm::FullMarsRecord::param, 469000), setKey(&dm::FullMarsRecord::chem, 226)),
        rule(all(matchParams(219220)), setKey(&dm::FullMarsRecord::param, 469000), setKey(&dm::FullMarsRecord::chem, 224)),
        rule(all(matchParams(222001)), setKey(&dm::FullMarsRecord::param, 445000), setKey(&dm::FullMarsRecord::chem, 236)),
        rule(all(matchParams(222006)), setKey(&dm::FullMarsRecord::param, 445000), setKey(&dm::FullMarsRecord::chem, 6)),
        rule(all(matchParams(222013)), setKey(&dm::FullMarsRecord::param, 445000), setKey(&dm::FullMarsRecord::chem, 13)),
        rule(all(matchParams(222015)), setKey(&dm::FullMarsRecord::param, 445000), setKey(&dm::FullMarsRecord::chem, 15)),
        rule(all(matchParams(222017)), setKey(&dm::FullMarsRecord::param, 445000), setKey(&dm::FullMarsRecord::chem, 8)),
        rule(all(matchParams(222019)), setKey(&dm::FullMarsRecord::param, 445000), setKey(&dm::FullMarsRecord::chem, 19)),
        rule(all(matchParams(222027)), setKey(&dm::FullMarsRecord::param, 445000), setKey(&dm::FullMarsRecord::chem, 27)),
        rule(all(matchParams(222031)), setKey(&dm::FullMarsRecord::param, 445000), setKey(&dm::FullMarsRecord::chem, 17)),
        rule(all(matchParams(222033)), setKey(&dm::FullMarsRecord::param, 445000), setKey(&dm::FullMarsRecord::chem, 33)),
        rule(all(matchParams(223006)), setKey(&dm::FullMarsRecord::param, 444000), setKey(&dm::FullMarsRecord::chem, 6)),
        rule(all(matchParams(223013)), setKey(&dm::FullMarsRecord::param, 444000), setKey(&dm::FullMarsRecord::chem, 13)),
        rule(all(matchParams(223015)), setKey(&dm::FullMarsRecord::param, 444000), setKey(&dm::FullMarsRecord::chem, 15)),
        rule(all(matchParams(223017)), setKey(&dm::FullMarsRecord::param, 444000), setKey(&dm::FullMarsRecord::chem, 8)),
        rule(all(matchParams(223019)), setKey(&dm::FullMarsRecord::param, 444000), setKey(&dm::FullMarsRecord::chem, 19)),
        rule(all(matchParams(223026)), setKey(&dm::FullMarsRecord::param, 444000), setKey(&dm::FullMarsRecord::chem, 26)),
        rule(all(matchParams(223027)), setKey(&dm::FullMarsRecord::param, 444000), setKey(&dm::FullMarsRecord::chem, 27)),
        rule(all(matchParams(223031)), setKey(&dm::FullMarsRecord::param, 444000), setKey(&dm::FullMarsRecord::chem, 17)),
        rule(all(matchParams(223033)), setKey(&dm::FullMarsRecord::param, 444000), setKey(&dm::FullMarsRecord::chem, 33))
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
    static auto wmo_ = ruleList(  //
                                  // Cloud cover (fraction -> %)
        fixCloudParam164(),       //  164 -> 228164, x100
        fixCloudParam186(),       //  186 -> 3073,   x100
        fixCloudParam187(),       //  187 -> 3074,   x100
        fixCloudParam188(),       //  188 -> 3075,   x100
        // Precipitation and water cycle (m -> mm)
        fixConvectivePrecip143(),      //  143 -> 228143, x1000
        fixTotalPrecip228(),           //  228 -> 228228, x1000
        fixRunOffWaterParam205(),      //  205 -> 231002, x1000
        fixSnowfallWaterEquivParam(),  //  144 -> 228144, x1000
        fixSurfaceRunoff8(),           //    8 -> 231010, x1000
        fixSubSurfaceRunoff9(),        //    9 -> 231012, x1000
        fixSnowDepth141(),             //  141 -> 228141, x1000
        fixLargeScalePrecip142(),      //  142 -> 3062,   x1000
        fixEvaporation182(),           //  182 -> 260259, x1000
        fixSkinReservoirContent198(),  //  198 -> 160198, x1000
        fixConvectiveSnowfall239(),    //  239 -> 231057, x1000
        fixLargeScaleSnowfall240(),    //  240 -> 231058, x1000
        fixFreezingRain228216(),       //  228216 -> 231001, x1000
        // Albedo (fraction -> %), required for ERA
        fixAlbedoUvDirect15(),          //   15 -> 210199, x100
        fixAlbedoUvDiffuse16(),         //   16 -> 210198, x100
        fixAlbedoNirDirect17(),         //   17 -> 210261, x100
        fixAlbedoNirDiffuse18(),        //   18 -> 210260, x100
        fixSnowAlbedo32(),              //   32 -> 228032, x100
        fixAlbedoClimatological174(),   //  174 -> 260509, x100
        fixForecastAlbedo243(),         //  243 -> 260509, x100
        fixAlbedoUvDirectIso210186(),   //  210186 -> 210201, x100
        fixAlbedoUvDirectVol210187(),   //  210187 -> 210202, x100
        fixAlbedoUvDirectGeo210188(),   //  210188 -> 210200, x100
        fixAlbedoNirDirectIso210189(),  //  210189 -> 210263, x100
        fixAlbedoNirDirectVol210190(),  //  210190 -> 210264, x100
        fixAlbedoNirDirectGeo210191(),  //  210191 -> 210262, x100
        // ERA other water-cycle parameters
        fixSnowEvaporation44(),           //   44 -> 231003, x1000
        fixSnowmelt45(),                  //   45 -> 3099,   x1000
        fixTotalColumnOzone206(),         //  206 -> 260132, x46698.05
        fixPotentialEvaporation228251(),  //  228251 -> 231005, x1000
        // Time-mean rate parameters (m/s -> mm/s)
        fixTimeMeanSurfaceRunoffRate172008(),     //  172008 -> 235020, x1000
        fixTimeMeanSubSurfaceRunoffRate172009(),  //  172009 -> 235021, x1000
        fixTimeMeanSnowEvapRate172044(),          //  172044 -> 235023, x1000
        fixTimeMeanSnowmeltRate172045(),          //  172045 -> 235024, x1000
        fixTimeMeanLargeScalePrecipRate172142(),  //  172142 -> 235029, x1000
        fixTimeMeanConvPrecipRate172143(),        //  172143 -> 235030, x1000
        fixTimeMeanTotalSnowfallRate172144(),     //  172144 -> 235031, x1000
        fixTimeMeanEvaporationRate172182(),       //  172182 -> 235043, x1000
        fixTimeMeanRunoffRate172205(),            //  172205 -> 235048, x1000
        fixTimeMeanTotalPrecipRate172228(),       //  172228 -> 235055, x1000
        fixTimeMeanSnowDepth235141(),             //  235141 -> 235078, x1000
        // Time-mean cloud cover and albedo (fraction -> %), new in ecCodes 2.38
        fixTimeMeanLowCloudCover235186(),   //  235186 -> 235108, x100
        fixTimeMeanMedCloudCover235187(),   //  235187 -> 235109, x100
        fixTimeMeanHighCloudCover235188(),  //  235188 -> 235110, x100
        fixTimeMeanForecastAlbedo235243()   //  235243 -> 235263, x100
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

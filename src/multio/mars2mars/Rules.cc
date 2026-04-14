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
    return rule(all(matchParams(35), OneOf{&dm::MarsRecord::levtype, {dm::LevType::SFC}}),                 //
                setKey(&dm::MarsRecord::levtype, dm::LevType::SOL), setKey(&dm::MarsRecord::levelist, 1),  //
                setKey(&dm::MarsRecord::param, 262024));                                                   //
}
auto fixParam36ToSol2() {
    return rule(all(matchParams(36), OneOf{&dm::MarsRecord::levtype, {dm::LevType::SFC}}),                 //
                setKey(&dm::MarsRecord::levtype, dm::LevType::SOL), setKey(&dm::MarsRecord::levelist, 2),  //
                setKey(&dm::MarsRecord::param, 262024));                                                   //
}
auto fixParam37ToSol3() {
    return rule(all(matchParams(37), OneOf{&dm::MarsRecord::levtype, {dm::LevType::SFC}}),                 //
                setKey(&dm::MarsRecord::levtype, dm::LevType::SOL), setKey(&dm::MarsRecord::levelist, 3),  //
                setKey(&dm::MarsRecord::param, 262024));                                                   //
}
auto fixParam38ToSol4() {
    return rule(all(matchParams(38), OneOf{&dm::MarsRecord::levtype, {dm::LevType::SFC}}),                 //
                setKey(&dm::MarsRecord::levtype, dm::LevType::SOL), setKey(&dm::MarsRecord::levelist, 4),  //
                setKey(&dm::MarsRecord::param, 262024));                                                   //
}

// To param 260199

auto fixParam39ToSol1() {
    return rule(all(matchParams(39), OneOf{&dm::MarsRecord::levtype, {dm::LevType::SFC}}),                 //
                setKey(&dm::MarsRecord::levtype, dm::LevType::SOL), setKey(&dm::MarsRecord::levelist, 1),  //
                setKey(&dm::MarsRecord::param, 260199));                                                   //
}
auto fixParam40ToSol2() {
    return rule(all(matchParams(40), OneOf{&dm::MarsRecord::levtype, {dm::LevType::SFC}}),                 //
                setKey(&dm::MarsRecord::levtype, dm::LevType::SOL), setKey(&dm::MarsRecord::levelist, 2),  //
                setKey(&dm::MarsRecord::param, 260199));                                                   //
}
auto fixParam41ToSol3() {
    return rule(all(matchParams(41), OneOf{&dm::MarsRecord::levtype, {dm::LevType::SFC}}),                 //
                setKey(&dm::MarsRecord::levtype, dm::LevType::SOL), setKey(&dm::MarsRecord::levelist, 3),  //
                setKey(&dm::MarsRecord::param, 260199));                                                   //
}
auto fixParam42ToSol4() {
    return rule(all(matchParams(42), OneOf{&dm::MarsRecord::levtype, {dm::LevType::SFC}}),                 //
                setKey(&dm::MarsRecord::levtype, dm::LevType::SOL), setKey(&dm::MarsRecord::levelist, 4),  //
                setKey(&dm::MarsRecord::param, 260199));                                                   //
}


// To param 260360

auto fixParam139ToSol1() {
    return rule(all(matchParams(139), OneOf{&dm::MarsRecord::levtype, {dm::LevType::SFC}}),                //
                setKey(&dm::MarsRecord::levtype, dm::LevType::SOL), setKey(&dm::MarsRecord::levelist, 1),  //
                setKey(&dm::MarsRecord::param, 260360));                                                   //
}
auto fixParam170ToSol2() {
    return rule(all(matchParams(170), OneOf{&dm::MarsRecord::levtype, {dm::LevType::SFC}}),                //
                setKey(&dm::MarsRecord::levtype, dm::LevType::SOL), setKey(&dm::MarsRecord::levelist, 2),  //
                setKey(&dm::MarsRecord::param, 260360));                                                   //
}
auto fixParam183ToSol3() {
    return rule(all(matchParams(183), OneOf{&dm::MarsRecord::levtype, {dm::LevType::SFC}}),                //
                setKey(&dm::MarsRecord::levtype, dm::LevType::SOL), setKey(&dm::MarsRecord::levelist, 3),  //
                setKey(&dm::MarsRecord::param, 260360));                                                   //
}
auto fixParam236ToSol4() {
    return rule(all(matchParams(236), OneOf{&dm::MarsRecord::levtype, {dm::LevType::SFC}}),                //
                setKey(&dm::MarsRecord::levtype, dm::LevType::SOL), setKey(&dm::MarsRecord::levelist, 4),  //
                setKey(&dm::MarsRecord::param, 260360));                                                   //
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
    return rule(all(matchParams(164)),                                           //
                setKey(&dm::MarsRecord::param, 228164), SetScaleFactor{100.0});  //
}
auto fixCloudParam186() {
    return rule(all(matchParams(186)),                                         //
                setKey(&dm::MarsRecord::param, 3073), SetScaleFactor{100.0});  //
}
auto fixCloudParam187() {
    return rule(all(matchParams(187)),                                         //
                setKey(&dm::MarsRecord::param, 3074), SetScaleFactor{100.0});  //
}
auto fixCloudParam188() {
    return rule(all(matchParams(188)),                                         //
                setKey(&dm::MarsRecord::param, 3075), SetScaleFactor{100.0});  //
}


//-----------------------------------------------------------------------------
// Precipitation
//-----------------------------------------------------------------------------

auto fixConvectivePrecip143() {
    return rule(all(matchParams(143)),                                            //
                setKey(&dm::MarsRecord::param, 228143), SetScaleFactor{1000.0});  //
}

auto fixTotalPrecip228() {
    return rule(all(matchParams(228)),                                            //
                setKey(&dm::MarsRecord::param, 228228), SetScaleFactor{1000.0});  //
}


//-----------------------------------------------------------------------------
// Height above
//-----------------------------------------------------------------------------

auto fixHeightAboveGround2m() {
    return rule(all(OneOf{&dm::MarsRecord::levtype, {dm::LevType::SFC}}, OneOf{&dm::MarsRecord::levelist, {0}},
                    matchParams(167, 168, 174096, 228037)),  //
                setKey(&dm::MarsRecord::levelist, 2));       //
}

auto fixHeightAboveGround10m() {
    return rule(all(OneOf{&dm::MarsRecord::levtype, {dm::LevType::SFC}}, OneOf{&dm::MarsRecord::levelist, {0}},
                    matchParams(228029, 228131, 228132, 165, 166, 207)),  //
                setKey(&dm::MarsRecord::levelist, 10));                   //
}

auto fixHeightAboveSea() {
    return rule(all(matchParams(140233, 140245, 140249)),  //
                setKey(&dm::MarsRecord::levelist, 10));    //
}


//-----------------------------------------------------------------------------
// Misc level fixes
//-----------------------------------------------------------------------------

auto fixMapToSol() {
    return rule(all(OneOf{&dm::MarsRecord::levtype, {dm::LevType::SFC}}, Has{&dm::MarsRecord::levelist},
                    NoneOf{&dm::MarsRecord::levelist, {0}},
                    matchParams(33, 238, 228038, 235080, 237080, 238080, 239080, 260360, 262000, 262024, 260199)),  //
                setKey(&dm::MarsRecord::levtype, dm::LevType::SOL));                                                //
}


auto fixRunOffWaterParam205() {
    return rule(all(matchParams(205)),                                            //
                setKey(&dm::MarsRecord::param, 231002), SetScaleFactor{1000.0});  //
}

auto fixSnowfallWaterEquivParam() {
    return rule(all(matchParams(144)),                                            //
                setKey(&dm::MarsRecord::param, 228144), SetScaleFactor{1000.0});  //
}


//-----------------------------------------------------------------------------
// Runoff and water cycle (m -> mm)
//-----------------------------------------------------------------------------

// Surface runoff: sro -> surfro
auto fixSurfaceRunoff8() {
    return rule(all(matchParams(8)),                                              //
                setKey(&dm::MarsRecord::param, 231010), SetScaleFactor{1000.0});  //
}

// Sub-surface runoff: ssro -> ssurfro
auto fixSubSurfaceRunoff9() {
    return rule(all(matchParams(9)),                                              //
                setKey(&dm::MarsRecord::param, 231012), SetScaleFactor{1000.0});  //
}

// Snow depth: sd -> sd (water equivalent)
auto fixSnowDepth141() {
    return rule(all(matchParams(141)),                                            //
                setKey(&dm::MarsRecord::param, 228141), SetScaleFactor{1000.0});  //
}

// Large-scale precipitation: lsp -> lsp
auto fixLargeScalePrecip142() {
    return rule(all(matchParams(142)),                                          //
                setKey(&dm::MarsRecord::param, 3062), SetScaleFactor{1000.0});  //
}

// Evaporation: e -> eva
auto fixEvaporation182() {
    return rule(all(matchParams(182)),                                            //
                setKey(&dm::MarsRecord::param, 260259), SetScaleFactor{1000.0});  //
}

// Skin reservoir content: src -> srcon
auto fixSkinReservoirContent198() {
    return rule(all(matchParams(198)),                                            //
                setKey(&dm::MarsRecord::param, 160198), SetScaleFactor{1000.0});  //
}

// Convective snowfall: csf -> csfwe
auto fixConvectiveSnowfall239() {
    return rule(all(matchParams(239)),                                            //
                setKey(&dm::MarsRecord::param, 231057), SetScaleFactor{1000.0});  //
}

// Large-scale snowfall: lsf -> lsfwe
auto fixLargeScaleSnowfall240() {
    return rule(all(matchParams(240)),                                            //
                setKey(&dm::MarsRecord::param, 231058), SetScaleFactor{1000.0});  //
}

// Accumulated freezing rain: fzra -> fzrawe
auto fixFreezingRain228216() {
    return rule(all(matchParams(228216)),                                         //
                setKey(&dm::MarsRecord::param, 231001), SetScaleFactor{1000.0});  //
}


//-----------------------------------------------------------------------------
// Albedo (fraction -> %)
// Required for ERA
//-----------------------------------------------------------------------------

// UV visible albedo for direct radiation: aluvp -> aluvp_p
auto fixAlbedoUvDirect15() {
    return rule(all(matchParams(15)),                                            //
                setKey(&dm::MarsRecord::param, 210199), SetScaleFactor{100.0});  //
}

// UV visible albedo for diffuse radiation: aluvd -> aluvd_p
auto fixAlbedoUvDiffuse16() {
    return rule(all(matchParams(16)),                                            //
                setKey(&dm::MarsRecord::param, 210198), SetScaleFactor{100.0});  //
}

// Near IR albedo for direct radiation: alnip -> alnip_p
auto fixAlbedoNirDirect17() {
    return rule(all(matchParams(17)),                                            //
                setKey(&dm::MarsRecord::param, 210261), SetScaleFactor{100.0});  //
}

// Near IR albedo for diffuse radiation: alnid -> alnid_p
auto fixAlbedoNirDiffuse18() {
    return rule(all(matchParams(18)),                                            //
                setKey(&dm::MarsRecord::param, 210260), SetScaleFactor{100.0});  //
}

// Snow albedo: asn -> asn
auto fixSnowAlbedo32() {
    return rule(all(matchParams(32)),                                            //
                setKey(&dm::MarsRecord::param, 228032), SetScaleFactor{100.0});  //
}

// Albedo (climatological): al -> al
auto fixAlbedoClimatological174() {
    return rule(all(matchParams(174)),                                           //
                setKey(&dm::MarsRecord::param, 260509), SetScaleFactor{100.0});  //
}

// Forecast albedo: fal -> al
auto fixForecastAlbedo243() {
    return rule(all(matchParams(243)),                                           //
                setKey(&dm::MarsRecord::param, 260509), SetScaleFactor{100.0});  //
}

// UV visible albedo for direct radiation, isotropic: aluvpi -> aluvpi_p
auto fixAlbedoUvDirectIso210186() {
    return rule(all(matchParams(210186)),                                        //
                setKey(&dm::MarsRecord::param, 210201), SetScaleFactor{100.0});  //
}

// UV visible albedo for direct radiation, volumetric: aluvpv -> aluvpv_p
auto fixAlbedoUvDirectVol210187() {
    return rule(all(matchParams(210187)),                                        //
                setKey(&dm::MarsRecord::param, 210202), SetScaleFactor{100.0});  //
}

// UV visible albedo for direct radiation, geometric: aluvpg -> aluvpg_p
auto fixAlbedoUvDirectGeo210188() {
    return rule(all(matchParams(210188)),                                        //
                setKey(&dm::MarsRecord::param, 210200), SetScaleFactor{100.0});  //
}

// Near IR albedo for direct radiation, isotropic: alnipi -> alnipi_p
auto fixAlbedoNirDirectIso210189() {
    return rule(all(matchParams(210189)),                                        //
                setKey(&dm::MarsRecord::param, 210263), SetScaleFactor{100.0});  //
}

// Near IR albedo for direct radiation, volumetric: alnipv -> alnipv_p
auto fixAlbedoNirDirectVol210190() {
    return rule(all(matchParams(210190)),                                        //
                setKey(&dm::MarsRecord::param, 210264), SetScaleFactor{100.0});  //
}

// Near IR albedo for direct radiation, geometric: alnipg -> alnipg_p
auto fixAlbedoNirDirectGeo210191() {
    return rule(all(matchParams(210191)),                                        //
                setKey(&dm::MarsRecord::param, 210262), SetScaleFactor{100.0});  //
}


//-----------------------------------------------------------------------------
// ERA other water-cycle parameters
//-----------------------------------------------------------------------------

// Snow evaporation: es -> eswe
auto fixSnowEvaporation44() {
    return rule(all(matchParams(44)),                                             //
                setKey(&dm::MarsRecord::param, 231003), SetScaleFactor{1000.0});  //
}

// Snowmelt: smlt -> snom
auto fixSnowmelt45() {
    return rule(all(matchParams(45)),                                           //
                setKey(&dm::MarsRecord::param, 3099), SetScaleFactor{1000.0});  //
}

// Total column ozone: tco3 -> tcioz (kg m-2 -> DU, 1 DU = 2.1415e-5 kg m-2)
auto fixTotalColumnOzone206() {
    return rule(all(matchParams(206)),                                              //
                setKey(&dm::MarsRecord::param, 260132), SetScaleFactor{46698.05});  //
}

// Potential evaporation: pev -> peva
auto fixPotentialEvaporation228251() {
    return rule(all(matchParams(228251)),                                         //
                setKey(&dm::MarsRecord::param, 231005), SetScaleFactor{1000.0});  //
}


//-----------------------------------------------------------------------------
// Time-mean rate parameters (m/s -> mm/s)
//-----------------------------------------------------------------------------

// Time-mean surface runoff rate: msror -> avg_surfror
auto fixTimeMeanSurfaceRunoffRate172008() {
    return rule(all(matchParams(172008)),                                         //
                setKey(&dm::MarsRecord::param, 235020), SetScaleFactor{1000.0});  //
}

// Time-mean sub-surface runoff rate: mssror -> avg_ssurfror
auto fixTimeMeanSubSurfaceRunoffRate172009() {
    return rule(all(matchParams(172009)),                                         //
                setKey(&dm::MarsRecord::param, 235021), SetScaleFactor{1000.0});  //
}

// Time-mean snow evaporation rate: esrate -> avg_esrwe
auto fixTimeMeanSnowEvapRate172044() {
    return rule(all(matchParams(172044)),                                         //
                setKey(&dm::MarsRecord::param, 235023), SetScaleFactor{1000.0});  //
}

// Time-mean snowmelt rate -> avg_smr
auto fixTimeMeanSnowmeltRate172045() {
    return rule(all(matchParams(172045)),                                         //
                setKey(&dm::MarsRecord::param, 235024), SetScaleFactor{1000.0});  //
}

// Time-mean large-scale precipitation rate: mlsprt -> avg_lsprate
auto fixTimeMeanLargeScalePrecipRate172142() {
    return rule(all(matchParams(172142)),                                         //
                setKey(&dm::MarsRecord::param, 235029), SetScaleFactor{1000.0});  //
}

// Time-mean convective precipitation rate: cprate -> avg_cpr
auto fixTimeMeanConvPrecipRate172143() {
    return rule(all(matchParams(172143)),                                         //
                setKey(&dm::MarsRecord::param, 235030), SetScaleFactor{1000.0});  //
}

// Time-mean total snowfall rate: mtsfr -> avg_tsrwe
auto fixTimeMeanTotalSnowfallRate172144() {
    return rule(all(matchParams(172144)),                                         //
                setKey(&dm::MarsRecord::param, 235031), SetScaleFactor{1000.0});  //
}

// Time-mean evaporation rate: erate -> avg_ie
auto fixTimeMeanEvaporationRate172182() {
    return rule(all(matchParams(172182)),                                         //
                setKey(&dm::MarsRecord::param, 235043), SetScaleFactor{1000.0});  //
}

// Time-mean runoff rate: mrort -> avg_rorwe
auto fixTimeMeanRunoffRate172205() {
    return rule(all(matchParams(172205)),                                         //
                setKey(&dm::MarsRecord::param, 235048), SetScaleFactor{1000.0});  //
}

// Time-mean total precipitation rate: tprate -> avg_tprate
auto fixTimeMeanTotalPrecipRate172228() {
    return rule(all(matchParams(172228)),                                         //
                setKey(&dm::MarsRecord::param, 235055), SetScaleFactor{1000.0});  //
}

// Time-mean snow depth: avg_sd_m -> avg_sd
auto fixTimeMeanSnowDepth235141() {
    return rule(all(matchParams(235141)),                                         //
                setKey(&dm::MarsRecord::param, 235078), SetScaleFactor{1000.0});  //
}


//-----------------------------------------------------------------------------
// Time-mean cloud cover and albedo (fraction -> %)
// New in ecCodes 2.38
//-----------------------------------------------------------------------------

// Time-mean low cloud cover: avg_lcc_frac -> avg_lcc
auto fixTimeMeanLowCloudCover235186() {
    return rule(all(matchParams(235186)),                                        //
                setKey(&dm::MarsRecord::param, 235108), SetScaleFactor{100.0});  //
}

// Time-mean medium cloud cover: avg_mcc_frac -> avg_mcc
auto fixTimeMeanMedCloudCover235187() {
    return rule(all(matchParams(235187)),                                        //
                setKey(&dm::MarsRecord::param, 235109), SetScaleFactor{100.0});  //
}

// Time-mean high cloud cover: avg_hcc_frac -> avg_hcc
auto fixTimeMeanHighCloudCover235188() {
    return rule(all(matchParams(235188)),                                        //
                setKey(&dm::MarsRecord::param, 235110), SetScaleFactor{100.0});  //
}

// Time-mean forecast albedo: avg_fal_frac -> avg_al
auto fixTimeMeanForecastAlbedo235243() {
    return rule(all(matchParams(235243)),                                        //
                setKey(&dm::MarsRecord::param, 235263), SetScaleFactor{100.0});  //
}


//-----------------------------------------------------------------------------
// Fix Timespan
//-----------------------------------------------------------------------------

auto fixTimespanMax2T() {
    return rule(all(matchParams(121, 228026, 201)),       //
                setKey(&dm::MarsRecord::param, 237167));  //
}

auto fixTimespanMaxCape() {
    return rule(all(matchParams(228035)),                 //
                setKey(&dm::MarsRecord::param, 237117));  //
}

auto fixTimespanMaxMuCapes() {
    return rule(all(matchParams(228036)),                 //
                setKey(&dm::MarsRecord::param, 237321));  //
}

auto fixTimespanMaxPrecipRate() {
    return rule(all(matchParams(228222, 228224, 228226)),  //
                setKey(&dm::MarsRecord::param, 237055));   //
}

auto fixTimespanMaxWindGust() {
    return rule(all(matchParams(123, 228028, 49)),        //
                setKey(&dm::MarsRecord::param, 237318));  //
}

auto fixTimespanMeanFlashDensity() {
    return rule(all(matchParams(228051, 228057, 228058)),  //
                setKey(&dm::MarsRecord::param, 235326));   //
}

auto fixTimespanMin2T() {
    return rule(all(matchParams(122, 228027, 202)),       //
                setKey(&dm::MarsRecord::param, 238167));  //
}

auto fixTimespanMinPrecipRate() {
    return rule(all(matchParams(228223, 228225, 228227)),  //
                setKey(&dm::MarsRecord::param, 238055));   //
}

auto fixTimespanModePrecip() {
    return rule(all(matchParams(260320, 260321, 260339)),  //
                setKey(&dm::MarsRecord::param, 260683));   //
}

auto fixTimespanSeverityPrecip() {
    return rule(all(matchParams(260318, 260319, 260338)),  //
                setKey(&dm::MarsRecord::param, 260682));   //
}


//-----------------------------------------------------------------------------
// Fix Windspeed
//-----------------------------------------------------------------------------

auto fixWindspeedU100m() {
    return rule(all(OneOf{&dm::MarsRecord::levtype, {dm::LevType::SFC}}, matchParams(228246)),  //
                setKey(&dm::MarsRecord::levtype, dm::LevType::HL), setKey(&dm::MarsRecord::levelist, 100),
                setKey(&dm::MarsRecord::param, 131));  //
}

auto fixWindspeedU200m() {
    return rule(all(OneOf{&dm::MarsRecord::levtype, {dm::LevType::SFC}}, matchParams(228239)),  //
                setKey(&dm::MarsRecord::levtype, dm::LevType::HL), setKey(&dm::MarsRecord::levelist, 200),
                setKey(&dm::MarsRecord::param, 131));  //
}

auto fixWindspeedV100m() {
    return rule(all(OneOf{&dm::MarsRecord::levtype, {dm::LevType::SFC}}, matchParams(228247)),  //
                setKey(&dm::MarsRecord::levtype, dm::LevType::HL), setKey(&dm::MarsRecord::levelist, 100),
                setKey(&dm::MarsRecord::param, 132));  //
}

auto fixWindspeedV200m() {
    return rule(all(OneOf{&dm::MarsRecord::levtype, {dm::LevType::SFC}}, matchParams(228240)),  //
                setKey(&dm::MarsRecord::levtype, dm::LevType::HL), setKey(&dm::MarsRecord::levelist, 200),
                setKey(&dm::MarsRecord::param, 132));  //
}

auto fixWindspeed100m() {
    return rule(all(OneOf{&dm::MarsRecord::levtype, {dm::LevType::SFC}}, matchParams(228241)),  //
                setKey(&dm::MarsRecord::levtype, dm::LevType::HL), setKey(&dm::MarsRecord::levelist, 200),
                setKey(&dm::MarsRecord::param, 10));  //
}

auto fixWindspeed200m() {
    return rule(all(OneOf{&dm::MarsRecord::levtype, {dm::LevType::SFC}}, matchParams(228249)),  //
                setKey(&dm::MarsRecord::levtype, dm::LevType::HL), setKey(&dm::MarsRecord::levelist, 100),
                setKey(&dm::MarsRecord::param, 10));  //
}


//-----------------------------------------------------------------------------
// Wave rules bits per value
//-----------------------------------------------------------------------------


auto ruleWaveBitsPerValueS1() {
    return rule(all(greaterThan(&dm::MarsRecord::step, std::int64_t{1}),
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
// Map incremental fields for type=4i
//-----------------------------------------------------------------------------

const RuleList& incrementalType4IRules() {
    static auto type4i_ = ruleList(rule(matchParams(200130), setKey(&dm::MarsRecord::param, 130)),
                                   rule(matchParams(200133), setKey(&dm::MarsRecord::param, 133)),
                                   rule(matchParams(200138), setKey(&dm::MarsRecord::param, 138)),
                                   rule(matchParams(200152), setKey(&dm::MarsRecord::param, 152)),
                                   rule(matchParams(200155), setKey(&dm::MarsRecord::param, 155)),
                                   rule(matchParams(200203), setKey(&dm::MarsRecord::param, 203)));
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
    static auto all_ = ruleList(incrementalType4IRules(), fixIFSOutput(), mapDeprecatedGrib1ToGrib2());
    return all_;
}

const RuleList& allRules() {
    static auto all_
        = ruleList(wmoUnitMapping(), incrementalType4IRules(), fixIFSOutput(), mapDeprecatedGrib1ToGrib2());
    return all_;
}

std::optional<MappingResult> applyMappings(const RuleList& rules, dm::MarsRecord& mars, dm::MiscRecord& misc) {
    return rules(mars, misc);
}

}  // namespace multio::mars2mars

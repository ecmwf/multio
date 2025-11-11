#include "multio/mars2grib/Rules.h"
#include "multio/datamod/GribKeys.h"
#include "multio/datamod/MarsMiscGeo.h"
#include "multio/datamod/core/EntryDef.h"
#include "multio/datamod/core/Record.h"
#include "multio/datamod/types/TypeOfLevel.h"
#include "multio/datamod/types/TypeOfStatisticalProcessing.h"
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

using TOL = dm::TypeOfLevel;
using TOSP = dm::TypeOfStatisticalProcessing;

namespace dm = multio::datamod;

//-----------------------------------------------------------------------------
// Matchers
//-----------------------------------------------------------------------------

auto matchChemical() {
    return all(Has{&dm::FullMarsRecord::chem}, Missing{&dm::FullMarsRecord::wavelength}, lessThan(&dm::FullMarsRecord::chem, 900));
}

auto matchAerosol() {
    return all(Has{&dm::FullMarsRecord::chem}, Missing{&dm::FullMarsRecord::wavelength}, greaterEqual(&dm::FullMarsRecord::chem, 900));
}

auto matchOptical() {
    return all(Missing{&dm::FullMarsRecord::chem}, Has{&dm::FullMarsRecord::wavelength});
}
auto matchChemicalOptical() {
    return all(Has{&dm::FullMarsRecord::chem}, Has{&dm::FullMarsRecord::wavelength});
}

auto matchLevType(dm::LevType lt) {
    return OneOf{&dm::FullMarsRecord::levtype, {lt}};
}

auto matchSatellite() {
    // Satellite data can be identified by having the keyword CHANNEL:
    // https://apps.ecmwf.int/mars-catalogue/?stream=oper&levtype=sfc&expver=1&month=nov&year=2025&date=2025-11-03&type=ssd&class=od
    //
    // Other satellite related keywords like instrument and ident is not always given because channel is actually
    // holding a channel like index as combination of instrument, indent and the true channel:
    // https://apps.ecmwf.int/mars-catalogue/?stream=elda&levtype=sfc&expver=1&month=nov&year=2025&type=em&class=od
    return all(Missing{&dm::FullMarsRecord::levtype}, Has{&dm::FullMarsRecord::channel});
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
auto timeRange(TimeRangeType type, TOSP typeOfStatisticalProcessing) {
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
auto derivedForecast() {
    return Setter([](SectionsConf& c) {
        c.product.ensureInit().modify().pdtCat.ensureInit().modify().processType.set(ProcessType::DerivedForecast);
        c.product.ensureInit().modify().process.ensureInit();
        c.product.ensureInit().modify().process.modify().type.set("derived");
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

auto typeOfLevel(TOL type) {
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
    return rule(OneOf{&dm::FullMarsRecord::repres, {repres}},
                Setter([=](SectionsConf& c) { c.grid.ensureInit().modify().templateNumber.set(num); }));
}

auto gridRules() {
    return exclusiveRuleList("gridRules",  //
                             makeGridRule(dm::Repres::LL, 0), makeGridRule(dm::Repres::GG, 40),
                             makeGridRule(dm::Repres::SH, 50));
}

auto localSectionRules() {
    return exclusiveRuleList(  //
        "localSectionRules",   //
        rule(all(Missing{&dm::FullMarsRecord::anoffset}, NoneOf{&dm::FullMarsRecord::klass, {"d1"}},
                 Missing{&dm::FullMarsRecord::method}, Missing{&dm::FullMarsRecord::channel}),
             localUse(1)),
        rule(all(Missing{&dm::FullMarsRecord::anoffset}, NoneOf{&dm::FullMarsRecord::klass, {"d1"}},
                 Has{&dm::FullMarsRecord::method}),
             localUse(15)),
        // DerivedEnsembleForecasts (type=em/es can be combined with satellite (have channel) - there is no valid PDT
        // for this - hence the channel information is stored in the local 14 template. Also notice that for these
        // templates channel acts as an index from which instrument can be inferred (this is not documented and
        // implemented anywhere)
        rule(all(Missing{&dm::FullMarsRecord::anoffset}, NoneOf{&dm::FullMarsRecord::klass, {"d1"}},
                 Missing{&dm::FullMarsRecord::method}, Has{&dm::FullMarsRecord::channel},
                 OneOf{&dm::FullMarsRecord::type, {"em", "es"}}),
             localUse(14)),
        rule(all(Has{&dm::FullMarsRecord::anoffset}, NoneOf{&dm::FullMarsRecord::klass, {"d1"}}), localUse(36)),
        rule(all(Missing{&dm::FullMarsRecord::anoffset}, OneOf{&dm::FullMarsRecord::klass, {"d1"}}), localUse(1001)),
        rule(all(Has{&dm::FullMarsRecord::anoffset}, OneOf{&dm::FullMarsRecord::klass, {"d1"}}), localUse(1036)));
}

auto processTypesRules() {
    return exclusiveRuleList(
        "processTypesRules",
        // Match any levtype but AL (or no levtype)
        rule(all(NoneOf{&dm::FullMarsRecord::levtype, {dm::LevType::AL}}, Missing{&dm::FullMarsRecord::number}, Missing{&dm::FullMarsRecord::hdate}, NoneOf{&dm::FullMarsRecord::type, {"em", "es"}})),
        rule(all(NoneOf{&dm::FullMarsRecord::levtype, {dm::LevType::AL}}, Has{&dm::FullMarsRecord::number}, Missing{&dm::FullMarsRecord::hdate}), ensemble()),
        rule(all(NoneOf{&dm::FullMarsRecord::levtype, {dm::LevType::AL}}, Has{&dm::FullMarsRecord::number}, Has{&dm::FullMarsRecord::hdate}), reforecast(), ensemble()),
        rule(all(NoneOf{&dm::FullMarsRecord::levtype, {dm::LevType::AL}}, Missing{&dm::FullMarsRecord::number}, Missing{&dm::FullMarsRecord::hdate}, OneOf{&dm::FullMarsRecord::type, {"em", "es"}}),
             derivedForecast(), ensemble()),
        // Levtype AL specific - detection whether a largeEnsemble is used should actually depend on
        // numberOfForecastsInEnsemble > 254
        rule(all(matchLevType(dm::LevType::AL), Has{&dm::FullMarsRecord::number}, Missing{&dm::FullMarsRecord::hdate}), largeEnsemble()),
        rule(all(matchLevType(dm::LevType::AL), Has{&dm::FullMarsRecord::number}, Has{&dm::FullMarsRecord::hdate}), reforecast(), largeEnsemble()));
}

auto packingRules() {
    return exclusiveRuleList(                                 //
        "packingRules",
        rule(OneOf{&dm::FullMarsRecord::packing, {"simple"}}, dataRepres(0)),  //
        rule(OneOf{&dm::FullMarsRecord::packing, {"ccsds"}}, dataRepres(42)),  //
        rule(OneOf{&dm::FullMarsRecord::packing, {"complex"}}, dataRepres(51)));
}


//-----------------------------------------------------------------------------
// Params
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// Satellite
//-----------------------------------------------------------------------------

// Single satellite rule - defined here to be checked twice - with levtype sfc
// and without any levtype but ident, instrument and channel
auto singleSatelliteRule() {
    return rule(matchParams(paramRange(260510, 260512)),  //
                pointInTime(),                            //
                satellite());
}

// derivedEnsembleProduct satellite rules --- for these cases the satellite information is stored in
// a local template. This is explicitly handled  in the `localSectionRules()`.
auto derivedEnsembleForecastSatelliteRule() {
    // TODO(pgeier) Reintroduce strict checks an missing timespan etc..
    //              194 is coming from fdb prod - the grib1-to-grib2 tool still emits timespan although 192 is not
    //              statistical
    // return rule(all(Missing{dm::TIMESPAN}, Missing{dm::STATTYPE}, matchParams(194)),  //
    //             pointInTime(), typeOfLevel(TOL::Surface));                            //
    return rule(matchParams(194),                           //
                pointInTime(), typeOfLevel(TOL::Surface));  //
}

auto satelliteRules() {
    return exclusiveRuleList(
        "satelliteRules",
        chainedRuleList(rule(all(Has{&dm::FullMarsRecord::channel}, NoneOf{&dm::FullMarsRecord::type, {"em", "es"}})),
                        singleSatelliteRule()),  //
        chainedRuleList(rule(all(Has{&dm::FullMarsRecord::channel}, OneOf{&dm::FullMarsRecord::type, {"em", "es"}})),
                        derivedEnsembleForecastSatelliteRule()),  //
        chainedRuleList(rule(Missing{&dm::FullMarsRecord::channel})));
}

//-----------------------------------------------------------------------------
// SFC
//-----------------------------------------------------------------------------

auto paramSFCRules() {
    return exclusiveRuleList(
        "paramSFCRules",                                   //
        rule(matchParams(228023),                          //
             pointInTime(), typeOfLevel(TOL::CloudBase)),  //
        rule(matchParams(                                  //
                 59, 78, 79, 136, 137, 164, 206, paramRange(162059, 162063), 162071, 162072, 162093, 228001, 228044,
                 228050, 228052, 228088, 228089, 228090, 228164, 260132),  //
             pointInTime(), typeOfLevel(TOL::EntireAtmosphere)),           //
        rule(matchParams(228007, 228011),                                  //
             pointInTime(), typeOfLevel(TOL::EntireLake)),                 //
        rule(matchParams(121),                                             //
             timeRange(TimeRangeType::FixedTimeRange, TOSP::Maximum),
             overallLengthOfTimeRange("6h"),                           //
             typeOfLevel(TOL::HeightAboveGroundAt2m), fixedLevel(2)),  //
        rule(matchParams(122),                                         //
             timeRange(TimeRangeType::FixedTimeRange, TOSP::Minimum),
             overallLengthOfTimeRange("6h"),                                                  //
             typeOfLevel(TOL::HeightAboveGroundAt2m), fixedLevel(2)),                         //
        rule(matchParams(201, 237167, 237168),                                                //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Maximum),            //
             typeOfLevel(TOL::HeightAboveGroundAt2m), fixedLevel(2)),                         //
        rule(matchParams(202, 238167, 238168),                                                //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Minimum),            //
             typeOfLevel(TOL::HeightAboveGroundAt2m), fixedLevel(2)),                         //
        rule(matchParams(228004, 235168),                                                     //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Average),            //
             typeOfLevel(TOL::HeightAboveGroundAt2m), fixedLevel(2)),                         //
        rule(matchParams(239167, 239168),                                                     //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::StandardDeviation),  //
             typeOfLevel(TOL::HeightAboveGroundAt2m), fixedLevel(2)),                         //
        rule(matchParams(123),                                                                //
             timeRange(TimeRangeType::FixedTimeRange, TOSP::Maximum),                         //
             overallLengthOfTimeRange("6h"),                                                  //
             typeOfLevel(TOL::HeightAboveGroundAt10m), fixedLevel(10)),                       //
        rule(matchParams(228028),                                                             //
             timeRange(TimeRangeType::FixedTimeRange, TOSP::Maximum),
             overallLengthOfTimeRange("3h"),                                        //
             typeOfLevel(TOL::HeightAboveGroundAt10m), fixedLevel(10)),             //
        rule(matchParams(49, 237165, 237166, 237207, 237318),                       //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Maximum),  //
             typeOfLevel(TOL::HeightAboveGroundAt10m), fixedLevel(10)),             //
        rule(matchParams(141233, 141245),                                           //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Average),  //
             typeOfLevel(TOL::HeightAboveSeaAt10m), fixedLevel(10)),                //
        rule(matchParams(143233, 143245),                                           //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Maximum),  //
             typeOfLevel(TOL::HeightAboveSeaAt10m), fixedLevel(10)),                //
        rule(matchParams(144233, 144245),                                           //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Minimum),  //
             typeOfLevel(TOL::HeightAboveSeaAt10m), fixedLevel(10)),                //
        rule(matchParams(145233, 145245),                                                     //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::StandardDeviation),  //
             typeOfLevel(TOL::HeightAboveSeaAt10m), fixedLevel(10)),                          //
        rule(matchParams(235087, 235088, 235136, 235137, 235087, 235088, 235137, 235288, 235287, 235290, 235326,
                         235383),                                                                             //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Average),                            //
             typeOfLevel(TOL::EntireAtmosphere)),                                                             //
        rule(matchParams(237087, 237088, 237326, 237288, 237290, 237137, 237287),                             //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Maximum),                            //
             typeOfLevel(TOL::EntireAtmosphere)),                                                             //
        rule(matchParams(238087, 238088, 238326, 238288, 238290, 238137, 238287),                             //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Minimum),                            //
             typeOfLevel(TOL::EntireAtmosphere)),                                                             //
        rule(matchParams(239087, 239088, 239326, 239288, 239290, 239137, 239287),                             //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::StandardDeviation),                  //
             typeOfLevel(TOL::EntireAtmosphere)),                                                             //
        rule(matchParams(228005, 235165, 235166),                                                             //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Average),                            //
             typeOfLevel(TOL::HeightAboveGroundAt10m), fixedLevel(10)),                                       //
        rule(matchParams(238165, 238166, 238207),                                                             //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Minimum),                            //
             typeOfLevel(TOL::HeightAboveGroundAt10m), fixedLevel(10)),                                       //
        rule(matchParams(239165, 239166, 239207),                                                             //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::StandardDeviation),                  //
             typeOfLevel(TOL::HeightAboveGroundAt10m), fixedLevel(10)),                                       //
        rule(matchParams(235151),                                                                             //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Average),                            //
             typeOfLevel(TOL::MeanSea)),                                                                      //
        rule(matchParams(237151),                                                                             //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Maximum),                            //
             typeOfLevel(TOL::MeanSea)),                                                                      //
        rule(matchParams(238151),                                                                             //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Minimum),                            //
             typeOfLevel(TOL::MeanSea)),                                                                      //
        rule(matchParams(239151),                                                                             //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::StandardDeviation),                  //
             typeOfLevel(TOL::MeanSea)),                                                                      //
        rule(matchParams(235039, 235040, 235049, 235050, 235053),                                             //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Average),                            //
             typeOfLevel(TOL::NominalTop)),                                                                   //
        rule(matchParams(paramRange(141101, 141105), 141208, 141209, 141215, 141216, 141220, 141229, 141232,  //
                         235020, 235021, paramRange(235029, 235031), paramRange(235033, 235038),              //
                         paramRange(235041, 235043), 235048, 235051, 235052, 235055, 235058,                  //
                         paramRange(235078, 235080), 235083, 235084, 235093, 235134, 235159, 235189, 235263,  //
                         235283, 235339),                                                                     //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Average),                            //
             typeOfLevel(TOL::Surface)),                                                                      //
        rule(matchParams(235108), timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Average),
             typeOfLevel(TOL::LowCloudLayer)),
        rule(matchParams(237108), timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Maximum),
             typeOfLevel(TOL::LowCloudLayer)),
        rule(matchParams(238108), timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Minimum),
             typeOfLevel(TOL::LowCloudLayer)),
        rule(matchParams(239108), timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::StandardDeviation),
             typeOfLevel(TOL::LowCloudLayer)),
        rule(matchParams(235090, 235091), timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Average),
             typeOfLevel(TOL::MixingLayer)),
        rule(matchParams(237090, 237091), timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Maximum),
             typeOfLevel(TOL::MixingLayer)),
        rule(matchParams(238090, 238091), timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Minimum),
             typeOfLevel(TOL::MixingLayer)),
        rule(matchParams(239090, 239091), timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::StandardDeviation),
             typeOfLevel(TOL::MixingLayer)),
        rule(matchParams(235322), timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Average),
             typeOfLevel(TOL::Tropopause)),
        rule(matchParams(237322), timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Maximum),
             typeOfLevel(TOL::Tropopause)),
        rule(matchParams(238322), timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Minimum),
             typeOfLevel(TOL::Tropopause)),
        rule(matchParams(239322), timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::StandardDeviation),
             typeOfLevel(TOL::Tropopause)),
        rule(matchParams(235077, 235094),                                           // 235077 also exists on SOL !
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Average),  //
             typeOfLevel(TOL::SoilLayer)),                                          //
        rule(matchParams(237077, 237094),                                           //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Maximum),  //
             typeOfLevel(TOL::SoilLayer)),                                          //
        rule(matchParams(238077, 238094),                                           //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Minimum),  //
             typeOfLevel(TOL::SoilLayer)),                                          //
        rule(matchParams(239077, 239094),                                           //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::StandardDeviation),  //
             typeOfLevel(TOL::SoilLayer)),                                                    //
        rule(matchParams(235309),                                                             //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Average),            //
             typeOfLevel(TOL::IceLayerOnWater)),                                              //
        rule(matchParams(237309),                                                             //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Maximum),            //
             typeOfLevel(TOL::IceLayerOnWater)),                                              //
        rule(matchParams(238309),                                                             //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Minimum),            //
             typeOfLevel(TOL::IceLayerOnWater)),                                              //
        rule(matchParams(239309),                                                             //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::StandardDeviation),  //
             typeOfLevel(TOL::IceLayerOnWater)),                                              //
        rule(matchParams(263024),                                                             //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Average),            //
             typeOfLevel(TOL::SeaIceLayer)),                                                  //
        rule(matchParams(265024),                                                             //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Maximum),            //
             typeOfLevel(TOL::SeaIceLayer)),                                                  //
        rule(matchParams(266024),                                                             //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Minimum),            //
             typeOfLevel(TOL::SeaIceLayer)),                                                  //
        rule(matchParams(267024),                                                             //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::StandardDeviation),  //
             typeOfLevel(TOL::SeaIceLayer)),                                                  //
        rule(matchParams(260683),                                                             //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Mode),               //
             typeOfLevel(TOL::Surface)),                                                      //
        rule(matchParams(260682),                                                             //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Severity),           //
             typeOfLevel(TOL::Surface)),                                                      //
        rule(matchParams(129172),                                                             //
             pointInTime(),                                                                   //
             typeOfLevel(TOL::HeightAboveGround)),                                            //
        rule(matchParams(165, 166, 207, 228029, 228131, 228132, 260260),                      //
             pointInTime(),                                                                   //
             typeOfLevel(TOL::HeightAboveGroundAt10m), fixedLevel(10)),                       //
        rule(matchParams(167, 168, 174096, 228037, 260242),                                   //
             pointInTime(),                                                                   //
             typeOfLevel(TOL::HeightAboveGroundAt2m), fixedLevel(2)),                         //
        rule(matchParams(140245, 140249, 140233),                                             //
             pointInTime(),                                                                   //
             typeOfLevel(TOL::HeightAboveSeaAt10m), fixedLevel(10)),                          //
        rule(matchParams(3075),                                                               //
             pointInTime(),                                                                   //
             typeOfLevel(TOL::HighCloudLayer)),                                               //
        rule(matchParams(3074),                                                               //
             pointInTime(),                                                                   //
             typeOfLevel(TOL::MediumCloudLayer)),                                             //
        rule(matchParams(3073),                                                               //
             pointInTime(),                                                                   //
             typeOfLevel(TOL::LowCloudLayer)),                                                //
        rule(matchParams(228014),                                                             //
             pointInTime(),                                                                   //
             typeOfLevel(TOL::IceLayerOnWater)),                                              //
        rule(matchParams(228013),                                                             //
             pointInTime(),                                                                   //
             typeOfLevel(TOL::IceTopOnWater)),                                                //
        rule(matchParams(228010),                                                             //
             pointInTime(),                                                                   //
             typeOfLevel(TOL::LakeBottom)),                                                   //
        rule(matchParams(235305),
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Average),
             typeOfLevel(TOL::LakeBottom)),
        rule(matchParams(237305),
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Maximum),
             typeOfLevel(TOL::LakeBottom)),
        rule(matchParams(238305),
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Minimum),
             typeOfLevel(TOL::LakeBottom)),
        rule(matchParams(239305),
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::StandardDeviation),
             typeOfLevel(TOL::LakeBottom)),
        rule(matchParams(151),                                                                //
             pointInTime(),                                                                   //
             typeOfLevel(TOL::MeanSea)),                                                      //
        rule(matchParams(262118),                                                             //
             pointInTime(),                                                                   //
             typeOfLevel(TOL::DepthBelowSeaLayer)),                                           //
        rule(matchParams(228231, 228232, 228233, 228234),                                     //
             pointInTime(),                                                                   //
             typeOfLevel(TOL::MixedLayerParcel)),                                             //
        rule(matchParams(228008, 228009),                                                     //
             pointInTime(),                                                                   //
             typeOfLevel(TOL::MixingLayer)),                                                  //
        rule(matchParams(228235, 228236, 228237),                                             //
             pointInTime(),                                                                   //
             typeOfLevel(TOL::MostUnstableParcel)),                                           //
        rule(matchParams(178, 179, 208, 209, 212),                                            //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Accumulation),       //
             typeOfLevel(TOL::NominalTop)),                                                   //
        rule(matchParams(228045),                                                             //
             pointInTime(),                                                                   //
             typeOfLevel(TOL::Tropopause)),                                                   //
        rule(all(                                                                             //
                 matchParams(228080, 228081, 228082, paramRange(233032, 233035), 235062, 235063, 235064),
                 matchChemical()),                                                       //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Accumulation),  //
             chemical(),                                                                 //
             typeOfLevel(TOL::Surface),                                                  //
             tablesConfig("custom"), localTablesVersion(0), tablesVersion(30)),          //
        rule(
            matchParams(  //
                8, 9, 20, 44, 45, 47, 50, 57, 58, paramRange(142, 147), 169, 175, 176, 177, 180, 181, 182, 189, 195,
                196, 197, 205, 210, 211, 213, 228, 239, 240, 3062, 3099, paramRange(162100, 162113),
                paramRange(222001, 222256), 228021, 228022, 228129, 228130, 228143, 228144, 228216, 228228, 228251,
                231001, 231002, 231003, 231005, 231010, 231012, 231057, 231058, paramRange(233000, 233031), 260259),  //
            timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Accumulation),
            typeOfLevel(TOL::Surface)),    //
        rule(matchParams(228051, 228053),  //
             timeRange(TimeRangeType::FixedTimeRange, TOSP::Average),
             overallLengthOfTimeRange("1h"),  //
             typeOfLevel(TOL::Surface)),      //
        rule(matchParams(228057, 228059),     //
             timeRange(TimeRangeType::FixedTimeRange, TOSP::Average),
             overallLengthOfTimeRange("3h"),  //
             typeOfLevel(TOL::Surface)),      //
        rule(matchParams(228058, 228060),     //
             timeRange(TimeRangeType::FixedTimeRange, TOSP::Average),
             overallLengthOfTimeRange("6h"),  //
             typeOfLevel(TOL::Surface)),      //
        rule(matchParams(228026, 228222),     //
             timeRange(TimeRangeType::FixedTimeRange, TOSP::Maximum),
             overallLengthOfTimeRange("3h"),       //
             typeOfLevel(TOL::Surface)),           //
        rule(matchParams(228224, 228035, 228036),  //
             timeRange(TimeRangeType::FixedTimeRange, TOSP::Maximum),
             overallLengthOfTimeRange("6h"),  //
             typeOfLevel(TOL::Surface)),      //
        rule(matchParams(228027, 228223),     //
             timeRange(TimeRangeType::FixedTimeRange, TOSP::Minimum),
             overallLengthOfTimeRange("3h"),  //
             typeOfLevel(TOL::Surface)),      //
        rule(matchParams(228225),             //
             timeRange(TimeRangeType::FixedTimeRange, TOSP::Minimum),
             overallLengthOfTimeRange("6h"),  //
             typeOfLevel(TOL::Surface)),      //
        rule(matchParams(260320),             //
             timeRange(TimeRangeType::FixedTimeRange, TOSP::Mode),
             overallLengthOfTimeRange("1h"),  //
             typeOfLevel(TOL::Surface)),      //
        rule(matchParams(260321),             //
             timeRange(TimeRangeType::FixedTimeRange, TOSP::Mode),
             overallLengthOfTimeRange("3h"),  //
             typeOfLevel(TOL::Surface)),      //
        rule(matchParams(260339),             //
             timeRange(TimeRangeType::FixedTimeRange, TOSP::Mode),
             overallLengthOfTimeRange("6h"),  //
             typeOfLevel(TOL::Surface)),      //
        rule(matchParams(260318),             //
             timeRange(TimeRangeType::FixedTimeRange, TOSP::Severity),
             overallLengthOfTimeRange("1h"),  //
             typeOfLevel(TOL::Surface)),      //
        rule(matchParams(260319),             //
             timeRange(TimeRangeType::FixedTimeRange, TOSP::Severity),
             overallLengthOfTimeRange("3h"),  //
             typeOfLevel(TOL::Surface)),      //
        rule(matchParams(260338),             //
             timeRange(TimeRangeType::FixedTimeRange, TOSP::Severity),
             overallLengthOfTimeRange("6h"),  //
             typeOfLevel(TOL::Surface)),      //
        rule(matchParams(                     //
                 paramRange(15, 18), paramRange(26, 32), 33, paramRange(34, 43), paramRange(66, 67), 74, 129, 134, 139,
                 141, 148, 159, paramRange(160, 163), 170, paramRange(172, 174), paramRange(186, 188), 198,
                 paramRange(229, 232), paramRange(234, 236), 238, paramRange(243, 245), 3020, 3067, 160198, 200199,
                 210200, 210201, 210202, 228003, 228012, paramRange(210186, 210191), 210262, 210263, 210264,
                 paramRange(228015, 228020), 228024, 228032, paramRange(228046, 228048), 228141,
                 paramRange(228217, 228221), 260004, 260005, 260015, 260038, 260048, 260109, 260121, 260123, 260255,
                 260289, 260292, 260293, 260509, 260688, 261001, 261002, 261014, 261015, 261016, 261018, 262000, 262100,
                 262139, 262140, 262144, 262124),          //
             pointInTime(),                                //
             typeOfLevel(TOL::Surface)),                   //
        rule(all(                                          //
                 matchParams(paramRange(228083, 228085)),  //
                 matchChemical()),                         //
             pointInTime(),                                //
             chemical(),
             typeOfLevel(TOL::Surface)),  //
        rule(matchParams(                 //
                 paramRange(140098, 140105), paramRange(140112, 140113), paramRange(140121, 140129),
                 paramRange(140131, 140134), paramRange(140207, 140209), paramRange(140211, 140212),
                 paramRange(140214, 140232), paramRange(140234, 140239), 140244, paramRange(140246, 140248),
                 paramRange(140252, 140254)),          //
             pointInTime(),                            //
             typeOfLevel(TOL::Surface)),               //
        rule(matchParams(paramRange(140114, 140120)),  //
             pointInTime(),                            //
             periodRange(),
             typeOfLevel(TOL::Surface)),  //
        rule(matchParams(paramRange(143101, 143105), 143208, 143209, 143215, 143216, 143220, 143229, 143232,              //
                         228226, 237013, 237041, 237042, 237055, 237078, 237080, 237083, 237084, 237093, 237117, 237134,  //
                         237159, 237263, 237321),                                                                         //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Maximum),                                        //
             typeOfLevel(TOL::Surface)),                                                                                  //
        rule(matchParams(paramRange(144101, 144105), 144208, 144209, 144215, 144216, 144220, 144229, 144232,                       //
                         228227, 238055, 238013, 238041, 238042, 238078, 238080, 238083, 238084, 238093, 238134, 238159, 238263),  //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Minimum),                                                 //
             typeOfLevel(TOL::Surface)),                                                                                           //
        rule(matchParams(paramRange(145101, 145105), 145208, 145209, 145215, 145216, 145220, 145229, 145232,  //
             239041, 239042, 239078, 239080, 239083, 239084, 239093, 239134, 239159, 239263),                 //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::StandardDeviation),                  //
             typeOfLevel(TOL::Surface)),                                                                      //
        rule(matchParams(140251),                                                                          //
             pointInTime(),                                                                                //
             dirFreq()),                                                                                   //
        rule(matchParams(262104),                                                                          //
             pointInTime(),                                                                                //
             typeOfLevel(TOL::Isothermal)),                                                                //

        //-----------------------------------------------------------------------------
        // Satellite - this is supported for ERA6. MultIOM is emiting levtype: sfc
        // TODO(pgeier) Fix should be to not map satellite with levtype sfc
        //-----------------------------------------------------------------------------
        singleSatelliteRule()

    );
}


auto paramHLRules() {
    return exclusiveRuleList(                                                                 //
        "paramHLRules",                                                                       //
        rule(matchParams(10, 54, 130, 131, 132, 157, 246, 247, 3031),                         //
             pointInTime(),                                                                   //
             typeOfLevel(TOL::HeightAboveGround)),                                            //
        rule(matchParams(235097, 235131, 235132),                                             //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Average),            //
             typeOfLevel(TOL::HeightAboveGround)),                                            //
        rule(matchParams(237097, 237131, 237132),                                             //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Maximum),            //
             typeOfLevel(TOL::HeightAboveGround)),                                            //
        rule(matchParams(238097, 238131, 238132),                                             //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Minimum),            //
             typeOfLevel(TOL::HeightAboveGround)),                                            //
        rule(matchParams(239097, 239131, 239132),                                             //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::StandardDeviation),  //
             typeOfLevel(TOL::HeightAboveGround))                                             //
    );
}


//-----------------------------------------------------------------------------
// ML
//-----------------------------------------------------------------------------

// NOTE - levtype ML is always mapped to TOL::Hybrid - these rules can be generalized once the time mapping is mapped
// orthogonally
auto paramMLRules() {
    return exclusiveRuleList(  //
        "paramMLRules",        //
        rule(matchParams(21, 22, 23, 75, 76, 77, paramRange(129, 133), 135, 138, 152, 155, 156, 157, 203, 246, 247, 248,
                         260290),                                                        //
             pointInTime(),                                                              //
             typeOfLevel(TOL::Hybrid)),                                                  //
        rule(matchParams(paramRange(162100, 162113)),                                    //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Accumulation),  //
             typeOfLevel(TOL::Hybrid))                                                   //
    );
}

//-----------------------------------------------------------------------------
// PL
//-----------------------------------------------------------------------------

// Special composer to handle pressure units differently
// The exclusion list should be the outermost - hence we are passing in the specializations
template <typename MkTail>
auto plLevelRules(MkTail&& mkTail) {
    return exclusiveRuleList(                      //
        "plLevelRules",                            //
        chainedRuleList(                           //
            rule(greaterEqual(&dm::FullMarsRecord::levelist, 100),  //
                 typeOfLevel(TOL::IsobaricInhPa)),
            mkTail()),                         //
        chainedRuleList(                       //
            rule(lessThan(&dm::FullMarsRecord::levelist, 100),  //
                 typeOfLevel(TOL::IsobaricInPa)),
            mkTail())  //
    );
}

auto paramPLRules() {
    return plLevelRules([]() {
        return exclusiveRuleList(  //
            "paramPLRules",        //
            rule(matchParams(1, 2, 10, 60, 75, 76, paramRange(129, 135), 138, 152, 155, 157, 203, 246, 247, 248, 157,
                             260290),
                 pointInTime()),
            rule(matchParams(235100, paramRange(235129, 235133), 235135, 235138, 235152, 235155, 235157, 235203, 235246,
                             263107),
                 timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Average)));
    });
}


//-----------------------------------------------------------------------------
// PT
//-----------------------------------------------------------------------------

auto paramPTRules() {
    return exclusiveRuleList(                                                       //
        "paramPTRules",                                                             //
        rule(matchParams(53, 54, 60, 131, 132, 133, 138, 155, 203),                 //
             pointInTime(),                                                         //
             typeOfLevel(TOL::Theta)),                                              //
        rule(matchParams(235100, 235203),                                           //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Average),  //
             typeOfLevel(TOL::Theta)),                                              //
        rule(matchParams(237203),                                                   //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Maximum),  //
             typeOfLevel(TOL::Theta)),                                              //
        rule(matchParams(238203),                                                   //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Minimum),  //
             typeOfLevel(TOL::Theta)),                                              //
        rule(matchParams(239203),                                                   //
             timeRange(TimeRangeType::SinceLastPostProcessingStep,
                       TOSP::StandardDeviation),  //
             typeOfLevel(TOL::Theta))             //
    );
}


//-----------------------------------------------------------------------------
// PV
//-----------------------------------------------------------------------------

auto paramPVRules() {
    return exclusiveRuleList(                                                       //
        "paramPVRules",                                                             //
        rule(matchParams(3, 54, 129, 131, 132, 133, 203),                           //
             pointInTime(),                                                         //
             typeOfLevel(TOL::PotentialVorticity)),                                 //
        rule(matchParams(235098, 235269),                                           //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Average),  //
             typeOfLevel(TOL::PotentialVorticity))                                  //
    );
}


//-----------------------------------------------------------------------------
// Soil
//-----------------------------------------------------------------------------

auto paramSOLRules() {
    return exclusiveRuleList(                                                       //
        "paramSOLRules",                                                            //
        rule(matchParams(262000, 262024),                                           //
             pointInTime(),                                                         //
             typeOfLevel(TOL::SeaIceLayer)),                                        //
        rule(matchParams(33, 74, 238, 228038, 228141, 235080, 237080, 238080, 239080),  //
             pointInTime(),                                                         //
             typeOfLevel(TOL::SnowLayer)),                                          //
        rule(matchParams(260360, 260199, 183),                                      //
             pointInTime(),                                                         //
             typeOfLevel(TOL::SoilLayer)),                                          //
        rule(matchParams(235077),                                                   // 235077 also exists on SOL !
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Average),  //
             typeOfLevel(TOL::SoilLayer)),                                          //
        rule(matchParams(235078),                                                   //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Average),  //
             typeOfLevel(TOL::SnowLayer))                                           //
    );
}


//-----------------------------------------------------------------------------
// Al
//-----------------------------------------------------------------------------

auto paramAlRules() {
    return exclusiveRuleList(                          //
        "paramALRules",                                //
        rule(matchParams(paramRange(213101, 213160)),  //
             pointInTime(),                            //
             randomPattern(),                          //
             typeOfLevel(TOL::AbstractSingleLevel))    //
    );
}


//-----------------------------------------------------------------------------
// Final composed param rules
//-----------------------------------------------------------------------------

// TODO can be optimized by pulling the levtype check one level out -
// exclusiveRuleList(
//    chainedRuleList(rule(matchLevType(XXX)), paramXXXRules()),
//    ....
// )

auto horizontalRules() {
    return exclusiveRuleList(  //
        "horizontalRules",     //
        chainedRuleList(rule(all(matchLevType(dm::LevType::SFC), Missing{&dm::FullMarsRecord::channel})),
                        paramSFCRules()),

        // By-passing rules - allow satellite products to pass without effects -- handled in `satelliteRules`
        rule(all(matchLevType(dm::LevType::SFC), Has{&dm::FullMarsRecord::channel})),  //
        rule(all(Missing{&dm::FullMarsRecord::levtype}, Has{&dm::FullMarsRecord::channel})),

        //
        chainedRuleList(rule(matchLevType(dm::LevType::HL)), paramHLRules()),
        chainedRuleList(rule(matchLevType(dm::LevType::ML)), paramMLRules()),
        chainedRuleList(rule(matchLevType(dm::LevType::PL)), paramPLRules()),
        chainedRuleList(rule(matchLevType(dm::LevType::PT)), paramPTRules()),
        chainedRuleList(rule(matchLevType(dm::LevType::PV)), paramPVRules()),
        chainedRuleList(rule(matchLevType(dm::LevType::SOL)), paramSOLRules()),
        chainedRuleList(rule(matchLevType(dm::LevType::AL)), paramAlRules()));
}


//-----------------------------------------------------------------------------
// Big rule tree...
//-----------------------------------------------------------------------------

const ChainedRuleList& allRules() {
    static auto all_ = chainedRuleList(  //
        gridRules(),                     //
        localSectionRules(),             //
        processTypesRules(),             //
        horizontalRules(),               //
        satelliteRules(),                //
        packingRules()                   //
    );

    return all_;
}

SectionsConf buildEncoderConf(const dm::FullMarsRecord& mars) {
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

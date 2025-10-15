#include "multio/mars2grib/Rules.h"
#include "multio/datamod/GribKeys.h"
#include "multio/datamod/MarsMiscGeo.h"
#include "multio/datamod/core/EntryDef.h"
#include "multio/datamod/core/Record.h"
#include "multio/datamod/types/TypeOfLevel.h"
#include "multio/datamod/types/TypeOfStatisticalProcessing.h"
#include "multio/mars2grib/Grib2Layout.h"
#include "multio/mars2grib/LegacyEncoderConf.h"
#include "multio/mars2grib/Mars2GribException.h"
#include "multio/mars2grib/generated/InferPDT.h"
#include "multio/mars2grib/grib2/DirFreq.h"
#include "multio/mars2grib/grib2/Satellite.h"
#include "multio/mars2grib/grib2/Time.h"
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

//----------------------------------------------------------------------------------------------------------------------
// Matchers
//----------------------------------------------------------------------------------------------------------------------

auto matchChemical() {
    return all(Has{&dm::FullMarsRecord::chem}, Missing{&dm::FullMarsRecord::wavelength},
               lessThan(&dm::FullMarsRecord::chem, 900));
}

auto matchAerosol() {
    return all(Has{&dm::FullMarsRecord::chem}, Missing{&dm::FullMarsRecord::wavelength},
               greaterEqual(&dm::FullMarsRecord::chem, 900));
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
    return Has{&dm::FullMarsRecord::channel};
}

//----------------------------------------------------------------------------------------------------------------------
// Setters
//----------------------------------------------------------------------------------------------------------------------

// Category setters
auto pointInTime() {
    return Setter([](const dm::FullMarsRecord& mars, const dm::MiscRecord&, LegacySectionsConf& c, Grib2Layout& gl) {
        // Legacy
        c.product.ensureInit().modify().pdtCat.ensureInit().modify().timeExtent.set(TimeExtent::PointInTime);
        c.product.ensureInit().modify().pointInTime.ensureInit();

        // New structure
        gl.pdtCat.timeExtent.set(TimeExtent::PointInTime);
        gl.dateTime = grib2::setDateTime(mars);
        gl.initForecastTime = grib2::setInitForecastTime();
        gl.pointInTime = grib2::setPointInTime(mars);
    });
}
auto timeRange(TimeRangeType type, TOSP typeOfStatisticalProcessing) {
    return Setter(
        [=](const dm::FullMarsRecord& mars, const dm::MiscRecord& misc, LegacySectionsConf& c, Grib2Layout& gl) {
            // Legacy
            c.product.ensureInit().modify().pdtCat.ensureInit().modify().timeExtent.set(TimeExtent::TimeRange);
            c.product.ensureInit().modify().timeRange.ensureInit().modify().type.set(type);
            c.product.ensureInit().modify().timeRange.ensureInit().modify().typeOfStatisticalProcessing.set(
                typeOfStatisticalProcessing);

            // New structure
            gl.pdtCat.timeExtent.set(TimeExtent::TimeRange);
            gl.dateTime = grib2::setDateTime(mars);
            gl.initForecastTime = grib2::setInitForecastTime();
            gl.timeRange = grib2::setTimeRange(mars, misc);
        });
}
auto overallLengthOfTimeRange(const std::string& l) {
    return Setter([=](const dm::FullMarsRecord&, const dm::MiscRecord&, LegacySectionsConf& c, Grib2Layout& gl) {
        c.product.ensureInit().modify().timeRange.ensureInit().modify().overallLengthOfTimeRange.set(l);
    });
}

auto ensemble() {
    return Setter([](const dm::FullMarsRecord&, const dm::MiscRecord&, LegacySectionsConf& c, Grib2Layout& gl) {
        // Legacy
        c.product.ensureInit().modify().pdtCat.ensureInit().modify().processSubType.set(ProcessSubType::Ensemble);
        c.product.ensureInit().modify().process.ensureInit();

        // New structure
        gl.pdtCat.processSubType.set(ProcessSubType::Ensemble);
    });
}
auto largeEnsemble() {
    return Setter([](const dm::FullMarsRecord&, const dm::MiscRecord&, LegacySectionsConf& c, Grib2Layout& gl) {
        // Legacy
        c.product.ensureInit().modify().pdtCat.ensureInit().modify().processSubType.set(ProcessSubType::LargeEnsemble);
        c.product.ensureInit().modify().process.ensureInit();

        // New structure
        gl.pdtCat.processSubType.set(ProcessSubType::LargeEnsemble);
    });
}
auto reforecast() {
    return Setter([](const dm::FullMarsRecord& mars, const dm::MiscRecord&, LegacySectionsConf& c, Grib2Layout& gl) {
        // Legacy
        c.product.ensureInit().modify().pdtCat.ensureInit().modify().processType.set(ProcessType::Reforecast);
        c.product.ensureInit().modify().process.ensureInit();

        // New structure
        gl.pdtCat.processType.set(ProcessType::Reforecast);
        gl.refDateTime = grib2::setRefDateTime(mars);
    });
}

auto chemical() {
    return Setter([](const dm::FullMarsRecord&, const dm::MiscRecord&, LegacySectionsConf& c, Grib2Layout& gl) {
        // Legacy
        c.product.ensureInit().modify().chemical.ensureInit();
        c.product.ensureInit().modify().pdtCat.ensureInit().modify().productCategory.set(ProductCategory::Chemical);

        // New structure
        gl.pdtCat.productCategory.set(ProductCategory::Chemical);
    });
}

auto periodRange() {
    return Setter([](const dm::FullMarsRecord&, const dm::MiscRecord&, LegacySectionsConf& c, Grib2Layout& gl) {
        // Legacy
        c.product.ensureInit().modify().periodRange.ensureInit();
        c.product.ensureInit().modify().pdtCat.ensureInit().modify().productCategory.set(ProductCategory::Wave);
        c.product.ensureInit().modify().pdtCat.ensureInit().modify().productSubCategory.set(
            ProductSubCategory::PeriodRange);

        // New structure
        gl.pdtCat.productCategory.set(ProductCategory::Wave);
        gl.pdtCat.productSubCategory.set(ProductSubCategory::PeriodRange);
    });
}

auto dirFreq() {
    return Setter(
        [](const dm::FullMarsRecord& mars, const dm::MiscRecord& misc, LegacySectionsConf& c, Grib2Layout& gl) {
            // Legacy
            c.product.ensureInit().modify().dirFreq.ensureInit();
            c.product.ensureInit().modify().pdtCat.ensureInit().modify().productCategory.set(ProductCategory::Wave);
            c.product.ensureInit().modify().pdtCat.ensureInit().modify().productSubCategory.set(
                ProductSubCategory::SpectraList);


            // New structure
            gl.pdtCat.productCategory.set(ProductCategory::Wave);
            gl.pdtCat.productSubCategory.set(ProductSubCategory::SpectraList);

            gl.dirFreqArrays = grib2::setDirFreqArrays(misc);
            gl.dirFreqMars = grib2::setDirFreqMars(mars);
        });
}

auto satellite() {
    return Setter([](const dm::FullMarsRecord& mars, const dm::MiscRecord& misc, LegacySectionsConf& c,
                     Grib2Layout& gl) {
        // Legacy
        c.product.ensureInit().modify().satellite.ensureInit();
        c.product.ensureInit().modify().pdtCat.ensureInit().modify().productCategory.set(ProductCategory::Satellite);

        // New structure
        gl.pdtCat.productCategory.set(ProductCategory::Satellite);
        gl.satellite = grib2::setSatellite(mars, misc);
    });
}

auto randomPattern() {
    return Setter([](const dm::FullMarsRecord&, const dm::MiscRecord&, LegacySectionsConf& c, Grib2Layout& gl) {
        // Legacy
        c.product.ensureInit().modify().randomPatterns.ensureInit();
        c.product.ensureInit().modify().pdtCat.ensureInit().modify().spatialExtent.set(SpatialExtent::RandomPatterns);

        // New structure
        gl.pdtCat.spatialExtent.set(SpatialExtent::RandomPatterns);
    });
}


// Other setters

auto typeOfLevel(TOL type, std::optional<std::int64_t> fixedLevel = std::optional<std::int64_t>{}) {
    return Setter(
        [=](const dm::FullMarsRecord& mars, const dm::MiscRecord& misc, LegacySectionsConf& c, Grib2Layout& gl) {
            // Legacy
            c.product.ensureInit().modify().level.ensureInit().modify().type.set(type);

            // New structure
            gl.level = grib2::setLevel(type, fixedLevel, mars);
            gl.vertical = grib2::setVertical(type, mars, misc);
        });
}

auto localUse(std::int64_t num) {
    return Setter([=](const dm::FullMarsRecord&, const dm::MiscRecord&, LegacySectionsConf& c, Grib2Layout& gl) {
        // Legacy
        c.localUse.ensureInit().modify().templateNumber.set(num);

        // New structure
        gl.structure.localDefinitionNumber.set(num);
        if (num > 1000) {
            gl.structure.localDefinitionNumber.set(num - 1000);
            gl.structure.destineLocalVersion.set(1);
        }
    });
}

auto dataRepres(std::int64_t num) {
    return Setter([=](const dm::FullMarsRecord&, const dm::MiscRecord&, LegacySectionsConf& c, Grib2Layout& gl) {
        // Legacy
        c.dataRepres.ensureInit().modify().templateNumber.set(num);

        // New structure
        gl.structure.dataRepresentationTemplateNumber.set(num);
    });
}


// TODO(pgeier) can be removed after migration
auto tablesConfig(const std::string& type) {
    return Setter([=](const dm::FullMarsRecord&, const dm::MiscRecord&, LegacySectionsConf& c, Grib2Layout& gl) {
        c.identification.ensureInit().modify().tables.ensureInit().modify().type.set(type);
    });
}

auto tablesVersion(std::int64_t version) {
    return Setter([=](const dm::FullMarsRecord&, const dm::MiscRecord&, LegacySectionsConf& c, Grib2Layout& gl) {
        // Legacy
        c.identification.ensureInit().modify().tables.ensureInit().modify().tablesVersion.set(version);

        // New structure
        gl.structure.tablesVersion.set(version);
    });
}

auto localTablesVersion(std::int64_t version) {
    return Setter([=](const dm::FullMarsRecord&, const dm::MiscRecord&, LegacySectionsConf& c, Grib2Layout& gl) {
        // Legacy
        c.identification.ensureInit().modify().tables.ensureInit().modify().localTablesVersion.set(version);

        // New structure
        gl.structure.localTablesVersion.set(version);
    });
}


//----------------------------------------------------------------------------------------------------------------------
// Composed rules
//----------------------------------------------------------------------------------------------------------------------

// TODO(pgeier) - Grid mapping is completely orthognal and should not be separated in separate branches
auto gridRules() {
    return exclusiveRuleList(
        rule(Has{&dm::FullMarsRecord::grid},
             Setter([=](const dm::FullMarsRecord& mars, const dm::MiscRecord&, LegacySectionsConf& c, Grib2Layout& gl) {
                 // c.grid.ensureInit().modify().gridDefinitionTemplateNumber.set(dm::gridTypeFromGrid(mars.grid.get()));
                 gl.structure.gridType.set(dm::gridTypeFromGrid(mars.grid.get()));

                 switch (gl.structure.gridType.get()) {
                     case dm::GridType::RegularGG:
                     case dm::GridType::ReducedGG:
                         c.grid.ensureInit().modify().templateNumber.set(40);
                         break;
                     case dm::GridType::RegularLL:
                         c.grid.ensureInit().modify().templateNumber.set(0);
                         break;
                     default:
                         throw Mars2GribException(
                             std::string("Grid ") + mars.grid.get() + std::string(" is not supported yet."), Here());
                 }
             })),
        rule(Has{&dm::FullMarsRecord::truncation},
             Setter([=](const dm::FullMarsRecord& mars, const dm::MiscRecord&, LegacySectionsConf& c, Grib2Layout& gl) {
                 // c.grid.ensureInit().modify().gridDefinitionTemplateNumber.set(dm::GridType::SH);
                 c.grid.ensureInit().modify().templateNumber.set(50);
                 gl.structure.gridType.set(dm::GridType::SH);
             })));
}


auto localSectionRules() {
    return exclusiveRuleList(  //
        rule(all(Missing{&dm::FullMarsRecord::anoffset}, NoneOf{&dm::FullMarsRecord::klass, {"d1"}},
                 Missing{&dm::FullMarsRecord::method}),
             localUse(1)),
        rule(all(Missing{&dm::FullMarsRecord::anoffset}, NoneOf{&dm::FullMarsRecord::klass, {"d1"}},
                 Has{&dm::FullMarsRecord::method}),
             localUse(15)),
        rule(all(Has{&dm::FullMarsRecord::anoffset}, NoneOf{&dm::FullMarsRecord::klass, {"d1"}}), localUse(36)),
        rule(all(Missing{&dm::FullMarsRecord::anoffset}, OneOf{&dm::FullMarsRecord::klass, {"d1"}}), localUse(1001)),
        rule(all(Has{&dm::FullMarsRecord::anoffset}, OneOf{&dm::FullMarsRecord::klass, {"d1"}}), localUse(1036)));
}

auto processTypesRules() {
    return exclusiveRuleList(
        // Match any levtype but AL (or no levtype)
        rule(all(NoneOf{&dm::FullMarsRecord::levtype, {dm::LevType::AL}}, Missing{&dm::FullMarsRecord::number},
                 Missing{&dm::FullMarsRecord::hdate})),
        rule(all(NoneOf{&dm::FullMarsRecord::levtype, {dm::LevType::AL}}, Has{&dm::FullMarsRecord::number},
                 Missing{&dm::FullMarsRecord::hdate}),
             ensemble()),
        rule(all(NoneOf{&dm::FullMarsRecord::levtype, {dm::LevType::AL}}, Has{&dm::FullMarsRecord::number},
                 Has{&dm::FullMarsRecord::hdate}),
             reforecast(), ensemble()),
        // Levtype AL specific - detection whether a largeEnsemble is used should actually depend on
        // numberOfForecastsInEnsemble > 254
        rule(all(matchLevType(dm::LevType::AL), Has{&dm::FullMarsRecord::number}, Missing{&dm::FullMarsRecord::hdate}),
             largeEnsemble()),
        rule(all(matchLevType(dm::LevType::AL), Has{&dm::FullMarsRecord::number}, Has{&dm::FullMarsRecord::hdate}),
             reforecast(), largeEnsemble()));
}


auto packingRules() {
    return exclusiveRuleList(                                                  //
        rule(OneOf{&dm::FullMarsRecord::packing, {"simple"}}, dataRepres(0)),  //
        rule(OneOf{&dm::FullMarsRecord::packing, {"ccsds"}}, dataRepres(42)),  //
        rule(OneOf{&dm::FullMarsRecord::packing, {"complex"}}, dataRepres(51)));
}


//----------------------------------------------------------------------------------------------------------------------
// Params
//----------------------------------------------------------------------------------------------------------------------

//----------------------------------------------------------------------------------------------------------------------
// Satellite
//----------------------------------------------------------------------------------------------------------------------

// Single satellite rule - defined here to be checked twice - with levtype sfc
// and without any levtype but ident, instrument and channel
auto singleSatelliteRule() {
    return rule(matchParams(paramRange(260510, 260512)),  //
                pointInTime(),                            //
                satellite());
}

auto paramSatelliteRules() {
    return exclusiveRuleList(singleSatelliteRule());
}

//----------------------------------------------------------------------------------------------------------------------
// SFC
//----------------------------------------------------------------------------------------------------------------------

auto paramSFCRules() {
    return exclusiveRuleList(                              //
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
             overallLengthOfTimeRange("6h"),               //
             typeOfLevel(TOL::HeightAboveGroundAt2m, 2)),  //
        rule(matchParams(122),                             //
             timeRange(TimeRangeType::FixedTimeRange, TOSP::Minimum),
             overallLengthOfTimeRange("6h"),                                                  //
             typeOfLevel(TOL::HeightAboveGroundAt2m, 2)),                                     //
        rule(matchParams(201, 237167, 237168),                                                //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Maximum),            //
             typeOfLevel(TOL::HeightAboveGroundAt2m, 2)),                                     //
        rule(matchParams(202, 238167, 238168),                                                //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Minimum),            //
             typeOfLevel(TOL::HeightAboveGroundAt2m, 2)),                                     //
        rule(matchParams(228004, 235168),                                                     //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Average),            //
             typeOfLevel(TOL::HeightAboveGroundAt2m, 2)),                                     //
        rule(matchParams(239167, 239168),                                                     //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::StandardDeviation),  //
             typeOfLevel(TOL::HeightAboveGroundAt2m, 2)),                                     //
        rule(matchParams(123),                                                                //
             timeRange(TimeRangeType::FixedTimeRange, TOSP::Maximum),                         //
             overallLengthOfTimeRange("6h"),                                                  //
             typeOfLevel(TOL::HeightAboveGroundAt10m, 10)),                                   //
        rule(matchParams(228028),                                                             //
             timeRange(TimeRangeType::FixedTimeRange, TOSP::Maximum),
             overallLengthOfTimeRange("3h"),                                        //
             typeOfLevel(TOL::HeightAboveGroundAt10m, 10)),                         //
        rule(matchParams(49, 237165, 237166, 237207, 237318),                       //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Maximum),  //
             typeOfLevel(TOL::HeightAboveGroundAt10m, 10)),                         //
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
             typeOfLevel(TOL::HeightAboveGroundAt10m, 10)),                                                   //
        rule(matchParams(238165, 238166, 238207),                                                             //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Minimum),                            //
             typeOfLevel(TOL::HeightAboveGroundAt10m, 10)),                                                   //
        rule(matchParams(239165, 239166, 239207),                                                             //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::StandardDeviation),                  //
             typeOfLevel(TOL::HeightAboveGroundAt10m, 10)),                                                   //
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
        rule(matchParams(235020, 235021, paramRange(235029, 235031), paramRange(235033, 235038),              //
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
        rule(matchParams(235090), timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Average),
             typeOfLevel(TOL::MixingLayer)),
        rule(matchParams(237090), timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Maximum),
             typeOfLevel(TOL::MixingLayer)),
        rule(matchParams(238090), timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Minimum),
             typeOfLevel(TOL::MixingLayer)),
        rule(matchParams(239090), timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::StandardDeviation),
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
             typeOfLevel(TOL::HeightAboveGroundAt10m, 10)),                                   //
        rule(matchParams(167, 168, 174096, 228037, 260242),                                   //
             pointInTime(),                                                                   //
             typeOfLevel(TOL::HeightAboveGroundAt2m, 2)),                                     //
        rule(matchParams(140245, 140249, 140233),                                             //
             pointInTime(),                                                                   //
             typeOfLevel(TOL::HeightAboveSeaAt10m, 10)),                                      //
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
        rule(matchParams(                                                                //
                 8, 9, 20, 44, 45, 47, 50, 57, 58, paramRange(142, 147), 169, 175, 176, 177, 180, 181, 182, 189, 195,
                 196, 197, 205, 210, 211, 213, 228, 239, 240, 3062, 3099, paramRange(162100, 162113),
                 paramRange(222001, 222256), 228021, 228022, 228129, 228130, 228143, 228144, 228216, 228228, 228251,
                 231001, 231002, 231003, 231005, 231010, 231012, 231057, 231058, paramRange(233000, 233031),
                 260259),  //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Accumulation),
             typeOfLevel(TOL::Surface)),   //
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
        rule(matchParams(228226, 237013, 237041, 237042, 237055, 237078, 237080, 237083, 237084, 237093, 237117, 237134,
                         237159, 237263, 237321),                                   //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Maximum),  //
             typeOfLevel(TOL::Surface)),                                            //
        rule(matchParams(228227, 238055, 238013, 238041, 238042, 238078, 238080, 238083, 238084, 238093, 238134, 238159,
                         238263),                                                                          //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Minimum),                         //
             typeOfLevel(TOL::Surface)),                                                                   //
        rule(matchParams(239041, 239042, 239078, 239080, 239083, 239084, 239093, 239134, 239159, 239263),  //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::StandardDeviation),               //
             typeOfLevel(TOL::Surface)),                                                                   //
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


//----------------------------------------------------------------------------------------------------------------------
// ML
//----------------------------------------------------------------------------------------------------------------------

// NOTE - levtype ML is always mapped to TOL::Hybrid - these rules can be generalized once the time mapping is mapped
// orthogonally
auto paramMLRules() {
    return exclusiveRuleList(  //
        rule(matchParams(21, 22, 23, 75, 76, 77, paramRange(129, 133), 135, 138, 152, 155, 156, 157, 203, 246, 247, 248,
                         260290),                                                        //
             pointInTime(),                                                              //
             typeOfLevel(TOL::Hybrid)),                                                  //
        rule(matchParams(paramRange(162100, 162113)),                                    //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Accumulation),  //
             typeOfLevel(TOL::Hybrid))                                                   //
    );
}

//----------------------------------------------------------------------------------------------------------------------
// PL
//----------------------------------------------------------------------------------------------------------------------

// Special composer to handle pressure units differently
// The exclusion list should be the outermost - hence we are passing in the specializations
template <typename MkTail>
auto plLevelRules(MkTail&& mkTail) {
    return exclusiveRuleList(                                       //
        chainedRuleList(                                            //
            rule(greaterEqual(&dm::FullMarsRecord::levelist, 100),  //
                 typeOfLevel(TOL::IsobaricInhPa)),
            mkTail()),                                          //
        chainedRuleList(                                        //
            rule(lessThan(&dm::FullMarsRecord::levelist, 100),  //
                 typeOfLevel(TOL::IsobaricInPa)),
            mkTail())  //
    );
}

auto paramPLRules() {
    return plLevelRules([]() {
        return exclusiveRuleList(  //
            rule(matchParams(1, 2, 60, 75, 76, paramRange(129, 135), 138, 152, 155, 157, 203, 246, 247, 248, 157,
                             260290),
                 pointInTime()),
            rule(matchParams(235100, paramRange(235129, 235133), 235135, 235138, 235152, 235155, 235157, 235203, 235246,
                             263107),
                 timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Average)));
    });
}


//----------------------------------------------------------------------------------------------------------------------
// PT
//----------------------------------------------------------------------------------------------------------------------

auto paramPTRules() {
    return exclusiveRuleList(                                                       //
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


//----------------------------------------------------------------------------------------------------------------------
// PV
//----------------------------------------------------------------------------------------------------------------------

auto paramPVRules() {
    return exclusiveRuleList(                                                       //
        rule(matchParams(3, 54, 129, 131, 132, 133, 203),                           //
             pointInTime(),                                                         //
             typeOfLevel(TOL::PotentialVorticity)),                                 //
        rule(matchParams(235098, 235269),                                           //
             timeRange(TimeRangeType::SinceLastPostProcessingStep, TOSP::Average),  //
             typeOfLevel(TOL::PotentialVorticity))                                  //
    );
}


//----------------------------------------------------------------------------------------------------------------------
// Soil
//----------------------------------------------------------------------------------------------------------------------

auto paramSOLRules() {
    return exclusiveRuleList(                                                       //
        rule(matchParams(262000, 262024),                                           //
             pointInTime(),                                                         //
             typeOfLevel(TOL::SeaIceLayer)),                                        //
        rule(matchParams(33, 74, 238, 228038, 228141),                              //
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


//----------------------------------------------------------------------------------------------------------------------
// Al
//----------------------------------------------------------------------------------------------------------------------

auto paramAlRules() {
    return exclusiveRuleList(                          //
        rule(matchParams(paramRange(213101, 213160)),  //
             pointInTime(),                            //
             randomPattern(),                          //
             typeOfLevel(TOL::AbstractSingleLevel))    //
    );
}


//----------------------------------------------------------------------------------------------------------------------
// Final composed param rules
//----------------------------------------------------------------------------------------------------------------------

// TODO can be optimized by pulling the levtype check one level out -
// exclusiveRuleList(
//    chainedRuleList(rule(matchLevType(XXX)), paramXXXRules()),
//    ....
// )

auto paramRules() {
    return exclusiveRuleList(                                                    //
        chainedRuleList(rule(matchLevType(dm::LevType::SFC)), paramSFCRules()),  //
        chainedRuleList(rule(matchLevType(dm::LevType::HL)), paramHLRules()),    //
        chainedRuleList(rule(matchLevType(dm::LevType::ML)), paramMLRules()),    //
        chainedRuleList(rule(matchLevType(dm::LevType::PL)), paramPLRules()),    //
        chainedRuleList(rule(matchLevType(dm::LevType::PT)), paramPTRules()),    //
        chainedRuleList(rule(matchLevType(dm::LevType::PV)), paramPVRules()),    //
        chainedRuleList(rule(matchLevType(dm::LevType::SOL)), paramSOLRules()),  //
        chainedRuleList(rule(matchLevType(dm::LevType::AL)), paramAlRules()),    //
        chainedRuleList(rule(matchSatellite()), paramSatelliteRules())           //
    );
}


//----------------------------------------------------------------------------------------------------------------------
// Big rule tree...
//----------------------------------------------------------------------------------------------------------------------

const ChainedRuleList& allRules() {
    static auto all_ = chainedRuleList(  //
        gridRules(),                     //
        localSectionRules(),             //
        processTypesRules(),             //
        paramRules(),                    //
        packingRules()                   //
    );

    return all_;
}

std::tuple<LegacySectionsConf, Grib2Layout> buildEncoderConf(const dm::FullMarsRecord& mars,
                                                             const dm::MiscRecord& misc) {
    LegacySectionsConf sections;
    Grib2Layout layout;
    if (!allRules()(mars, misc, sections, layout)) {
        std::ostringstream oss;
        oss << "Cannot map mars keys. None of the outermost rules apply: ";
        util::print(oss, allRules());
        throw Mars2GribException(oss.str(), Here());
    }
    dm::applyRecordDefaults(sections);
    dm::validateRecord(sections);

    // TODO(pgeier) will change in future
    dm::applyRecordDefaults(layout.pdtCat);
    dm::validateRecord(layout.pdtCat);
    return std::make_tuple(std::move(sections), std::move(layout));
}

}  // namespace multio::mars2grib::rules

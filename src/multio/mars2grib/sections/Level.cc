#include "multio/mars2grib/sections/Level.h"
#include "multio/datamod/ContainerInterop.h"
#include "multio/datamod/GribKeys.h"
#include "multio/datamod/types/LevType.h"
#include "multio/datamod/core/Compare.h"
#include "multio/mars2grib/Mars2GribException.h"
#include "multio/mars2grib/sections/SectionSetter.h"

#include "multio/util/Print.h"

namespace multio::mars2grib::sections {

std::optional<LEVELIST_t> levelForTypeOfLevel(const LevelConfigurator& levelConf, const dm::MarsRecord& mars,
                                              const dm::MiscRecord& misc) {
    auto throwLevelistMissing = [&]() {
        std::ostringstream oss;
        oss << "Missing levelist to extract level for typeOfLevel ";
        util::print(oss, levelConf.type);
        oss << ". Passed MARS keys: ";
        util::print(oss, mars);
        throw Mars2GribException(oss.str(), Here());
    };
    switch (levelConf.type) {
        // The 2m and 10m entries can be removed once the fortran encoder is removed
        case dm::TypeOfLevel::HeightAboveSea:
        case dm::TypeOfLevel::HeightAboveSeaAt10m:
        case dm::TypeOfLevel::HeightAboveGroundAt2m:
        case dm::TypeOfLevel::HeightAboveGroundAt10m:
        case dm::TypeOfLevel::HeightAboveGround: {
            const bool isSFC = mars.levtype.has() && mars.levtype.get() == dm::LevType::SFC;

            if (levelConf.fixedLevel.has()) {
                return levelConf.fixedLevel.get();
            }
            if (isSFC) {
                if (!mars.levelist.has()) {
                    throwLevelistMissing();
                }
                return mars.levelist.get();
            }
            return {};
        }
        case dm::TypeOfLevel::Snow:
        case dm::TypeOfLevel::SnowLayer:
        case dm::TypeOfLevel::SoilLayer:
        case dm::TypeOfLevel::SeaIceLayer:
        case dm::TypeOfLevel::IsobaricInPa:
        case dm::TypeOfLevel::PotentialVorticity:
        case dm::TypeOfLevel::Theta:
        case dm::TypeOfLevel::Hybrid: {
            if (!mars.levelist.has()) {
                throwLevelistMissing();
            }
            return mars.levelist.get();
        }
        case dm::TypeOfLevel::IsobaricInhPa: {
            if (!mars.levelist.has()) {
                throwLevelistMissing();
            }
            return mars.levelist.get() / 100;
        }
        default:
            return {};
    }
}


dm::HorizontalGribKeys horizontalForTypeOfLevel(const LevelConfigurator& levelConf, const dm::MarsRecord& mars,
                                                const dm::MiscRecord& misc) {
    auto throwLevelistMissing = [&]() {
        std::ostringstream oss;
        oss << "Missing levelist to extract horizontal keys for typeOfLevel ";
        util::print(oss, levelConf.type);
        oss << ". Passed MARS keys: ";
        util::print(oss, mars);
        throw Mars2GribException(oss.str(), Here());
    };

    auto handleHeightAbove = [&](std::int64_t typeOfFirstFixedSurface) {
        dm::HorizontalGribKeys ret;

        const bool isSFC = mars.levtype.has() && mars.levtype.get() == dm::LevType::SFC;
        ret.typeOfFirstFixedSurface.set(typeOfFirstFixedSurface);
        ret.typeOfSecondFixedSurface.set(255);

        if (levelConf.fixedLevel.has()) {
            ret.scaledValueOfFirstFixedSurface.set(levelConf.fixedLevel.get());
            ret.scaleFactorOfFirstFixedSurface.set(0);
        }
        if (isSFC) {
            if (mars.levelist.isUnset()) {
                throwLevelistMissing();
            }
            ret.scaledValueOfFirstFixedSurface.set(mars.levelist.get());
            ret.scaleFactorOfFirstFixedSurface.set(0);
        }
        dm::applyRecordDefaults(ret);
        dm::validateRecord(ret);
        return ret;
    };


    auto handle2SurfacesWithoutLevel
        = [&](std::int64_t typeOfFirstFixedSurface, std::int64_t typeOfSecondFixedSurface) {
              dm::HorizontalGribKeys ret;
              ret.typeOfFirstFixedSurface.set(typeOfFirstFixedSurface);
              ret.typeOfSecondFixedSurface.set(typeOfSecondFixedSurface);
              dm::applyRecordDefaults(ret);
              dm::validateRecord(ret);
              return ret;
          };

    auto handle1SurfaceWithoutLevel = [&](std::int64_t typeOfFirstFixedSurface) {
        dm::HorizontalGribKeys ret;
        ret.typeOfFirstFixedSurface.set(typeOfFirstFixedSurface);
        ret.typeOfSecondFixedSurface.set(255);
        dm::applyRecordDefaults(ret);
        dm::validateRecord(ret);
        return ret;
    };

    auto handle1SurfaceWithLevel
        = [&](std::int64_t typeOfFirstFixedSurface, std::optional<std::string> pressureUnits = {}) {
              dm::HorizontalGribKeys ret;

              if (pressureUnits) {
                  ret.pressureUnits.set(std::move(*pressureUnits));
              }

              ret.typeOfFirstFixedSurface.set(typeOfFirstFixedSurface);
              ret.typeOfSecondFixedSurface.set(255);

              if (levelConf.fixedLevel.has()) {
                  ret.scaledValueOfFirstFixedSurface.set(levelConf.fixedLevel.get());
              }
              else {
                  if (mars.levelist.isUnset()) {
                      throwLevelistMissing();
                  }
                  ret.scaledValueOfFirstFixedSurface.set(mars.levelist.get());
              }
              ret.scaleFactorOfFirstFixedSurface.set(0);
              dm::applyRecordDefaults(ret);
              dm::validateRecord(ret);
              return ret;
          };

    auto handle2SurfaceLayerWithlevel = [&](std::int64_t typeOfSurface) {
        dm::HorizontalGribKeys ret;
        if (mars.levelist.isUnset()) {
            throwLevelistMissing();
        }
        ret.typeOfFirstFixedSurface.set(typeOfSurface);
        ret.typeOfSecondFixedSurface.set(typeOfSurface);
        ret.scaledValueOfFirstFixedSurface.set(mars.levelist.get() - 1);
        ret.scaleFactorOfFirstFixedSurface.set(0);
        ret.scaledValueOfSecondFixedSurface.set(mars.levelist.get());
        ret.scaleFactorOfSecondFixedSurface.set(0);
        dm::applyRecordDefaults(ret);
        dm::validateRecord(ret);
        return ret;
    };

    switch (levelConf.type) {
        // The 2m and 10m entries can be removed once the fortran encoder is removed
        case dm::TypeOfLevel::HeightAboveSea:
        case dm::TypeOfLevel::HeightAboveSeaAt10m:
            return handleHeightAbove(102);
        case dm::TypeOfLevel::HeightAboveGroundAt2m:
        case dm::TypeOfLevel::HeightAboveGroundAt10m:
        case dm::TypeOfLevel::HeightAboveGround:
            return handleHeightAbove(103);
        case dm::TypeOfLevel::EntireLake:
            return handle2SurfacesWithoutLevel(1, 162);
        case dm::TypeOfLevel::EntireAtmosphere:
            return handle2SurfacesWithoutLevel(1, 8);
        case dm::TypeOfLevel::DepthBelowSeaLayer:
            return handle2SurfacesWithoutLevel(160, 160);
        case dm::TypeOfLevel::IceLayerOnWater:
            return handle2SurfacesWithoutLevel(174, 176);
        case dm::TypeOfLevel::Surface:
            return handle1SurfaceWithoutLevel(1);
        case dm::TypeOfLevel::IceTopOnWater:
            return handle1SurfaceWithoutLevel(174);
        case dm::TypeOfLevel::CloudBase:
            return handle1SurfaceWithoutLevel(2);
        case dm::TypeOfLevel::AbstractMultipleLevels:
        case dm::TypeOfLevel::AbstractSingleLevel:
            return handle1SurfaceWithoutLevel(191);
        case dm::TypeOfLevel::Isothermal:
            return handle1SurfaceWithoutLevel(20);
        case dm::TypeOfLevel::LakeBottom:
            return handle1SurfaceWithoutLevel(162);
        case dm::TypeOfLevel::MeanSea:
            return handle1SurfaceWithoutLevel(101);
        case dm::TypeOfLevel::MixedLayerParcel:
            return handle1SurfaceWithoutLevel(18);
        case dm::TypeOfLevel::MostUnstableParcel:
            return handle1SurfaceWithoutLevel(17);
        case dm::TypeOfLevel::MixingLayer:
            return handle1SurfaceWithoutLevel(166);
        case dm::TypeOfLevel::NominalTop:
            return handle1SurfaceWithoutLevel(8);
        case dm::TypeOfLevel::Tropopause:
            return handle1SurfaceWithoutLevel(7);
        case dm::TypeOfLevel::Hybrid:
            return handle1SurfaceWithLevel(105, "hPa");
        case dm::TypeOfLevel::PotentialVorticity:
            return handle1SurfaceWithLevel(109);
        case dm::TypeOfLevel::Theta:
            return handle1SurfaceWithLevel(107);
        case dm::TypeOfLevel::Snow:
            return handle1SurfaceWithLevel(114);  // Might be wrong... to be checked how level are set
        case dm::TypeOfLevel::IsobaricInPa: {
            return handle1SurfaceWithLevel(100, "pa");
        }
        case dm::TypeOfLevel::IsobaricInhPa: {
            return handle1SurfaceWithLevel(100, "hPa");
        }
        case dm::TypeOfLevel::HighCloudLayer: {
            dm::HorizontalGribKeys ret;
            ret.typeOfFirstFixedSurface.set(100);
            ret.typeOfSecondFixedSurface.set(8);
            ret.scaledValueOfFirstFixedSurface.set(45000);
            ret.scaleFactorOfFirstFixedSurface.set(0);
            dm::applyRecordDefaults(ret);
            dm::validateRecord(ret);
            return ret;
        }
        case dm::TypeOfLevel::MediumCloudLayer: {
            dm::HorizontalGribKeys ret;
            ret.typeOfFirstFixedSurface.set(100);
            ret.typeOfSecondFixedSurface.set(100);
            ret.scaledValueOfFirstFixedSurface.set(80000);
            ret.scaleFactorOfFirstFixedSurface.set(0);
            ret.scaledValueOfSecondFixedSurface.set(45000);
            ret.scaleFactorOfSecondFixedSurface.set(0);
            dm::applyRecordDefaults(ret);
            dm::validateRecord(ret);
            return ret;
        }
        case dm::TypeOfLevel::LowCloudLayer: {
            dm::HorizontalGribKeys ret;
            ret.typeOfFirstFixedSurface.set(1);
            ret.typeOfSecondFixedSurface.set(100);
            ret.scaledValueOfSecondFixedSurface.set(80000);
            ret.scaleFactorOfSecondFixedSurface.set(0);
            dm::applyRecordDefaults(ret);
            dm::validateRecord(ret);
            return ret;
        }
        case dm::TypeOfLevel::SeaIceLayer: {
            return handle2SurfaceLayerWithlevel(152);
        }
        case dm::TypeOfLevel::SnowLayer: {
            return handle2SurfaceLayerWithlevel(114);
        }
        case dm::TypeOfLevel::SoilLayer: {
            return handle2SurfaceLayerWithlevel(151);
        }
        default:
            return {};
    }
}

std::optional<dm::VerticalGribKeys> verticalForTypeOfLevel(const LevelConfigurator& levelConf,
                                                           const dm::MarsRecord& mars, const dm::MiscRecord& misc) {
    switch (levelConf.type) {
        case dm::TypeOfLevel::Hybrid:
        case dm::TypeOfLevel::Snow: {
            if (mars.levtype.has() && mars.levtype.get() == dm::LevType::ML) {
                dm::VerticalGribKeys ret;
                ASSERT(misc.pv.holdsReference());

                if (misc.pv.isUnset()) {
                    std::ostringstream oss;
                    oss << "Missing key " << dm::Pv.keyInfo() << " to set vertical information for typeOfLevel ";
                    util::print(oss, levelConf.type);
                    oss << ". Mars keys: ";
                    util::print(oss, mars);
                    throw Mars2GribException(oss.str(), Here());
                };

                ret.pv.setRef(misc.pv.get());
                ASSERT(ret.pv.holdsReference());
                dm::applyRecordDefaults(ret);
                dm::validateRecord(ret);
                return ret;
            }
            return {};
        }
        default:
            return {};
    }
};

DynSectionSetter::Config LevelSetter::sectionInfo() const {
    DynSectionSetter::Config ret;
    ret.registerPrepare = false;
    ret.registerAllocate = true;
    ret.registerPreset = true;
    ret.registerRuntime = true;
    ret.registerCheck = true;
    return ret;
};

void LevelSetter::allocate(util::MioGribHandle& h, const dm::MarsRecord& mars, const dm::MiscRecord& misc,
                           const dm::Geometry& geo) const {
    // set type of level here....
    auto vert = verticalForTypeOfLevel(conf_, mars, misc);
    if (vert) {
        dm::dumpRecord(*vert, h);
    }
}

void LevelSetter::preset(util::MioGribHandle& h, const dm::MarsRecord& mars, const dm::MiscRecord& misc,
                         const dm::Geometry& geo) const {
    setLevels(h, mars, misc, geo);
}

void LevelSetter::runtime(util::MioGribHandle& h, const dm::MarsRecord& mars, const dm::MiscRecord& misc,
                          const dm::Geometry& geo) const {
    // TODO this should be only relevant for ML fields (because we don't cache them on level...)
    setLevels(h, mars, misc, geo);
}

void LevelSetter::setLevels(util::MioGribHandle& h, const dm::MarsRecord& mars, const dm::MiscRecord& misc,
                            const dm::Geometry& geo) const {
    auto optLevel = levelForTypeOfLevel(conf_, mars, misc);
    // TODO make the LevelDef own the typeOfLevel after migration
    dm::dumpEntry(dm::TypeOfLevelEntry, conf_.type, h);
    if (optLevel) {
        h.setValue("level", *optLevel);
    }
}

void LevelSetter::check(const util::MioGribHandle& h, const dm::MarsRecord& mars, const dm::MiscRecord& misc,
                        const dm::Geometry& geo) const {
    auto inferedHoriz = horizontalForTypeOfLevel(conf_, mars, misc);
    auto horiz = dm::readRecord<dm::HorizontalGribKeys>(h);
    if (inferedHoriz != horiz) {
        std::ostringstream oss;
        oss << "LevelSetter{";
        util::print(oss, conf_);
        oss << "}::check- inferred and read horizontal keys are different: " << std::endl;
        oss << "Expected: ";
        util::print(oss, inferedHoriz);
        oss << std::endl;
        oss << "Read: ";
        util::print(oss, horiz);
        oss << std::endl;
        oss << "Mars keys: ";
        util::print(oss, mars);
        oss << std::endl;
        throw Mars2GribException(oss.str(), Here());
    }
}

// void LevelSetter::collectKeyInfo(KeyInfoList& req, KeyInfoList& opt, const dm::MarsRecord& mars) const {
//     addKeyInfo<MarsKeys::LEVTYPE>(req);

//     // Check if levelist is required
//     switch (levelConf.type) {
//         // The 2m and 10m entries can be removed once the fortran encoder is removed
//         case dm::TypeOfLevel::HeightAboveSea:
//         case dm::TypeOfLevel::HeightAboveSeaAt10m:
//         case dm::TypeOfLevel::HeightAboveGroundAt2m:
//         case dm::TypeOfLevel::HeightAboveGroundAt10m:
//         case dm::TypeOfLevel::HeightAboveGround: {
//             const bool isSFC = mars.levtype.has() && mars.levtype.get() == LevType::SFC;

//             if (levelConf.fixedLevel.isMissing() && isSFC) {
//                 addKeyInfo<MarsKeys::LEVELIST>(req);
//             }
//             break;
//         }
//         case dm::TypeOfLevel::Snow:
//         case dm::TypeOfLevel::SnowLayer:
//         case dm::TypeOfLevel::SoilLayer:
//         case dm::TypeOfLevel::SeaIceLayer:
//         case dm::TypeOfLevel::IsobaricInPa:
//         case dm::TypeOfLevel::PotentialVorticity:
//         case dm::TypeOfLevel::Theta:
//         case dm::TypeOfLevel::Hybrid:
//         case dm::TypeOfLevel::IsobaricInhPa: {
//             addKeyInfo<MarsKeys::LEVELIST>(req);
//             break;
//         }
//         default:
//             break;
//     }

//     // Check if vertical information is required
//     switch (levelConf.type) {
//         case dm::TypeOfLevel::Hybrid:
//         case dm::TypeOfLevel::Snow: {
//             if (mars.levtype.has() && mars.levtype.get() == LevType::ML) {
//                 addKeyInfo<MiscKeys::Pv>(req);
//             }
//             break;
//         }
//         default:
//             break;
//     }
// }

};  // namespace multio::mars2grib::sections

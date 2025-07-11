#include "multio/action/encode-mtg2/sections/Level.h"
#include "multio/action/encode-mtg2/EncodeMtg2Exception.h"
#include "multio/action/encode-mtg2/sections/SectionSetter.h"
#include "multio/datamod/ContainerInterop.h"
#include "multio/datamod/DataModelling.h"
#include "multio/datamod/GribKeys.h"
#include "multio/datamod/MarsTypes.h"

namespace multio::action::sections {

std::optional<datamod::KeyDefValueType_t<datamod::MarsKeys::LEVELIST>> levelForTypeOfLevel(
    const LevelKeyValueSet& levelConf, const datamod::MarsKeyValueSet& mars, const datamod::MiscKeyValueSet& misc) {
    using namespace datamod;
    auto tol = key<LevelDef::Type>(levelConf).get();
    const auto& fixedLevel = key<LevelDef::FixedLevel>(levelConf);
    const auto& levelist = key<MarsKeys::LEVELIST>(mars);

    auto throwLevelistMissing = [&]() {
        std::ostringstream oss;
        oss << "Missing levelist to extract level for typeOfLevel " << tol << ". Passed MARS keys: " << mars;
        throw EncodeMtg2Exception(oss.str(), Here());
    };
    switch (tol) {
        // The 2m and 10m entries can be removed once the fortran encoder is removed
        case TypeOfLevel::HeightAboveSea:
        case TypeOfLevel::HeightAboveSeaAt10m:
        case TypeOfLevel::HeightAboveGroundAt2m:
        case TypeOfLevel::HeightAboveGroundAt10m:
        case TypeOfLevel::HeightAboveGround: {
            const auto& levtype = key<MarsKeys::LEVTYPE>(mars);
            const bool isSFC = levtype.has() && levtype.get() == LevType::SFC;

            if (fixedLevel.has()) {
                return fixedLevel.get();
            }
            if (isSFC) {
                if (!levelist.has()) {
                    throwLevelistMissing();
                }
                return levelist.get();
            }
            return {};
        }
        case TypeOfLevel::Snow:
        case TypeOfLevel::SnowLayer:
        case TypeOfLevel::SoilLayer:
        case TypeOfLevel::SeaIceLayer:
        case TypeOfLevel::IsobaricInPa:
        case TypeOfLevel::PotentialVorticity:
        case TypeOfLevel::Theta:
        case TypeOfLevel::Hybrid: {
            if (!levelist.has()) {
                throwLevelistMissing();
            }
            return levelist.get();
        }
        case TypeOfLevel::IsobaricInhPa: {
            if (!levelist.has()) {
                throwLevelistMissing();
            }
            return levelist.get() / 100;
        }
        default:
            return {};
    }
}


datamod::HorizontalKeyValueSet horizontalForTypeOfLevel(const LevelKeyValueSet& levelConf,
                                                        const datamod::MarsKeyValueSet& mars,
                                                        const datamod::MiscKeyValueSet& misc) {
    using namespace datamod;
    auto tol = key<LevelDef::Type>(levelConf).get();
    const auto& fixedLevel = key<LevelDef::FixedLevel>(levelConf);
    const auto& levelist = key<MarsKeys::LEVELIST>(mars);

    auto throwLevelistMissing = [&]() {
        std::ostringstream oss;
        oss << "Missing levelist to extract horizontal keys for typeOfLevel " << tol << ". Passed MARS keys: " << mars;
        throw EncodeMtg2Exception(oss.str(), Here());
    };

    auto handleHeightAbove = [&](std::int64_t typeOfFirstFixedSurface) {
        HorizontalKeyValueSet ret;

        const auto& levtype = key<MarsKeys::LEVTYPE>(mars);
        const bool isSFC = levtype.has() && levtype.get() == LevType::SFC;
        ret.set<HorizontalKeys::TypeOfFirstFixedSurface>(typeOfFirstFixedSurface);
        ret.set<HorizontalKeys::TypeOfSecondFixedSurface>(255);

        if (fixedLevel.has()) {
            ret.set<HorizontalKeys::ScaledValueOfFirstFixedSurface>(fixedLevel.get());
            ret.set<HorizontalKeys::ScaleFactorOfFirstFixedSurface>(0);
        }
        if (isSFC) {
            if (levelist.isMissing()) {
                throwLevelistMissing();
            }
            ret.set<HorizontalKeys::ScaledValueOfFirstFixedSurface>(levelist.get());
            ret.set<HorizontalKeys::ScaleFactorOfFirstFixedSurface>(0);
        }
        alterAndValidate(ret);
        return ret;
    };


    auto handle2SurfacesWithoutLevel
        = [&](std::int64_t typeOfFirstFixedSurface, std::int64_t typeOfSecondFixedSurface) {
              HorizontalKeyValueSet ret;
              ret.set<HorizontalKeys::TypeOfFirstFixedSurface>(typeOfFirstFixedSurface);
              ret.set<HorizontalKeys::TypeOfSecondFixedSurface>(typeOfSecondFixedSurface);
              alterAndValidate(ret);
              return ret;
          };

    auto handle1SurfaceWithoutLevel = [&](std::int64_t typeOfFirstFixedSurface) {
        HorizontalKeyValueSet ret;
        ret.set<HorizontalKeys::TypeOfFirstFixedSurface>(typeOfFirstFixedSurface);
        ret.set<HorizontalKeys::TypeOfSecondFixedSurface>(255);
        alterAndValidate(ret);
        return ret;
    };

    auto handle1SurfaceWithLevel = [&](std::int64_t typeOfFirstFixedSurface, std::optional<std::string> pressureUnits = {}) {
        HorizontalKeyValueSet ret;

        if (pressureUnits) {
            ret.set<HorizontalKeys::PressureUnits>(std::move(*pressureUnits));
        }

        const auto& levtype = key<MarsKeys::LEVTYPE>(mars);
        ret.set<HorizontalKeys::TypeOfFirstFixedSurface>(typeOfFirstFixedSurface);
        ret.set<HorizontalKeys::TypeOfSecondFixedSurface>(255);

        if (fixedLevel.has()) {
            ret.set<HorizontalKeys::ScaledValueOfFirstFixedSurface>(fixedLevel.get());
        }
        else {
            if (levelist.isMissing()) {
                throwLevelistMissing();
            }
            ret.set<HorizontalKeys::ScaledValueOfFirstFixedSurface>(levelist.get());
        }
        ret.set<HorizontalKeys::ScaleFactorOfFirstFixedSurface>(0);
        alterAndValidate(ret);
        return ret;
    };

    auto handle2SurfaceLayerWithlevel = [&](std::int64_t typeOfSurface) {
        HorizontalKeyValueSet ret;
        if (levelist.isMissing()) {
            throwLevelistMissing();
        }
        ret.set<HorizontalKeys::TypeOfFirstFixedSurface>(typeOfSurface);
        ret.set<HorizontalKeys::TypeOfSecondFixedSurface>(typeOfSurface);
        ret.set<HorizontalKeys::ScaledValueOfFirstFixedSurface>(levelist.get() - 1);
        ret.set<HorizontalKeys::ScaleFactorOfFirstFixedSurface>(0);
        ret.set<HorizontalKeys::ScaledValueOfSecondFixedSurface>(levelist.get());
        ret.set<HorizontalKeys::ScaleFactorOfSecondFixedSurface>(0);
        alterAndValidate(ret);
        return ret;
    };

    switch (tol) {
        // The 2m and 10m entries can be removed once the fortran encoder is removed
        case TypeOfLevel::HeightAboveSea:
        case TypeOfLevel::HeightAboveSeaAt10m:
            return handleHeightAbove(102);
        case TypeOfLevel::HeightAboveGroundAt2m:
        case TypeOfLevel::HeightAboveGroundAt10m:
        case TypeOfLevel::HeightAboveGround:
            return handleHeightAbove(103);
        case TypeOfLevel::EntireLake:
            return handle2SurfacesWithoutLevel(1, 162);
        case TypeOfLevel::EntireAtmosphere:
            return handle2SurfacesWithoutLevel(1, 8);
        case TypeOfLevel::DepthBelowSeaLayer:
            return handle2SurfacesWithoutLevel(160, 160);
        case TypeOfLevel::IceLayerOnWater:
            return handle2SurfacesWithoutLevel(174, 176);
        case TypeOfLevel::Surface:
            return handle1SurfaceWithoutLevel(1);
        case TypeOfLevel::IceTopOnWater:
            return handle1SurfaceWithoutLevel(174);
        case TypeOfLevel::CloudBase:
            return handle1SurfaceWithoutLevel(2);
        case TypeOfLevel::AbstractMultipleLevels:
        case TypeOfLevel::AbstractSingleLevel:
            return handle1SurfaceWithoutLevel(191);
        case TypeOfLevel::Isothermal:
            return handle1SurfaceWithoutLevel(20);
        case TypeOfLevel::LakeBottom:
            return handle1SurfaceWithoutLevel(162);
        case TypeOfLevel::MeanSea:
            return handle1SurfaceWithoutLevel(101);
        case TypeOfLevel::MixedLayerParcel:
            return handle1SurfaceWithoutLevel(18);
        case TypeOfLevel::MostUnstableParcel:
            return handle1SurfaceWithoutLevel(17);
        case TypeOfLevel::MixingLayer:
            return handle1SurfaceWithoutLevel(166);
        case TypeOfLevel::NominalTop:
            return handle1SurfaceWithoutLevel(8);
        case TypeOfLevel::Tropopause:
            return handle1SurfaceWithoutLevel(7);
        case TypeOfLevel::Hybrid:
            return handle1SurfaceWithLevel(105, "hPa");
        case TypeOfLevel::PotentialVorticity:
            return handle1SurfaceWithLevel(109);
        case TypeOfLevel::Theta:
            return handle1SurfaceWithLevel(107);
        case TypeOfLevel::Snow:
            return handle1SurfaceWithLevel(114);  // Might be wrong... to be checked how level are set
        case TypeOfLevel::IsobaricInPa: {
            return handle1SurfaceWithLevel(100, "pa");
        }
        case TypeOfLevel::IsobaricInhPa: {
            return handle1SurfaceWithLevel(100, "hPa");
        }
        case TypeOfLevel::HighCloudLayer: {
            HorizontalKeyValueSet ret;
            ret.set<HorizontalKeys::TypeOfFirstFixedSurface>(100);
            ret.set<HorizontalKeys::TypeOfSecondFixedSurface>(8);
            ret.set<HorizontalKeys::ScaledValueOfFirstFixedSurface>(45000);
            ret.set<HorizontalKeys::ScaleFactorOfFirstFixedSurface>(0);
            alterAndValidate(ret);
            return ret;
        }
        case TypeOfLevel::MediumCloudLayer: {
            HorizontalKeyValueSet ret;
            ret.set<HorizontalKeys::TypeOfFirstFixedSurface>(100);
            ret.set<HorizontalKeys::TypeOfSecondFixedSurface>(100);
            ret.set<HorizontalKeys::ScaledValueOfFirstFixedSurface>(80000);
            ret.set<HorizontalKeys::ScaleFactorOfFirstFixedSurface>(0);
            ret.set<HorizontalKeys::ScaledValueOfSecondFixedSurface>(45000);
            ret.set<HorizontalKeys::ScaleFactorOfSecondFixedSurface>(0);
            alterAndValidate(ret);
            return ret;
        }
        case TypeOfLevel::LowCloudLayer: {
            HorizontalKeyValueSet ret;
            ret.set<HorizontalKeys::TypeOfFirstFixedSurface>(1);
            ret.set<HorizontalKeys::TypeOfSecondFixedSurface>(100);
            ret.set<HorizontalKeys::ScaledValueOfSecondFixedSurface>(80000);
            ret.set<HorizontalKeys::ScaleFactorOfSecondFixedSurface>(0);
            alterAndValidate(ret);
            return ret;
        }
        case TypeOfLevel::SeaIceLayer: {
            return handle2SurfaceLayerWithlevel(152);
        }
        case TypeOfLevel::SnowLayer: {
            return handle2SurfaceLayerWithlevel(114);
        }
        case TypeOfLevel::SoilLayer: {
            return handle2SurfaceLayerWithlevel(151);
        }
        default:
            return {};
    }
}

std::optional<datamod::VerticalKeyValueSet> verticalForTypeOfLevel(const LevelKeyValueSet& levelConf,
                                                                   const datamod::MarsKeyValueSet& mars,
                                                                   const datamod::MiscKeyValueSet& misc) {
    using namespace datamod;
    auto tol = key<LevelDef::Type>(levelConf).get();

    const auto& levtype = key<MarsKeys::LEVTYPE>(mars);
    switch (tol) {
        case TypeOfLevel::Hybrid:
        case TypeOfLevel::Snow: {
            if (levtype.has() && levtype.get() == LevType::ML) {
                datamod::VerticalKeyValueSet ret;
                ASSERT(key<MiscKeys::Pv>(misc).holdsReference());
                const auto& pv = key<MiscKeys::Pv>(misc);

                if (pv.isMissing()) {
                    std::ostringstream oss;
                    oss << "Missing key " << key<MiscKeys::Pv>().keyInfo()
                        << " to set vertical information for typeOfLevel " << tol << ". Mars keys: " << mars;
                    throw EncodeMtg2Exception(oss.str(), Here());
                };

                key<VerticalKeys::PV>(ret).setRef(pv.get());
                ASSERT(key<VerticalKeys::PV>(ret).holdsReference());
                alterAndValidate(ret);
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

void LevelSetter::allocate(util::MioGribHandle& h, const datamod::MarsKeyValueSet& mars,
                           const datamod::MiscKeyValueSet& misc, const datamod::Geometry& geo) const {
    // set type of level here....
    auto vert = verticalForTypeOfLevel(conf_, mars, misc);
    if (vert) {
        write(*vert, h);
    }
}

void LevelSetter::preset(util::MioGribHandle& h, const datamod::MarsKeyValueSet& mars,
                         const datamod::MiscKeyValueSet& misc, const datamod::Geometry& geo) const {
    setLevels(h, mars, misc, geo);
}

void LevelSetter::runtime(util::MioGribHandle& h, const datamod::MarsKeyValueSet& mars,
                          const datamod::MiscKeyValueSet& misc, const datamod::Geometry& geo) const {
    // TODO this should be only relevant for ML fields (because we don't cache them on level...)
    setLevels(h, mars, misc, geo);
}

void LevelSetter::setLevels(util::MioGribHandle& h, const datamod::MarsKeyValueSet& mars,
                            const datamod::MiscKeyValueSet& misc, const datamod::Geometry& geo) const {
    using namespace datamod;

    auto optLevel = levelForTypeOfLevel(conf_, mars, misc);
    // TODO make the LevelDef own the typeOfLevel after migration
    write(KeyValue<Grib2Keys::TypeOfLevel>{key<LevelDef::Type>(conf_)}, h);
    if (optLevel) {
        h.setValue("level", *optLevel);
    }
}

void LevelSetter::check(const util::MioGribHandle& h, const datamod::MarsKeyValueSet& mars,
                        const datamod::MiscKeyValueSet& misc, const datamod::Geometry& geo) const {
    auto inferedHoriz = horizontalForTypeOfLevel(conf_, mars, misc);
    auto horiz = read(datamod::HorizontalKeySet{}, h);
    if (inferedHoriz != horiz) {
        std::ostringstream oss;
        oss << "LevelSetter{" << conf_ << "}::check- inferred and read horizontal keys are different: " << std::endl;
        oss << "Expected: " << inferedHoriz << std::endl;
        oss << "Read: " << horiz << std::endl;
        oss << "Mars keys: " << mars << std::endl;
        throw EncodeMtg2Exception(oss.str(), Here());
    }
}

void LevelSetter::collectKeyInfo(KeyInfoList& req, KeyInfoList& opt, const datamod::MarsKeyValueSet& mars) const {
    using namespace datamod;
    addKeyInfo<MarsKeys::LEVTYPE>(req);

    auto tol = key<LevelDef::Type>(conf_).get();
    const auto& fixedLevel = key<LevelDef::FixedLevel>(conf_);
    const auto& levelist = key<MarsKeys::LEVELIST>(mars);

    // Check if levelist is required
    switch (tol) {
        // The 2m and 10m entries can be removed once the fortran encoder is removed
        case TypeOfLevel::HeightAboveSea:
        case TypeOfLevel::HeightAboveSeaAt10m:
        case TypeOfLevel::HeightAboveGroundAt2m:
        case TypeOfLevel::HeightAboveGroundAt10m:
        case TypeOfLevel::HeightAboveGround: {
            const auto& levtype = key<MarsKeys::LEVTYPE>(mars);
            const bool isSFC = levtype.has() && levtype.get() == LevType::SFC;

            if (fixedLevel.isMissing() && isSFC) {
                addKeyInfo<MarsKeys::LEVELIST>(req);
            }
            break;
        }
        case TypeOfLevel::Snow:
        case TypeOfLevel::SnowLayer:
        case TypeOfLevel::SoilLayer:
        case TypeOfLevel::SeaIceLayer:
        case TypeOfLevel::IsobaricInPa:
        case TypeOfLevel::PotentialVorticity:
        case TypeOfLevel::Theta:
        case TypeOfLevel::Hybrid:
        case TypeOfLevel::IsobaricInhPa: {
            addKeyInfo<MarsKeys::LEVELIST>(req);
            break;
        }
        default:
            break;
    }

    // Check if vertical information is required
    switch (tol) {
        case TypeOfLevel::Hybrid:
        case TypeOfLevel::Snow: {
            const auto& levtype = key<MarsKeys::LEVTYPE>(mars);
            if (levtype.has() && levtype.get() == LevType::ML) {
                addKeyInfo<MiscKeys::Pv>(req);
            }
            break;
        }
        default:
            break;
    }
}

};  // namespace multio::action::sections

#include "multio/action/encode-mtg2/sections/Level.h"
#include "multio/action/encode-mtg2/EncodeMtg2Exception.h"
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
        key<HorizontalKeys::TypeOfFirstFixedSurface>(ret).set(typeOfFirstFixedSurface);
        key<HorizontalKeys::TypeOfSecondFixedSurface>(ret).set(255);

        if (fixedLevel.has()) {
            key<HorizontalKeys::ScaledValueOfFirstFixedSurface>(ret).set(fixedLevel.get());
            key<HorizontalKeys::ScaleFactorOfFirstFixedSurface>(ret).set(0);
        }
        if (isSFC) {
            if (levelist.isMissing()) {
                throwLevelistMissing();
            }
            key<HorizontalKeys::ScaledValueOfFirstFixedSurface>(ret).set(levelist.get());
            key<HorizontalKeys::ScaleFactorOfFirstFixedSurface>(ret).set(0);
        }
        alterAndValidate(ret);
        return ret;
    };


    auto handle2SurfacesWithoutLevel
        = [&](std::int64_t typeOfFirstFixedSurface, std::int64_t typeOfSecondFixedSurface) {
              HorizontalKeyValueSet ret;
              key<HorizontalKeys::TypeOfFirstFixedSurface>(ret).set(typeOfFirstFixedSurface);
              key<HorizontalKeys::TypeOfSecondFixedSurface>(ret).set(typeOfSecondFixedSurface);
              alterAndValidate(ret);
              return ret;
          };

    auto handle1SurfaceWithoutLevel = [&](std::int64_t typeOfFirstFixedSurface) {
        HorizontalKeyValueSet ret;
        key<HorizontalKeys::TypeOfFirstFixedSurface>(ret).set(typeOfFirstFixedSurface);
        key<HorizontalKeys::TypeOfSecondFixedSurface>(ret).set(255);
        alterAndValidate(ret);
        return ret;
    };

    auto handle1SurfaceWithLevel = [&](std::int64_t typeOfFirstFixedSurface) {
        HorizontalKeyValueSet ret;

        const auto& levtype = key<MarsKeys::LEVTYPE>(mars);
        key<HorizontalKeys::TypeOfFirstFixedSurface>(ret).set(typeOfFirstFixedSurface);
        key<HorizontalKeys::TypeOfSecondFixedSurface>(ret).set(255);

        if (fixedLevel.has()) {
            key<HorizontalKeys::ScaledValueOfFirstFixedSurface>(ret).set(fixedLevel.get());
        }
        else {
            if (levelist.isMissing()) {
                throwLevelistMissing();
            }
            key<HorizontalKeys::ScaledValueOfFirstFixedSurface>(ret).set(levelist.get());
        }
        key<HorizontalKeys::ScaleFactorOfFirstFixedSurface>(ret).set(0);
        alterAndValidate(ret);
        return ret;
    };

    auto handle2SurfaceLayerWithlevel = [&](std::int64_t typeOfSurface) {
        HorizontalKeyValueSet ret;
        if (levelist.isMissing()) {
            throwLevelistMissing();
        }
        key<HorizontalKeys::TypeOfFirstFixedSurface>(ret).set(typeOfSurface);
        key<HorizontalKeys::TypeOfSecondFixedSurface>(ret).set(typeOfSurface);
        key<HorizontalKeys::ScaledValueOfFirstFixedSurface>(ret).set(levelist.get() - 1);
        key<HorizontalKeys::ScaleFactorOfFirstFixedSurface>(ret).set(0);
        key<HorizontalKeys::ScaledValueOfSecondFixedSurface>(ret).set(levelist.get());
        key<HorizontalKeys::ScaleFactorOfSecondFixedSurface>(ret).set(0);
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
            return handle1SurfaceWithLevel(105);
        case TypeOfLevel::PotentialVorticity:
            return handle1SurfaceWithLevel(109);
        case TypeOfLevel::Theta:
            return handle1SurfaceWithLevel(107);
        case TypeOfLevel::Snow:
            return handle1SurfaceWithLevel(114);  // Might be wrong... to be checked how level are set
        case TypeOfLevel::IsobaricInPa: {
            auto ret = handle1SurfaceWithLevel(100);
            key<HorizontalKeys::PressureUnits>(ret).set("pa");
            alterAndValidate(ret);
            return ret;
        }
        case TypeOfLevel::IsobaricInhPa: {
            auto ret = handle1SurfaceWithLevel(100);
            key<HorizontalKeys::PressureUnits>(ret).set("hPa");
            alterAndValidate(ret);
            return ret;
        }
        case TypeOfLevel::HighCloudLayer: {
            HorizontalKeyValueSet ret;
            key<HorizontalKeys::TypeOfFirstFixedSurface>(ret).set(100);
            key<HorizontalKeys::TypeOfSecondFixedSurface>(ret).set(8);
            key<HorizontalKeys::ScaledValueOfFirstFixedSurface>(ret).set(45000);
            key<HorizontalKeys::ScaleFactorOfFirstFixedSurface>(ret).set(0);
            alterAndValidate(ret);
            return ret;
        }
        case TypeOfLevel::MediumCloudLayer: {
            HorizontalKeyValueSet ret;
            key<HorizontalKeys::TypeOfFirstFixedSurface>(ret).set(100);
            key<HorizontalKeys::TypeOfSecondFixedSurface>(ret).set(100);
            key<HorizontalKeys::ScaledValueOfFirstFixedSurface>(ret).set(80000);
            key<HorizontalKeys::ScaleFactorOfFirstFixedSurface>(ret).set(0);
            key<HorizontalKeys::ScaledValueOfSecondFixedSurface>(ret).set(45000);
            key<HorizontalKeys::ScaleFactorOfSecondFixedSurface>(ret).set(0);
            alterAndValidate(ret);
            return ret;
        }
        case TypeOfLevel::LowCloudLayer: {
            HorizontalKeyValueSet ret;
            key<HorizontalKeys::TypeOfFirstFixedSurface>(ret).set(1);
            key<HorizontalKeys::TypeOfSecondFixedSurface>(ret).set(100);
            key<HorizontalKeys::ScaledValueOfSecondFixedSurface>(ret).set(80000);
            key<HorizontalKeys::ScaleFactorOfSecondFixedSurface>(ret).set(0);
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
                const auto& pv = key<MiscKeys::Pv>(misc);
                if (pv.isMissing()) {
                    std::ostringstream oss;
                    oss << "Missing key " << key<MiscKeys::Pv>().keyInfo()
                        << " to set vertical information for typeOfLevel " << tol << ". Mars keys: " << mars;
                    throw EncodeMtg2Exception(oss.str(), Here());
                };
                key<VerticalKeys::PV>(ret).set(pv);
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
        throw EncodeMtg2Exception("", Here());
    }
}

};  // namespace multio::action::sections

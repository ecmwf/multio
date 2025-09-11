#include "multio/mars2grib/grib2/Level.h"

#include "multio/datamod/GribKeys.h"
#include "multio/datamod/core/Record.h"
#include "multio/datamod/types/LevType.h"
#include "multio/datamod/types/TypeOfLevel.h"
#include "multio/mars2grib/Mars2GribException.h"

#include "multio/util/Print.h"

namespace multio::mars2grib::grib2 {

LevelKeys setLevel(dm::TypeOfLevel tol, std::optional<std::int64_t> fixedLevel, const dm::FullMarsRecord& mars) {
    auto throwLevelistMissing = [&]() {
        std::ostringstream oss;
        oss << "Missing levelist to extract level for typeOfLevel ";
        util::print(oss, tol);
        oss << ". Passed MARS keys: ";
        util::print(oss, mars);
        throw Mars2GribException(oss.str(), Here());
    };
    LevelKeys ret;
    ret.typeOfLevel.set(tol);
    switch (tol) {
        // The 2m and 10m entries can be removed once the fortran encoder is removed
        case dm::TypeOfLevel::HeightAboveSea:
        case dm::TypeOfLevel::HeightAboveSeaAt10m:
        case dm::TypeOfLevel::HeightAboveGroundAt2m:
        case dm::TypeOfLevel::HeightAboveGroundAt10m:
        case dm::TypeOfLevel::HeightAboveGround: {
            const bool isSFC = mars.levtype.isSet() && mars.levtype.get() == dm::LevType::SFC;

            if (fixedLevel) {
                ret.level.set(*fixedLevel);
            }
            else if (isSFC) {
                if (!mars.levelist.isSet()) {
                    throwLevelistMissing();
                }
                ret.level.set(mars.levelist.get());
            }
            break;
        }
        case dm::TypeOfLevel::Snow:
        case dm::TypeOfLevel::SnowLayer:
        case dm::TypeOfLevel::SoilLayer:
        case dm::TypeOfLevel::SeaIceLayer:
        case dm::TypeOfLevel::IsobaricInPa:
        case dm::TypeOfLevel::PotentialVorticity:
        case dm::TypeOfLevel::Theta:
        case dm::TypeOfLevel::Hybrid: {
            if (!mars.levelist.isSet()) {
                throwLevelistMissing();
            }
            ret.level.set(mars.levelist.get());
            break;
        }
        case dm::TypeOfLevel::IsobaricInhPa: {
            if (!mars.levelist.isSet()) {
                throwLevelistMissing();
            }
            ret.level.set(mars.levelist.get() / 100);
            break;
        }
        default:
            break;
    }

    dm::applyRecordDefaults(ret);
    dm::validateRecord(ret);
    return ret;
}


std::optional<VerticalKeys> setVertical(dm::TypeOfLevel tol, const dm::FullMarsRecord& mars,
                                        const dm::MiscRecord& misc) {
    switch (tol) {
        case dm::TypeOfLevel::Hybrid:
        case dm::TypeOfLevel::Snow: {
            if (mars.levtype.isSet() && mars.levtype.get() == dm::LevType::ML) {
                VerticalKeys ret;

                if (!misc.pv.isSet()) {
                    std::ostringstream oss;
                    oss << "Missing key " << dm::Pv.keyInfo() << " to set vertical information for typeOfLevel ";
                    util::print(oss, tol);
                    oss << ". Mars keys: ";
                    util::print(oss, mars);
                    throw Mars2GribException(oss.str(), Here());
                };

                ret.pvPresent.set(true);
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


};  // namespace multio::mars2grib::grib2

#include "multio/mars2grib/grib2/Geometry.h"

#include "multio/mars2grib/grib2/Utils.h"


namespace multio::mars2grib::grib2 {

void writeGeometry(const dm::Geometry& geo, metkit::codes::CodesHandle& handle, bool readbackTest) {
    std::visit(
        [&](const auto& g) {
            using GeoT = std::decay_t<decltype(g)>;
            if constexpr (std::is_same_v<GeoT, dm::GeoReducedGGRecord>) {
                handle.set("truncateDegrees", 1);
                handle.set("interpretationOfNumberOfPoints", 1);
                handle.setMissing("iDirectionIncrement");
            }
            if constexpr (std::is_same_v<GeoT, dm::GeoRegularGGRecord>) {
                handle.set("truncateDegrees", 1);
            }
            writeKeys(g, handle, readbackTest);
        },
        geo);
}


};  // namespace multio::mars2grib::grib2

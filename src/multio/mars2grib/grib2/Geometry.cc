#include "multio/datamod/ContainerInterop.h"
#include "multio/datamod/MarsMiscGeo.h"
#include "multio/datamod/core/EntryDumper.h"
#include "multio/datamod/types/GridType.h"
#include "multio/mars2grib/Mars2GribException.h"

#include "multio/mars2grib/grib2/Geometry.h"

namespace multio::mars2grib::grib2 {

void writeGeometry(const dm::Geometry& geo, metkit::codes::CodesHandle& handle) {
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
            // TODO(peier) C++20: use designater initializer
            dm::DumpOptions dumpOpts;
            dumpOpts.removeMissingKeys = true;
            dm::dumpRecord(g, handle, dumpOpts);
        },
        geo);
}


};  // namespace multio::mars2grib::grib2

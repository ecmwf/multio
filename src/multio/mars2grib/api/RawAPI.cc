#include "RawAPI.h"

namespace multio::mars2grib {

Mars2GribRaw::Mars2GribRaw(RawOptions options) : encoder_{options.cached} {}

std::unique_ptr<metkit::codes::CodesHandle> Mars2GribRaw::getHandle(const dm::FullMarsRecord& marsKeys,
                                                             const dm::MiscRecord& miscKeys,
                                                             const dm::Geometry& geoKeys) {
    return encoder_.getHandle(marsKeys, miscKeys, geoKeys);
}

}  // namespace multio::mars2grib


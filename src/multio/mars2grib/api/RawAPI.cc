#include "RawAPI.h"
#include "multio/mars2grib/Encoder.h"

namespace multio::mars2grib {

EncoderOptions toEncoderOptions(const RawOptions& opts) {
    EncoderOptions ret;
    ret.enableCache = opts.enableCache;
    ret.enableReadbackTest = opts.enableReadbackTest;
    return ret;
}

Mars2GribRaw::Mars2GribRaw(RawOptions options) : encoder_{toEncoderOptions(options)} {}

std::unique_ptr<metkit::codes::CodesHandle> Mars2GribRaw::getHandle(const dm::FullMarsRecord& marsKeys,
                                                             const dm::MiscRecord& miscKeys,
                                                             const dm::Geometry& geoKeys) {
    return encoder_.getHandle(marsKeys, miscKeys, geoKeys);
}

}  // namespace multio::mars2grib


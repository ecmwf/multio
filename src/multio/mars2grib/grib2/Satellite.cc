#include "multio/mars2grib/grib2/Satellite.h"

#include "multio/datamod/MarsMiscGeo.h"
#include "multio/datamod/core/Record.h"
#include "multio/mars2grib/Mars2GribException.h"

namespace multio::mars2grib::grib2 {

SatelliteKeys setSatellite(const dm::FullMarsRecord& mars, const dm::MiscRecord& misc) {
    SatelliteKeys ret;
    ret.numberOfContributingSpectralBands.set(1);

    ret.satelliteSeries = misc.satelliteSeries;
    ret.satelliteNumber.set(mars.ident.get());
    ret.instrumentType.set(mars.instrument.get());
    ret.scaleFactorOfCentralWaveNumber.set(misc.scaleFactorOfCentralWaveNumber.get());
    ret.scaledValueOfCentralWaveNumber.set(misc.scaledValueOfCentralWaveNumber.get());

    dm::applyRecordDefaults(ret);
    dm::validateRecord(ret);
    return ret;
}


};  // namespace multio::mars2grib::grib2

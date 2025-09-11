#include "multio/mars2grib/grib2/DirFreq.h"
#include <cmath>

#include "multio/datamod/core/Record.h"
#include "multio/mars2grib/Mars2GribException.h"

namespace multio::mars2grib::grib2 {

DirFreqArrayKeys setDirFreqArrays(const dm::MiscRecord& miscKeys) {
    if (!miscKeys.waveDirections.isSet()) {
        throw dm::DataModellingException("Additional value \"waveDirections\" is required", Here());
    }
    if (!miscKeys.waveFrequencies.isSet()) {
        throw dm::DataModellingException("Additional value \"waveFrequencies\" is required", Here());
    }

    DirFreqArrayKeys ret;
    {
        constexpr double RAD2DEG = 180.0 / M_PI;
        const int64_t scaleFactorOfWaveDirections
            = miscKeys.scaleFactorOfWaveDirections.isSet() ? miscKeys.scaleFactorOfWaveDirections.get() : 2;

        ret.numberOfWaveDirections.set(miscKeys.waveDirections.get().size());
        ret.scaleFactorOfWaveDirections.set(scaleFactorOfWaveDirections);
        ret.scaledValuesOfWaveDirections.set(std::vector<int64_t>{});
        ret.scaledValuesOfWaveDirections.modify().reserve(ret.numberOfWaveDirections.get());

        std::transform(
            miscKeys.waveDirections.get().begin(), miscKeys.waveDirections.get().end(),
            std::back_inserter(ret.scaledValuesOfWaveDirections.modify()), [&](double x) {
                return static_cast<int64_t>(std::round(x * std::pow(10, scaleFactorOfWaveDirections) * RAD2DEG));
            });
    }
    {
        const int64_t scaleFactorOfWaveFrequencies
            = miscKeys.scaleFactorOfWaveFrequencies.isSet() ? miscKeys.scaleFactorOfWaveFrequencies.get() : 6;

        ret.numberOfWaveFrequencies.set(miscKeys.waveFrequencies.get().size());
        ret.scaleFactorOfWaveFrequencies.set(scaleFactorOfWaveFrequencies);
        ret.scaledValuesOfWaveFrequencies.set(std::vector<int64_t>{});
        ret.scaledValuesOfWaveFrequencies.modify().reserve(ret.numberOfWaveFrequencies.get());

        std::transform(miscKeys.waveFrequencies.get().begin(), miscKeys.waveFrequencies.get().end(),
                       std::back_inserter(ret.scaledValuesOfWaveFrequencies.modify()), [&](double x) {
                           return static_cast<int64_t>(std::round(x * std::pow(10, scaleFactorOfWaveFrequencies)));
                       });
    }
    dm::applyRecordDefaults(ret);
    dm::validateRecord(ret);
    return ret;
}

DirFreqMarsKeys setDirFreqMars(const dm::FullMarsRecord& mars) {
    DirFreqMarsKeys ret;
    ret.waveDirectionNumber.set(mars.direction.get());
    ret.waveFrequencyNumber.set(mars.frequency.get());

    dm::applyRecordDefaults(ret);
    dm::validateRecord(ret);
    return ret;
}


};  // namespace multio::mars2grib::grib2

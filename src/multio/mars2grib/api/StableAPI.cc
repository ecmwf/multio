#include "StableAPI.h"
#include "RawAPI.h"
#include "multio/datamod/AtlasGeo.h"
#include "multio/mars2grib/Mars2GribException.h"

namespace multio::mars2grib {

namespace dm = multio::datamod;

RawOptions toRawOptions(Options opts) {
    RawOptions ret;
    ret.enableCache = opts.enableCache;
    return ret;
};

// Dispatching a key to an entry definition.
// If the passed type is not supported for the passed entry an Mars2GribException is throw
template <typename Rec, typename V>
void setValOrThrow(const std::string& key, Rec& rec, V&& val) {
    dm::dispatchEntry(key, rec, [&](const auto& entryDef) {
        using EntryType = typename std::decay_t<decltype(entryDef)>::EntryType;
        if constexpr (EntryType::template CanSetValue_v<V>) {
            entryDef.get(rec).set(std::forward<V>(val));
        }
        else {
            throw Mars2GribException("Unsupported type", Here());
        }
    });
}

void MarsIdentifiers::set(const std::string& key, std::int64_t value) {
    setValOrThrow(key, values_, value);
};
void MarsIdentifiers::set(const std::string& key, const std::string& value) {
    setValOrThrow(key, values_, value);
};

void AdditionalValues::setTablesVersion(std::int64_t value) {
    dm::TablesVersion.get(values_).set(value);
}
void AdditionalValues::setGeneratingProcessIdentifier(std::int64_t value) {
    dm::GeneratingProcessIdentifier.get(values_).set(value);
}
void AdditionalValues::setTypeOfProcessedData(std::int64_t value) {
    dm::TypeOfProcessedDataEntry.get(values_).set(value);
}
void AdditionalValues::setInitialStep(std::int64_t value) {
    dm::InitialStep.get(values_).set(value);
}
void AdditionalValues::setTimeIncrementInSeconds(std::int64_t value) {
    dm::TimeIncrementInSeconds.get(values_).set(value);
}
void AdditionalValues::setLengthOfTimeWindowInSeconds(std::int64_t value) {
    dm::LengthOfTimeWindowInSeconds.get(values_).set(value);
}
void AdditionalValues::setBitmapPresent(bool value) {
    dm::BitmapPresent.get(values_).set(value);
}
void AdditionalValues::setMissingValue(double value) {
    dm::MissingValue.get(values_).set(value);
}
void AdditionalValues::setTypeOfEnsembleForecast(std::int64_t value) {
    dm::TypeOfEnsembleForecast.get(values_).set(value);
}
void AdditionalValues::setNumberOfForecastsInEnsemble(std::int64_t value) {
    dm::NumberOfForecastsInEnsemble.get(values_).set(value);
}
void AdditionalValues::setSatelliteSeries(std::int64_t value) {
    dm::SatelliteSeries.get(values_).set(value);
}
void AdditionalValues::setScaleFactorOfCentralWaveNumber(std::int64_t value) {
    dm::ScaleFactorOfCentralWaveNumber.get(values_).set(value);
}
void AdditionalValues::setScaledValueOfCentralWaveNumber(std::int64_t value) {
    dm::ScaledValueOfCentralWaveNumber.get(values_).set(value);
}
void AdditionalValues::setPV(std::reference_wrapper<const std::vector<double>> value) {
    dm::Pv.get(values_).set(value);
}
// TODO(pgeier)
// void AdditionalValues::setPV(std::int64_t numberOfLevels) {
//     NOTIMP;
// }
void AdditionalValues::setWaveDirections(std::reference_wrapper<const std::vector<double>> value) {
    dm::WaveDirections.get(values_).set(value);
}
void AdditionalValues::setWaveFrequencies(std::reference_wrapper<const std::vector<double>> value) {
    dm::WaveFrequencies.get(values_).set(value);
}
void AdditionalValues::setBitsPerValue(std::int64_t value) {
    dm::BitsPerValue.get(values_).set(value);
}

GeometryType parseGeometryType(const std::string& val) {
    if (val == "regular_gg") {
        return GeometryType::RegularGG;
    }
    if (val == "reduced_gg") {
        return GeometryType::ReducedGG;
    }
    if (val == "regular_ll") {
        return GeometryType::RegularLL;
    }
    if (val == "sh") {
        return GeometryType::SH;
    }
    if (val == "HEALPix") {
        return GeometryType::HEALPix;
    }
    throw Mars2GribException(std::string("Unknown geometry type ") + val, Here());
};

dm::Geometry initGeometry(GeometryType t) {
    switch (t) {
        case GeometryType::ReducedGG: {
            return dm::GeoReducedGGRecord{};
        }
        case GeometryType::RegularGG: {
            return dm::GeoRegularGGRecord{};
        }
        case GeometryType::RegularLL: {
            return dm::GeoRegularLLRecord{};
        }
        case GeometryType::HEALPix: {
            return dm::GeoHEALPixRecord{};
        }
        case GeometryType::SH: {
            return dm::GeoSHRecord{};
        }
        default:
            throw Mars2GribException(std::string("initGeometry: Unhandled geometry type"), Here());
    }
}


void GeometryValues::setGeometryType(const std::string& type) {
    setGeometryType(parseGeometryType(type));
};
void GeometryValues::setGeometryType(GeometryType type) {
    values_ = initGeometry(type);
};


void GeometryValues::set(const std::string& key, std::int64_t value) {
    std::visit([&](auto& geoRec) { setValOrThrow(key, geoRec, value); }, values_);
}
void GeometryValues::set(const std::string& key, const std::string& value) {
    std::visit([&](auto& geoRec) { setValOrThrow(key, geoRec, value); }, values_);
}
void GeometryValues::set(const std::string& key, std::reference_wrapper<const std::vector<double>> value) {
    std::visit([&](auto& geoRec) { setValOrThrow(key, geoRec, value); }, values_);
}


Mars2Grib::Mars2Grib(Options options) : rawApi_{toRawOptions(options)} {};


std::unique_ptr<metkit::codes::CodesHandle> Mars2Grib::encode(const MarsIdentifiers& mars, const AdditionalValues& misc,
                                                              const GeometryValues& geo, const double* values,
                                                              size_t len) {
    auto handle = rawApi_.getHandle(mars.values_, misc.values_, geo.values_);
    handle->set("values", metkit::codes::Span<const double>(values, len));
    return handle;
}

std::unique_ptr<metkit::codes::CodesHandle> Mars2Grib::encode(const MarsIdentifiers& mars, const AdditionalValues& misc,
                                                              const GeometryValues& geo, const float* values,
                                                              size_t len) {
    auto handle = rawApi_.getHandle(mars.values_, misc.values_, geo.values_);
    handle->set("values", metkit::codes::Span<const float>(values, len));
    return handle;
}

std::unique_ptr<metkit::codes::CodesHandle> Mars2Grib::encode(const MarsIdentifiers& mars, const AdditionalValues& misc,
                                                              const GeometryValues& geo,
                                                              const std::vector<double>& values) {
    return encode(mars, misc, geo, values.data(), values.size());
}

std::unique_ptr<metkit::codes::CodesHandle> Mars2Grib::encode(const MarsIdentifiers& mars, const AdditionalValues& misc,
                                                              const GeometryValues& geo,
                                                              const std::vector<float>& values) {
    return encode(mars, misc, geo, values.data(), values.size());
}

std::unique_ptr<metkit::codes::CodesHandle> Mars2Grib::encode(const MarsIdentifiers& mars, const AdditionalValues& misc,
                                                              const double* values, size_t len) {
    auto handle = rawApi_.getHandle(mars.values_, misc.values_, dm::makeUnscopedGeometry(mars.values_));
    handle->set("values", metkit::codes::Span<const double>(values, len));
    return handle;
}
std::unique_ptr<metkit::codes::CodesHandle> Mars2Grib::encode(const MarsIdentifiers& mars, const AdditionalValues& misc,
                                                              const float* values, size_t len) {
    auto handle = rawApi_.getHandle(mars.values_, misc.values_, dm::makeUnscopedGeometry(mars.values_));
    handle->set("values", metkit::codes::Span<const float>(values, len));
    return handle;
}

std::unique_ptr<metkit::codes::CodesHandle> Mars2Grib::encode(const MarsIdentifiers& mars, const AdditionalValues& misc,
                                                              const std::vector<double>& values) {
    return encode(mars, misc, values.data(), values.size());
}
std::unique_ptr<metkit::codes::CodesHandle> Mars2Grib::encode(const MarsIdentifiers& mars, const AdditionalValues& misc,
                                                              const std::vector<float>& values) {
    return encode(mars, misc, values.data(), values.size());
}

std::unique_ptr<metkit::codes::CodesHandle> Mars2Grib::encode(const MarsIdentifiers& mars, const AdditionalValues& misc,
                                                              const GeometryValues& geo) {
    return rawApi_.getHandle(mars.values_, misc.values_, geo.values_);
}

std::unique_ptr<metkit::codes::CodesHandle> Mars2Grib::encode(const MarsIdentifiers& mars,
                                                              const AdditionalValues& misc) {
    return rawApi_.getHandle(mars.values_, misc.values_, dm::makeUnscopedGeometry(mars.values_));
}

}  // namespace multio::mars2grib

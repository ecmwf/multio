/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#pragma once

#include <cstdint>
#include <memory>
#include <vector>

#include "metkit/codes/CodesHandleDeleter.h"

#include "multio/datamod/MarsMiscGeo.h"

#include "multio/mars2grib/api/RawAPI.h"

namespace multio::mars2grib {

namespace dm = multio::datamod;

//---------------------------------------------------------------------------------------------------------------------

struct Options {
    bool cached = true;
};

//---------------------------------------------------------------------------------------------------------------------


struct MarsValues {
    void set(const std::string& key, std::int64_t value);
    void set(const std::string& key, const std::string& value);

    // Implementation  detail
    // Can change in the future
    dm::MarsRecord values_;
};


//---------------------------------------------------------------------------------------------------------------------

// Additional key/values for encoding
// Alternative name suggestion: EncodingDetails
struct AdditionalValues {
    // Default is the latest tablesVersion
    void setTablesVersion(std::int64_t);

    // ATM we try to map these keys for a few combination of MARS keys
    // however, we can not guaratee to do it for all cases and it might need
    // customization
    void setGeneratingProcessIdentifier(std::int64_t);
    void setTypeOfProcessedData(std::int64_t);

    // to be reviewed if required after implemneting statType & timeSpan properly
    void setInitialStep(std::int64_t);

    // Describe the time difference between points for inner most statistical computation
    void setTimeIncrementInSeconds(std::int64_t);

    // Used with ANOFFSET
    // anoffset: sets position within a time window
    // lengthOfTimeWindow: sets the duration of the analysis window
    void setLengthOfTimeWindowInSeconds(std::int64_t);

    void setBitmapPresent(bool);
    // If bitmap present is set, missing value can be overwritten
    void setMissingValue(double);

    void setTypeOfEnsembleForecast(std::int64_t);
    void setNumberOfForecastsInEnsemble(std::int64_t);

    // to be removed once DGOV updates MARS language
    void setSatelliteSeries(std::int64_t);
    // to be checked if these are possibly derived from MARS keys
    void setScaleFactorOfCentralWavenumber(std::int64_t);
    void setScaledValueOfCentralWavenumber(std::int64_t);

    // Will be inferred in the future given the numberOfLevels
    // For now the full array has to be provided
    void setPV(std::reference_wrapper<const std::vector<double>>);
    // To be implemented ...
    void setPV(std::int64_t numberOfLevels);

    // Will be inferred in the future.
    // For now the full array has to be provided
    void setWaveDirections(std::reference_wrapper<const std::vector<double>>);
    void setWaveFrequencies(std::reference_wrapper<const std::vector<double>>);

    // Should be looked up by a table with Param, Levtype...
    void setBitsPerValue(std::int64_t);

    // Implementation  detail
    // Can change in the future
    dm::MiscRecord values_;
};


enum class GeometryType : std::size_t
{
    GG,       // gg
    LL,       // ll
    SH,       // sh
    HEALPix,  // HEALPix
};

struct GeometryValues {

    /**
     * @brief Set the geometry type.
     *
     * This function sets the geometry type for the object.
     * Clears all values if the same type is set twice.
     *
     * @param type The geometry type identifier. Valid values are:
     *  - `"gg"`       : Galactocentric grid
     *  - `"sh"`       : Spherical harmonics
     *  - `"HEALPix"`  : HEALPix tessellation
     *  - `"ll"`       : Latitude-longitude grid
     *
     * @throws Mars2GribException if `type` is not one of the valid values.
     */
    void setGeometryType(const std::string& key);
    void setGeometryType(GeometryType key);

    void set(const std::string& key, std::int64_t value);
    void set(const std::string& key, const std::string& value);
    void set(const std::string& key, std::reference_wrapper<const std::vector<double>>);

    // Implementation  detail
    // Can change in the future
    dm::Geometry values_;
};


class Mars2Grib {
public:
    Mars2Grib(Options options = Options{});  // Only possible constructor

    Mars2Grib(const Mars2Grib&) = delete;
    Mars2Grib(Mars2Grib&&) = default;


    Mars2Grib& operator=(const Mars2Grib&) = delete;
    Mars2Grib& operator=(Mars2Grib&&) = default;

    ~Mars2Grib() = default;


    std::unique_ptr<codes_handle> encode(const MarsValues& mars, const AdditionalValues& misc,
                                         const GeometryValues& geom, const std::vector<double>& values);
    std::unique_ptr<codes_handle> encode(const MarsValues& mars, const AdditionalValues& misc,
                                         const GeometryValues& geom, const std::vector<float>& values);

    std::unique_ptr<codes_handle> encode(const MarsValues& mars, const AdditionalValues& misc,
                                         const std::vector<double>& values);
    std::unique_ptr<codes_handle> encode(const MarsValues& mars, const AdditionalValues& misc,
                                         const std::vector<float>& values);

    std::unique_ptr<codes_handle> encode(const MarsValues& mars, const AdditionalValues& misc,
                                         const GeometryValues& geom);
    std::unique_ptr<codes_handle> encode(const MarsValues& mars, const AdditionalValues& misc);

private:
    Mars2GribRaw rawApi_;
};

}  // namespace multio::mars2grib

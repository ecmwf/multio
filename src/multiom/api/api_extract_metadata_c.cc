#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <functional>
#include <iostream>
#include <optional>
#include <sstream>
#include <unordered_map>
#include <vector>


#include "c/api.h"
#include "eccodes.h"

namespace {
bool hasKey(codes_handle* handle, const char* key) {
    return codes_is_defined(handle, key) != 0;
}

std::string getString(codes_handle* handle, const char* key) {
    // char keyval[1024];
    // size_t keylen = sizeof(keyval);
    // CODES_CHECK(codes_get_string(handle, key, keyval, &keylen), nullptr);
    std::string ret;
    std::size_t keylen = 1024;
    // Use resize instead of reserive - will allocate enough memory and sets the size internally to the string
    ret.resize(keylen);
    // Now eccodes is writing in the buffer...
    CODES_CHECK(codes_get_string(handle, key, ret.data(), &keylen), nullptr);
    // Now a second resize will only shrink (never enlarge) the buffer and just adjust the size without modifying the
    // buffer ret.resize(keylen-1);
    ret.resize(strlen(ret.c_str()));  // does also work and might be more reliable
    // std::cout << "Get string " << key << " (len: " << keylen << ", stringsize: " << ret.size() << ": " <<
    // std::string(ret) <<std::endl;
    return ret;
}

long getLong(codes_handle* handle, const char* key) {
    long ret;
    CODES_CHECK(codes_get_long(handle, key, &ret), nullptr);
    return ret;
}

double getDouble(codes_handle* handle, const char* key) {
    double ret;
    CODES_CHECK(codes_get_double(handle, key, &ret), nullptr);
    return ret;
}

std::size_t getSize(codes_handle* handle, const char* key) {
    std::size_t ret;
    CODES_CHECK(codes_get_size(handle, key, &ret), nullptr);
    return ret;
}

std::vector<double> getDoubleArray(codes_handle* handle, const char* key) {
    std::vector<double> ret;
    std::size_t size = getSize(handle, key);
    ret.resize(size);
    CODES_CHECK(codes_get_double_array(handle, key, ret.data(), &size), nullptr);
    ret.resize(size);
    return ret;
}

std::vector<long> getLongArray(codes_handle* handle, const char* key) {
    std::vector<long> ret;
    std::size_t size = getSize(handle, key);
    ret.resize(size);
    CODES_CHECK(codes_get_long_array(handle, key, ret.data(), &size), nullptr);
    ret.resize(size);
    return ret;
}

enum class SetDefault : unsigned long
{
    IfKeyGiven = 0,
    Always = 1,
};

using OptVal = std::optional<std::string>;
int getAndSet(codes_handle* h, void* dict, const char* key, const char* setName = NULL, OptVal defaultVal = {},
              SetDefault defPolicy = SetDefault::IfKeyGiven) {
    if (hasKey(h, key)) {
        std::string val = getString(h, key);
        if (val.empty() && defaultVal) {
            val = *defaultVal;
        }
        int ret = multio_grib2_dict_set(dict, setName == NULL ? key : setName, val.data());
        if (ret != 0)
            return ret;
    }
    else {
        if (defaultVal && (defPolicy == SetDefault::Always)) {
            int ret = multio_grib2_dict_set(dict, setName == NULL ? key : setName, defaultVal->data());
            if (ret != 0)
                return ret;
        }
    }
    return 0;
}
int getAndSet(codes_handle* h, void* dict, const char* key, OptVal defaultVal,
              SetDefault defPolicy = SetDefault::IfKeyGiven) {
    return getAndSet(h, dict, key, NULL, defaultVal, defPolicy);
}
int getAndSetIfNonZero(codes_handle* h, void* dict, const char* key, const char* setName = NULL) {
    if (hasKey(h, key)) {
        std::string val = getString(h, key);
        if (!val.empty() && val != "0") {
            int ret = multio_grib2_dict_set(dict, setName == NULL ? key : setName, val.data());
            if (ret != 0)
                return ret;
        }
    }
    return 0;
}

using OptVal = std::optional<std::string>;
int getAndSetDouble(codes_handle* h, void* dict, const char* key, const char* setName = NULL, OptVal defaultVal = {},
                    SetDefault defPolicy = SetDefault::IfKeyGiven) {
    if (hasKey(h, key)) {
        std::string val = std::to_string(getDouble(h, key));
        int ret = multio_grib2_dict_set(dict, setName == NULL ? key : setName, val.data());
        if (ret != 0)
            return ret;
    }
    else {
        if (defaultVal && (defPolicy == SetDefault::Always)) {
            int ret = multio_grib2_dict_set(dict, setName == NULL ? key : setName, defaultVal->data());
            if (ret != 0)
                return ret;
        }
    }
    return 0;
}
int getAndSetDouble(codes_handle* h, void* dict, const char* key, OptVal defaultVal,
                    SetDefault defPolicy = SetDefault::IfKeyGiven) {
    return getAndSetDouble(h, dict, key, NULL, defaultVal, defPolicy);
}

using OptVal = std::optional<std::string>;
int getAndSetLong(codes_handle* h, void* dict, const char* key, const char* setName = NULL, OptVal defaultVal = {},
                  SetDefault defPolicy = SetDefault::IfKeyGiven) {
    if (hasKey(h, key)) {
        std::string val = std::to_string(getLong(h, key));
        int ret = multio_grib2_dict_set(dict, setName == NULL ? key : setName, val.data());
        if (ret != 0)
            return ret;
    }
    else {
        if (defaultVal && (defPolicy == SetDefault::Always)) {
            int ret = multio_grib2_dict_set(dict, setName == NULL ? key : setName, defaultVal->data());
            if (ret != 0)
                return ret;
        }
    }
    return 0;
}
int getAndSetLong(codes_handle* h, void* dict, const char* key, OptVal defaultVal,
                  SetDefault defPolicy = SetDefault::IfKeyGiven) {
    return getAndSetLong(h, dict, key, NULL, defaultVal, defPolicy);
}

template <typename T>
std::string arrayToJSONString(const std::vector<T>& arr) {
    std::ostringstream oss;
    bool first = true;
    oss << "[";
    for (const auto& v : arr) {
        if (first) {
            first = false;
        }
        else {
            oss << ", ";
        }
        oss << v;
    }
    oss << "]";

    // std::cout << "Array: " << oss.str() << std::endl;

    return oss.str();
}

int getAndSetDoubleArray(codes_handle* h, void* dict, const char* key, const char* setName = NULL) {
    if (hasKey(h, key)) {
        int ret = multio_grib2_dict_set(dict, setName == NULL ? key : setName,
                                        arrayToJSONString(getDoubleArray(h, key)).data());
        if (ret != 0)
            return ret;
    }
    return 0;
}

int getAndSetLongArray(codes_handle* h, void* dict, const char* key, const char* setName = NULL) {
    if (hasKey(h, key)) {
        auto jsonData = arrayToJSONString(getLongArray(h, key));
        int ret = multio_grib2_dict_set(dict, setName == NULL ? key : setName, jsonData.data());
        if (ret != 0)
            return ret;
    }
    return 0;
}

using GridTypeFunction = std::function<int(codes_handle*, void*, void*)>;

int handleReducedGG(codes_handle* h, void* mars_dict, void* par_dict) {
    void* geom = NULL;
    int ret = multio_grib2_dict_create(&geom, "reduced-gg");


    ret = getAndSet(h, geom, "truncateDegrees", "truncate-degrees");
    if (ret != 0)
        return ret;

    ret = getAndSetIfNonZero(h, geom, "numberOfPointsAlongAMeridian", "number-of-points-along-a-meridian");
    if (ret != 0)
        return ret;

    ret = getAndSetIfNonZero(h, geom, "numberOfParallelsBetweenAPoleAndTheEquator",
                             "number-of-parallels-between-pole-and-equator");
    if (ret != 0)
        return ret;

    ret = getAndSetDouble(h, geom, "latitudeOfFirstGridPointInDegrees", "latitude-of-first-grid-point-in-degrees");
    if (ret != 0)
        return ret;

    ret = getAndSetDouble(h, geom, "longitudeOfFirstGridPointInDegrees", "longitude-of-first-grid-point-in-degrees");
    if (ret != 0)
        return ret;

    ret = getAndSetDouble(h, geom, "latitudeOfLastGridPointInDegrees", "latitude-of-last-grid-point-in-degrees");
    if (ret != 0)
        return ret;

    ret = getAndSetDouble(h, geom, "longitudeOfLastGridPointInDegrees", "longitude-of-last-grid-point-in-degrees");
    if (ret != 0)
        return ret;

    ret = getAndSetLongArray(h, geom, "pl", "pl");
    if (ret != 0)
        return ret;

    ret = multio_grib2_dict_set(mars_dict, "repres", "gg");
    if (ret != 0)
        return ret;

    return multio_grib2_dict_set_geometry(par_dict, geom);
}

int handleSH(codes_handle* h, void* mars_dict, void* par_dict) {
    void* geom = NULL;
    int ret = multio_grib2_dict_create(&geom, "sh");


    ret = getAndSet(h, geom, "pentagonalResolutionParameterJ", "pentagonal-resolution-j");
    if (ret != 0)
        return ret;

    ret = getAndSet(h, geom, "pentagonalResolutionParameterK", "pentagonal-resolution-k");
    if (ret != 0)
        return ret;

    ret = getAndSet(h, geom, "pentagonalResolutionParameterM", "pentagonal-resolution-m");
    if (ret != 0)
        return ret;

    ret = multio_grib2_dict_set(mars_dict, "repres", "sh");
    if (ret != 0)
        return ret;


    return multio_grib2_dict_set_geometry(par_dict, geom);
}

int handleLL(codes_handle* h, void* mars_dict, void* par_dict) {
    void* geom = NULL;
    int ret = multio_grib2_dict_create(&geom, "ll");


    ret = multio_grib2_dict_set(mars_dict, "repres", "ll");
    if (ret != 0)
        return ret;


    return multio_grib2_dict_set_geometry(par_dict, geom);
}

int handleGridType(codes_handle* h, const std::string& gridType, void* mars_dict, void* par_dict) {
    const static std::unordered_map<std::string, GridTypeFunction> gridMap{
        {"reduced_gg", &handleReducedGG},
        {"regular_ll", &handleLL},
        {"sh", &handleSH},
    };

    const auto gridTypeFunc = gridMap.find(gridType);
    // std::cout << "GridType: " << gridType << " " << (gridType == "reduced_gg") << std::endl;

    if (gridTypeFunc != gridMap.cend()) {
        return gridTypeFunc->second(h, mars_dict, par_dict);
    }

    std::cerr << "Unhandled gridType '" << gridType << "'" << std::endl;
    return -1;
};

int handlePackingType(codes_handle* h, const std::string& packingType, void* mars_dict) {
    const static std::unordered_map<std::string, std::string> packingMap{
        {"grid_simple", "simple"},
        {"grid_complex", "complex"},
        {"spectral_complex", "complex"},
        {"grid_ccsds", "ccsds"},
    };

    const auto packingTypeVal = packingMap.find(packingType);

    if (packingTypeVal != packingMap.cend()) {
        return multio_grib2_dict_set(mars_dict, "packing", packingTypeVal->second.c_str());
    }

    std::cerr << "Unhandled packingType '" << packingType << "'" << std::endl;
    return -1;
};


}  // namespace

extern "C" {

int multio_grib2_encoder_extract_metadata(void* multio_grib2, void* grib, void** mars_dict, void** par_dict) {
    int ret = 0;
    codes_handle* h = NULL;


    // TODO
    // - add defaults
    // - try to iterate mars namespace

    h = (codes_handle*)grib;

    ret = multio_grib2_dict_create(mars_dict, "mars");
    if (ret != 0)
        return ret;

    ret = multio_grib2_dict_create(par_dict, "parametrization");
    if (ret != 0)
        return ret;

    // Handling mars keys
    ret = getAndSet(h, *mars_dict, "stream");
    if (ret != 0)
        return ret;

    ret = getAndSet(h, *mars_dict, "type");
    if (ret != 0)
        return ret;

    ret = getAndSet(h, *mars_dict, "class");
    if (ret != 0)
        return ret;

    if (hasKey(h, "origin")) {
        ret = getAndSet(h, *mars_dict, "origin");
        if (ret != 0)
            return ret;
    }
    else if (hasKey(h, "centre")) {
        ret = getAndSet(h, *mars_dict, "centre", "origin");
        if (ret != 0)
            return ret;
    }

    ret = getAndSet(h, *mars_dict, "anoffest");
    if (ret != 0)
        return ret;

    ret = getAndSetIfNonZero(h, *mars_dict, "number");
    if (ret != 0)
        return ret;

    ret = getAndSet(h, *mars_dict, "ident");
    if (ret != 0)
        return ret;

    ret = getAndSet(h, *mars_dict, "instrument");
    if (ret != 0)
        return ret;

    ret = getAndSet(h, *mars_dict, "channel");
    if (ret != 0)
        return ret;

    // TODO paramType is experimental
    ret = getAndSet(h, *mars_dict, "paramType");
    if (ret != 0)
        return ret;

    ret = getAndSet(h, *mars_dict, "chemId", "chem");
    if (ret != 0)
        return ret;

    ret = getAndSet(h, *mars_dict, "param");
    if (ret != 0)
        return ret;

    ret = getAndSet(h, *mars_dict, "model");
    if (ret != 0)
        return ret;

    if (hasKey(h, "levtype")) {
        std::string levtype = getString(h, "levtype");
        int ret = multio_grib2_dict_set(*mars_dict, "levtype", levtype.c_str());
        if (ret != 0)
            return ret;

        // The encoders expect levtype pl with levelist in Pa - hence we need to convert hPa properly
        if (hasKey(h, "level")) {
            std::optional<std::string> levelist;
            if (levtype == "pl") {
                std::string pressureUnits = getString(h, "pressureUnits");
                if (pressureUnits == "hPa") {
                    long levelistVal = getLong(h, "level");
                    levelist = std::to_string(levelistVal * 100);
                }
            }

            if (!levelist) {
                levelist = getString(h, "level");
            }

            int ret = multio_grib2_dict_set(*mars_dict, "levelist", levelist->c_str());
            if (ret != 0)
                return ret;
        }
    }

    ret = getAndSet(h, *mars_dict, "direction");
    if (ret != 0)
        return ret;

    ret = getAndSet(h, *mars_dict, "frequency");
    if (ret != 0)
        return ret;

    ret = getAndSet(h, *mars_dict, "date");
    if (ret != 0)
        return ret;
        
    ret = getAndSet(h, *mars_dict, "hdate");
    if (ret != 0)
        return ret;

    // Handle time explicitly - generate a HHMMSS representation istead of dafult HHMM representation
    {
        // TODO - this will cause problems with referenceDate/time in grib2....
        long hh = getLong(h, "hour");
        long mm = getLong(h, "minute");
        long ss = getLong(h, "second");
        std::string timeStr{std::to_string(hh * 10000 + mm * 100 + ss)};
        ret = multio_grib2_dict_set(*mars_dict, "time", timeStr.c_str());
        if (ret != 0)
            return ret;
    }


    // For some reason mars returns an empty string for step
    if (hasKey(h, "endStep")) {
        long endStep = getLong(h, "endStep");
        long startStep = getLong(h, "startStep");
        
        ret = multio_grib2_dict_set(*mars_dict, "step", std::to_string(endStep).c_str());
        if (ret != 0)
            return ret;
            
        long stepRange = endStep - startStep;
        if (stepRange > 0) {
            ret = multio_grib2_dict_set(*mars_dict, "timeproc", std::to_string(stepRange).c_str());
            if (ret != 0)
                return ret;
        }
    }

    ret = getAndSet(h, *mars_dict, "truncation");
    if (ret != 0)
        return ret;

    ret = getAndSet(h, *mars_dict, "timeproc");
    if (ret != 0)
        return ret;

    ret = getAndSet(h, *mars_dict, "expver");
    if (ret != 0)
        return ret;

    ret = getAndSet(h, *mars_dict, "gridName", "grid");
    if (ret != 0)
        return ret;


    // Handling parametrization keys
    ret = getAndSet(h, *par_dict, "tablesVersion");
    if (ret != 0)
        return ret;

    ret = getAndSet(h, *par_dict, "generatingProcessIdentifier");
    if (ret != 0)
        return ret;

    ret = getAndSetLong(h, *par_dict, "typeOfProcessedData");
    if (ret != 0)
        return ret;

    ret = getAndSet(h, *par_dict, "initialStep", OptVal{"0"}, SetDefault::Always);
    if (ret != 0)
        return ret;

    ret = getAndSet(h, *par_dict, "lengthOfTimeStepInSeconds", OptVal{"3600"}, SetDefault::Always);
    if (ret != 0)
        return ret;

    ret = getAndSet(h, *par_dict, "lengthOfTimeRangeInSeconds", OptVal{"3600"}, SetDefault::IfKeyGiven);
    if (ret != 0)
        return ret;

    ret = getAndSet(h, *par_dict, "valuesScaleFactor");
    if (ret != 0)
        return ret;

    ret = getAndSet(h, *par_dict, "pv");
    if (ret != 0)
        return ret;

    ret = getAndSetIfNonZero(h, *par_dict, "numberOfMissingValues");
    if (ret != 0)
        return ret;

    ret = getAndSet(h, *par_dict, "valueOfMissingValues");
    if (ret != 0)
        return ret;

    ret = getAndSet(h, *par_dict, "systemNumber");
    if (ret != 0)
        return ret;

    ret = getAndSet(h, *par_dict, "methodNumber");
    if (ret != 0)
        return ret;

    // Set this to missing - will only be read if number is non zero
    if (hasKey(h, "number")) {
        if (hasKey(h, "typeOfEnsembleForecast")) {
            ret = getAndSet(h, *par_dict, "typeOfEnsembleForecast");
            if (ret != 0)
                return ret;
        }
        else if (hasKey(h, "eps")) {
            ret = getAndSet(h, *par_dict, "eps", "typeOfEnsembleForecast");
            if (ret != 0)
                return ret;
        }
    }

    ret = getAndSetIfNonZero(h, *par_dict, "numberOfForecastsInEnsemble");
    if (ret != 0)
        return ret;

    ret = getAndSet(h, *par_dict, "lengthOfTimeWindow");
    if (ret != 0)
        return ret;

    ret = getAndSet(h, *par_dict, "bitsPerValue", OptVal{"24"});
    if (ret != 0)
        return ret;

    ret = getAndSet(h, *par_dict, "periodMin");
    if (ret != 0)
        return ret;

    ret = getAndSet(h, *par_dict, "periodMax");
    if (ret != 0)
        return ret;

    ret = getAndSet(h, *par_dict, "waveDirections");
    if (ret != 0)
        return ret;

    ret = getAndSet(h, *par_dict, "waveFrequencies");
    if (ret != 0)
        return ret;

    ret = getAndSet(h, *par_dict, "satelliteSeries");
    if (ret != 0)
        return ret;

    ret = getAndSet(h, *par_dict, "scaleFactorOfCentralWavenumber");
    if (ret != 0)
        return ret;

    ret = getAndSet(h, *par_dict, "scaledValueOfCentralWavenumber");
    if (ret != 0)
        return ret;


    // TODO - this should be only set to 1 for statistical fields with step 0
    ret = multio_grib2_dict_set(*par_dict, "encodeStepZero", "1");
    if (ret != 0)
        return ret;

    if (hasKey(h, "setPackingType")) {
        std::string setPackingType = getString(h, "setPackingType");
        // std::cout << "setPackingType: " << setPackingType << std::endl;

        ret = handlePackingType(h, setPackingType, *mars_dict);
        if (ret != 0)
            return ret;
    }


    if (hasKey(h, "gridType")) {
        std::string gridType = getString(h, "gridType");
        // std::cout << "Grid: " << gridType << std::endl;

        ret = handleGridType(h, gridType, *mars_dict, *par_dict);
        if (ret != 0)
            return ret;
    }

    // ret = multio_grib2_dict_set(*par_dict, "geometry" geometry);
    // if(ret != 0) return ret;

    return 0;
}
}

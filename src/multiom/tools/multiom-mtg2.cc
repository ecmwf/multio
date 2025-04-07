/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/// @author Domokos Sarmany
/// @author Simon Smart
/// @author Tiago Quintino

/// @date Oct 2019

#include <stdio.h>
#include <stdlib.h>
#include <fstream>
#include <regex>
#include <stdexcept>
#include <unordered_map>
#include <unordered_set>
#include <variant>

#include "atlas/grid.h"
#include "atlas/library.h"
#include "atlas/parallel/mpi/mpi.h"
#include "eccodes.h"
#include "eckit/config/LocalConfiguration.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/filesystem/PathName.h"
#include "eckit/io/FileHandle.h"
#include "eckit/io/MemoryHandle.h"
#include "eckit/io/PeekHandle.h"
#include "eckit/io/StdFile.h"
#include "eckit/log/Log.h"
#include "eckit/maths/Functions.h"
#include "eckit/message/Decoder.h"
#include "eckit/message/Message.h"
#include "eckit/message/Reader.h"
#include "eckit/option/CmdArgs.h"
#include "eckit/option/SimpleOption.h"
#include "metkit/codes/CodesContent.h"
#include "metkit/codes/CodesHandleDeleter.h"
#include "metkit/codes/CodesSplitter.h"
#include "multio/LibMultio.h"
#include "multio/tools/MultioTool.h"
#include "multiom/api/c/api.h"

namespace multiom {

struct ForeignDictType;

}

template <>
class std::default_delete<multiom::ForeignDictType> {
public:
    void operator()(multiom::ForeignDictType* ptr) const {
        void* p = static_cast<void*>(ptr);
        ASSERT(multio_grib2_dict_destroy(&p) == 0);
    }
};


namespace multiom {

namespace {


// Extract functionality
using Value = std::variant<std::string, long, double>;
using Map = std::unordered_map<std::string, Value>;

class MapSetter : public eckit::message::MetadataGatherer {
    Map& map_;

    void setValue(const std::string& key, const std::string& value) override { map_.insert_or_assign(key, value); }
    void setValue(const std::string& key, long value) override { map_.insert_or_assign(key, value); }
    void setValue(const std::string& key, double value) override { map_.insert_or_assign(key, value); }

public:
    MapSetter(Map& map) : map_(map) {}
};

Map getMarsKeys(const eckit::message::Message& msg) {
    Map map;
    MapSetter setter{map};
    msg.getMetadata(setter, {eckit::message::ValueRepresentation::Native, eckit::Optional<std::string>{"mars"}});
    return map;
}


// API Wrappers
enum class MultiOMDictKind : unsigned long
{
    Options,
    MARS,
    Parametrization,
    // Geometry dicts
    ReducedGG,
    RegularLL,
    SH,
};


std::string multiOMDictKindString(MultiOMDictKind kind) {
    switch (kind) {
        case MultiOMDictKind::Options:
            return "options";
        case MultiOMDictKind::MARS:
            return "mars";
        case MultiOMDictKind::Parametrization:
            return "parametrization";
        case MultiOMDictKind::ReducedGG:
            return "reduced-gg";
        case MultiOMDictKind::RegularLL:
            return "regular-ll";
        case MultiOMDictKind::SH:
            return "sh";
        default:
            NOTIMP;
    }
}

struct MultiOMDict {
    MultiOMDict(MultiOMDictKind kind);
    ~MultiOMDict() = default;

    MultiOMDict(MultiOMDict&&) noexcept = default;
    MultiOMDict& operator=(MultiOMDict&&) noexcept = default;

    void toYAML(const std::string& file = "stdout");

    void set(const char* key, const char* val);
    void set(const std::string& key, const std::string& val);

    // Typed setters
    void set(const std::string& key, std::int64_t val);
    void set(const std::string& key, double val);
    void set(const std::string& key, bool val);
    void set(const std::string& key, const std::int64_t* val, std::size_t len);
    void set(const std::string& key, const double* val, std::size_t len);
    void set(const std::string& key, const std::vector<std::int64_t>& val);
    void set(const std::string& key, const std::vector<double>& val);

    void set(const std::string& key, const Value& val);

    // Set geoemtry on parametrization
    void set_geometry(MultiOMDict&& geom);

    void* get();

    MultiOMDictKind kind_;
    std::unique_ptr<ForeignDictType> dict_;
    std::unique_ptr<MultiOMDict> geom_;
};

MultiOMDict::MultiOMDict(MultiOMDictKind kind) : kind_{kind} {
    std::string kindStr = multiOMDictKindString(kind);
    void* dict = NULL;
    ASSERT(multio_grib2_dict_create(&dict, kindStr.data()) == 0);

    if (kind == MultiOMDictKind::Options) {
        ASSERT(multio_grib2_init_options(&dict) == 0);
    }
    dict_.reset(static_cast<ForeignDictType*>(dict));
}

void MultiOMDict::toYAML(const std::string& file) {
    multio_grib2_dict_to_yaml(get(), file.c_str());
}

void MultiOMDict::set(const char* key, const char* val) {
    if (multio_grib2_dict_set(get(), key, val) != 0) {
        throw std::runtime_error(std::string("Can not set key ") + std::string(key) + std::string(" with value ")
                                 + std::string(val));
    }
}
void MultiOMDict::set(const std::string& key, const std::string& val) {
    set(key.c_str(), val.c_str());
}

// Typed setters
void MultiOMDict::set(const std::string& key, std::int64_t val) {
    if (multio_grib2_dict_set_int64(get(), key.c_str(), val) != 0) {
        throw std::runtime_error(std::string("Can not set key ") + std::string(key) + std::string(" with int64 value ")
                                 + std::to_string(val));
    }
}
void MultiOMDict::set(const std::string& key, double val) {
    if (multio_grib2_dict_set_double(get(), key.c_str(), val) == 0) {
        throw std::runtime_error(std::string("Can not set key ") + std::string(key) + std::string(" with double value ")
                                 + std::to_string(val));
    }
}
void MultiOMDict::set(const std::string& key, bool val) {
    set(key, (std::int64_t)val);
}
void MultiOMDict::set(const std::string& key, const std::int64_t* val, std::size_t len) {
    if (multio_grib2_dict_set_int64_array(get(), key.c_str(), val, len) != 0) {
        throw std::runtime_error(std::string("Can not set key ") + std::string(key) + std::string(" with int64 array"));
    }
}
void MultiOMDict::set(const std::string& key, const double* val, std::size_t len) {
    if (multio_grib2_dict_set_double_array(get(), key.c_str(), val, len) != 0) {
        throw std::runtime_error(std::string("Can not set key ") + std::string(key)
                                 + std::string(" with double array"));
    }
}
void MultiOMDict::set(const std::string& key, const std::vector<std::int64_t>& val) {
    set(key, val.data(), val.size());
}
void MultiOMDict::set(const std::string& key, const std::vector<double>& val) {
    set(key, val.data(), val.size());
}
void MultiOMDict::set(const std::string& key, const Value& val) {
    std::visit([&](const auto& typedVal) { this->set(key, typedVal); }, val);
}

// Set geoemtry on parametrization
void MultiOMDict::set_geometry(MultiOMDict&& geom) {
    ASSERT(kind_ == MultiOMDictKind::Parametrization);
    switch (geom.kind_) {
        case MultiOMDictKind::ReducedGG:
        case MultiOMDictKind::RegularLL:
        case MultiOMDictKind::SH:
            geom_ = std::make_unique<MultiOMDict>(std::move(geom));
            ASSERT(multio_grib2_dict_set_geometry(get(), geom_->get()) == 0);
            // ASSERT(multio_grib2_dict_set_geometry(get(), geom.get()) == 0);
            break;
        default:
            throw std::runtime_error(std::string("Passed dict is not a geometry dict"));
    }
}

void* MultiOMDict::get() {
    return static_cast<void*>(dict_.get());
}


namespace extract {

atlas::Grid readGrid(const std::string& name) {
    atlas::mpi::Scope mpi_scope("self");
    return atlas::Grid{name};
}

template <class GridType>
GridType createGrid(const std::string& atlasNamedGrid) {
    const atlas::Grid grid = readGrid(atlasNamedGrid);
    auto structuredGrid = atlas::StructuredGrid(grid);
    return GridType(structuredGrid);
}


bool hasKey(codes_handle* handle, const char* key) {
    int err;
    return codes_is_defined(handle, key) != 0 && codes_is_missing(handle, key, &err) == 0;
}

std::string getString(codes_handle* handle, const char* key) {
    std::string ret;
    std::size_t keylen = 1024;
    // Use resize instead of reserive - will allocate enough memory and sets the size internally to the string
    ret.resize(keylen);
    // Now eccodes is writing in the buffer...
    CODES_CHECK(codes_get_string(handle, key, ret.data(), &keylen), nullptr);
    ret.resize(strlen(ret.c_str()));
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
void getAndSet(codes_handle* h, MultiOMDict& dict, const char* key, const char* setName = NULL, OptVal defaultVal = {},
               SetDefault defPolicy = SetDefault::IfKeyGiven) {
    if (hasKey(h, key)) {
        std::string val = getString(h, key);
        if (val.empty() && defaultVal) {
            val = *defaultVal;
        }
        dict.set(setName == NULL ? key : setName, val);
    }
    else {
        if (defaultVal && (defPolicy == SetDefault::Always)) {
            dict.set(setName == NULL ? key : setName, *defaultVal);
        }
    }
}
void getAndSet(codes_handle* h, MultiOMDict& dict, const char* key, OptVal defaultVal,
               SetDefault defPolicy = SetDefault::IfKeyGiven) {
    getAndSet(h, dict, key, NULL, defaultVal, defPolicy);
}
void getAndSetIfNonZero(codes_handle* h, MultiOMDict& dict, const char* key, const char* setName = NULL) {
    if (hasKey(h, key)) {
        std::string val = getString(h, key);
        if (!val.empty() && val != "0") {
            dict.set(setName == NULL ? key : setName, val);
        }
    }
}


void getAndSet(const Map& map, MultiOMDict& dict, const char* key, const char* setName = NULL, OptVal defaultVal = {},
               SetDefault defPolicy = SetDefault::IfKeyGiven) {
    if (auto search = map.find(key); search != map.end()) {
        dict.set(setName == NULL ? key : setName, search->second);
    }
    else {
        if (defaultVal && (defPolicy == SetDefault::Always)) {
            dict.set(setName == NULL ? key : setName, *defaultVal);
        }
    }
}
void getAndSet(const Map& map, MultiOMDict& dict, const char* key, OptVal defaultVal,
               SetDefault defPolicy = SetDefault::IfKeyGiven) {
    getAndSet(map, dict, key, NULL, defaultVal, defPolicy);
}


void getAndSetDouble(codes_handle* h, MultiOMDict& dict, const char* key, const char* setName = NULL,
                     OptVal defaultVal = {}, SetDefault defPolicy = SetDefault::IfKeyGiven) {
    if (hasKey(h, key)) {
        std::string val = std::to_string(getDouble(h, key));
        dict.set(setName == NULL ? key : setName, val);
    }
    else {
        if (defaultVal && (defPolicy == SetDefault::Always)) {
            dict.set(setName == NULL ? key : setName, *defaultVal);
        }
    }
}
void getAndSetDouble(codes_handle* h, MultiOMDict& dict, const char* key, OptVal defaultVal,
                     SetDefault defPolicy = SetDefault::IfKeyGiven) {
    getAndSetDouble(h, dict, key, NULL, defaultVal, defPolicy);
}

using OptVal = std::optional<std::string>;
void getAndSetLong(codes_handle* h, MultiOMDict& dict, const char* key, const char* setName = NULL,
                   OptVal defaultVal = {}, SetDefault defPolicy = SetDefault::IfKeyGiven) {
    if (hasKey(h, key)) {
        std::string val = std::to_string(getLong(h, key));
        dict.set(setName == NULL ? key : setName, val);
    }
    else {
        if (defaultVal && (defPolicy == SetDefault::Always)) {
            dict.set(setName == NULL ? key : setName, *defaultVal);
        }
    }
}
void getAndSetLong(codes_handle* h, MultiOMDict& dict, const char* key, OptVal defaultVal,
                   SetDefault defPolicy = SetDefault::IfKeyGiven) {
    getAndSetLong(h, dict, key, NULL, defaultVal, defPolicy);
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
    return oss.str();
}

void getAndSetDoubleArray(codes_handle* h, MultiOMDict& dict, const char* key, const char* setName = NULL) {
    if (hasKey(h, key)) {
        dict.set(setName == NULL ? key : setName, getDoubleArray(h, key));
    }
}

void getAndSetLongArray(codes_handle* h, MultiOMDict& dict, const char* key, const char* setName = NULL) {
    if (hasKey(h, key)) {
        auto jsonData = arrayToJSONString(getLongArray(h, key));
        dict.set(setName == NULL ? key : setName, jsonData.data());
    }
}

using GridTypeFunction = std::function<void(codes_handle*, MultiOMDict&, MultiOMDict&)>;

void handleReducedGG(codes_handle* h, MultiOMDict& mars_dict, MultiOMDict& par_dict) {
    MultiOMDict geom{MultiOMDictKind::ReducedGG};

    getAndSet(h, geom, "truncateDegrees", "truncate-degrees");
    getAndSetIfNonZero(h, geom, "numberOfPointsAlongAMeridian", "number-of-points-along-a-meridian");
    getAndSetIfNonZero(h, geom, "numberOfParallelsBetweenAPoleAndTheEquator",
                       "number-of-parallels-between-pole-and-equator");
    getAndSetDouble(h, geom, "latitudeOfFirstGridPointInDegrees", "latitude-of-first-grid-point-in-degrees");
    getAndSetDouble(h, geom, "longitudeOfFirstGridPointInDegrees", "longitude-of-first-grid-point-in-degrees");
    getAndSetDouble(h, geom, "latitudeOfLastGridPointInDegrees", "latitude-of-last-grid-point-in-degrees");
    getAndSetDouble(h, geom, "longitudeOfLastGridPointInDegrees", "longitude-of-last-grid-point-in-degrees");
    getAndSetLongArray(h, geom, "pl", "pl");
    mars_dict.set("repres", "gg");
    par_dict.set_geometry(std::move(geom));
}

void handleReducedGGAtlas(codes_handle* h, MultiOMDict& mars_dict, MultiOMDict& par_dict) {
    MultiOMDict geom{MultiOMDictKind::ReducedGG};

    std::string gridName = getString(h, "gridName");
    const auto gaussianGrid = createGrid<atlas::GaussianGrid>(gridName);

    getAndSet(h, geom, "truncateDegrees", "truncate-degrees");
    geom.set("numberOfParallelsBetweenAPoleAndTheEquator", std::to_string(gaussianGrid.N()).c_str());
    getAndSetIfNonZero(h, geom, "numberOfPointsAlongAMeridian", "number-of-points-along-a-meridian");

    {
        auto it = gaussianGrid.lonlat().begin();

        geom.set("latitudeOfFirstGridPointInDegrees", std::to_string((*it)[1]).data());
        geom.set("longitudeOfFirstGridPointInDegrees", std::to_string((*it)[0]).data());

        it += gaussianGrid.size() - 1;
        geom.set("latitudeOfLastGridPointInDegrees", std::to_string((*it)[1]).data());

        const auto equator = gaussianGrid.N();
        const auto maxLongitude = gaussianGrid.x(gaussianGrid.nx(equator) - 1, equator);
        geom.set("longitudeOfLastGridPointInDegrees", std::to_string(maxLongitude).data());
    }

    {
        auto tmp = gaussianGrid.nx();
        std::vector<long> pl(tmp.size(), 0);
        for (int i = 0; i < tmp.size(); ++i) {
            pl[i] = long(tmp[i]);
        }
        geom.set("pl", arrayToJSONString(pl).data());
    }

    mars_dict.set("repres", "gg");
    par_dict.set_geometry(std::move(geom));
}

void handleSH(codes_handle* h, MultiOMDict& mars_dict, MultiOMDict& par_dict) {
    MultiOMDict geom{MultiOMDictKind::SH};

    geom.set("pentagonalResolutionParameterJ", "pentagonal-resolution-j");
    geom.set("pentagonalResolutionParameterK", "pentagonal-resolution-k");
    geom.set("pentagonalResolutionParameterM", "pentagonal-resolution-m");
    mars_dict.set("repres", "sh");

    par_dict.set_geometry(std::move(geom));
}

void handleLL(codes_handle* h, MultiOMDict& mars_dict, MultiOMDict& par_dict) {
    MultiOMDict geom{MultiOMDictKind::RegularLL};

    mars_dict.set("repres", "ll");

    par_dict.set_geometry(std::move(geom));
}

void handleGridType(codes_handle* h, const std::string& gridType, MultiOMDict& mars_dict, MultiOMDict& par_dict) {
    const static std::unordered_map<std::string, GridTypeFunction> gridMap{
        {"reduced_gg", &handleReducedGGAtlas},
        {"regular_ll", &handleLL},
        {"sh", &handleSH},
    };

    const auto gridTypeFunc = gridMap.find(gridType);
    if (gridTypeFunc == gridMap.cend()) {
        throw std::runtime_error(std::string("Unhandled gridType '") + gridType + std::string("'"));
    }
    gridTypeFunc->second(h, mars_dict, par_dict);
};

void handlePackingType(codes_handle* h, const std::string& packingType, MultiOMDict& mars_dict) {
    const static std::unordered_map<std::string, std::string> packingMap{
        {"grid_simple", "simple"},
        {"grid_complex", "complex"},
        {"spectral_complex", "complex"},
        {"grid_ccsds", "ccsds"},
    };

    const auto packingTypeVal = packingMap.find(packingType);
    if (packingTypeVal == packingMap.cend()) {
        throw std::runtime_error(std::string("Unhandled packingType '") + packingType + std::string("'"));
    }
    mars_dict.set("packing", packingTypeVal->second.c_str());
};


void grib1ToGrib2(Map& marsKeys, codes_handle* h, MultiOMDict& marsDict, MultiOMDict& parDict) {
    getAndSet(marsKeys, marsDict, "stream");
    getAndSet(marsKeys, marsDict, "type");
    getAndSet(marsKeys, marsDict, "class");
    getAndSet(marsKeys, marsDict, "expver");

    if (hasKey(h, "origin")) {
        getAndSet(h, marsDict, "origin");
    }
    else if (hasKey(h, "centre")) {
        getAndSet(h, marsDict, "centre", "origin");
    }

    getAndSet(marsKeys, marsDict, "anoffset");
    getAndSet(marsKeys, marsDict, "ident");
    getAndSet(marsKeys, marsDict, "instrument");
    getAndSet(marsKeys, marsDict, "channel");
    // getAndSet(marsKeys, marsDict, "chemId", "chem");
    getAndSet(marsKeys, marsDict, "chem");
    getAndSet(marsKeys, marsDict, "param");
    getAndSet(marsKeys, marsDict, "model");

    if (hasKey(h, "levtype")) {
        std::string levtype = getString(h, "levtype");
        marsDict.set("levtype", levtype);

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

            marsDict.set("levelist", *levelist);
        }
    }

    getAndSet(marsKeys, marsDict, "direction");
    getAndSet(marsKeys, marsDict, "frequency");
    getAndSet(marsKeys, marsDict, "date");
    getAndSet(marsKeys, marsDict, "hdate");

    // Handle time explicitly - generate a HHMMSS representation istead of dafult HHMM representation
    {
        // TODO - this will cause problems with referenceDate/time in grib2....
        long hh = getLong(h, "hour");
        long mm = getLong(h, "minute");
        long ss = getLong(h, "second");
        marsDict.set("time", std::to_string(hh * 10000 + mm * 100 + ss));
    }

    // For some reason mars returns an empty string for step
    if (hasKey(h, "endStep")) {
        long endStep = getLong(h, "endStep");
        long startStep = getLong(h, "startStep");
        marsDict.set("step", std::to_string(endStep));

        long stepRange = endStep - startStep;
        if (stepRange > 0) {
            // TODO to be renamed to timespan
            marsDict.set("timeproc", std::to_string(stepRange));
        }
    }
    getAndSet(marsKeys, marsDict, "truncation");

    getAndSet(h, marsDict, "gridName", "grid");

    getAndSet(h, parDict, "tablesVersion");
    getAndSet(h, parDict, "generatingProcessIdentifier");
    getAndSetLong(h, parDict, "typeOfProcessedData");
    getAndSet(h, parDict, "initialStep", OptVal{"0"}, SetDefault::Always);
    getAndSet(h, parDict, "timeIncrement", "lengthOfTimeStepInSeconds", OptVal{"3600"}, SetDefault::Always);
    // getAndSet(h, parDict, "lengthOfTimeRangeInSeconds", OptVal{"3600"}, SetDefault::IfKeyGiven);
    getAndSet(h, parDict, "valuesScaleFactor");
    getAndSetDoubleArray(h, parDict, "pv", "pv");
    getAndSetIfNonZero(h, parDict, "numberOfMissingValues");
    getAndSet(h, parDict, "valueOfMissingValues");
    getAndSet(h, parDict, "systemNumber");
    getAndSet(h, parDict, "methodNumber");

    if (marsKeys.find("number") != marsKeys.end()) {
        getAndSet(marsKeys, marsDict, "number");
        if (hasKey(h, "typeOfEnsembleForecast")) {
            getAndSet(h, parDict, "typeOfEnsembleForecast");
        }
        else if (hasKey(h, "eps")) {
            getAndSet(h, parDict, "eps", "typeOfEnsembleForecast");
        }

        if (!hasKey(h, "numberOfForecastsInEnsemble")) {
            throw std::runtime_error("Expected a key numberOfForecastsInEnsemble");
        }
        long numForecasts = getLong(h, "numberOfForecastsInEnsemble");
        if (numForecasts == 0) {
            throw std::runtime_error("The value for key numberOfForecastsInEnsemble must not be 0");
        }
        parDict.set("numberOfForecastsInEnsemble", std::to_string(numForecasts));
    }


    getAndSet(h, parDict, "lengthOfTimeWindow");
    getAndSet(h, parDict, "bitsPerValue", OptVal{"24"});
    getAndSet(h, parDict, "periodMin");
    getAndSet(h, parDict, "periodMax");
    getAndSetLongArray(h, parDict, "scaledValuesOfWaveDirections", "waveDirections");
    getAndSetLongArray(h, parDict, "scaledValuesOfWaveFrequencies", "waveFrequencies");
    getAndSet(h, parDict, "satelliteSeries");
    getAndSet(h, parDict, "scaledFactorOfCentralWavenumber");
    getAndSet(h, parDict, "scaledValueOfCentralWavenumber");

    // TODO - this should be only set to 1 for statistical fields with step 0
    parDict.set("encodeStepZero", "1");

    if (hasKey(h, "setPackingType")) {
        std::string setPackingType = getString(h, "setPackingType");
        handlePackingType(h, setPackingType, marsDict);
    }

    if (hasKey(h, "gridType")) {
        std::string gridType = getString(h, "gridType");
        handleGridType(h, gridType, marsDict, parDict);
    }
}

void postFixToolOnly(codes_handle* in, codes_handle* out) {
    if (hasKey(in, "gridType")) {
        std::string gridType = getString(in, "gridType");
        if (gridType == "reduced_gg") {
            long shapeOfTheEarth = getLong(in, "shapeOfTheEarth");
            CODES_CHECK(codes_set_long(out, "shapeOfTheEarth", shapeOfTheEarth), nullptr);
        }
    }
}

}  // namespace extract


// Helpers

bool isDiscipline192Param(long p) {
    // Fetched via rest API of paramDb
    // https://codes.ecmwf.int/parameter-database/api/v1/param/?discipline=192&encoding=grib2&limit=9000&offset=0&regex=false
    static const std::unordered_set<long> set{
        4,      5,      6,      7,      11,     12,     13,     14,     19,     24,     25,     35,     36,     37,
        38,     39,     40,     41,     42,     46,     62,     63,     64,     65,     70,     71,     80,     81,
        82,     83,     84,     85,     86,     87,     88,     89,     90,     91,     92,     93,     94,     95,
        96,     97,     98,     99,     100,    101,    102,    103,    104,    105,    106,    107,    108,    109,
        110,    111,    112,    113,    114,    115,    116,    117,    118,    119,    120,    123,    125,    126,
        127,    128,    139,    140,    153,    154,    158,    170,    171,    183,    184,    185,    190,    191,
        192,    193,    199,    200,    204,    214,    215,    216,    217,    218,    219,    220,    221,    222,
        223,    224,    225,    226,    227,    233,    236,    237,    241,    242,    249,    250,    251,    252,
        253,    254,    129001, 129002, 129003, 129004, 129005, 129011, 129012, 129013, 129014, 129021, 129022, 129023,
        129024, 129025, 129026, 129027, 129028, 129029, 129030, 129031, 129032, 129033, 129034, 129035, 129036, 129037,
        129038, 129039, 129040, 129041, 129042, 129043, 129044, 129045, 129046, 129047, 129048, 129049, 129050, 129051,
        129052, 129053, 129054, 129057, 129058, 129059, 129060, 129061, 129062, 129063, 129064, 129065, 129066, 129067,
        129070, 129071, 129078, 129079, 129080, 129081, 129082, 129083, 129084, 129085, 129086, 129087, 129088, 129089,
        129090, 129091, 129092, 129093, 129094, 129095, 129096, 129097, 129098, 129099, 129100, 129101, 129102, 129103,
        129104, 129105, 129106, 129107, 129108, 129109, 129110, 129111, 129112, 129113, 129114, 129115, 129116, 129117,
        129118, 129119, 129120, 129121, 129122, 129123, 129125, 129126, 129127, 129128, 129129, 129130, 129131, 129132,
        129133, 129134, 129135, 129136, 129137, 129138, 129139, 129140, 129141, 129142, 129143, 129144, 129145, 129146,
        129147, 129148, 129149, 129150, 129151, 129152, 129153, 129154, 129155, 129156, 129157, 129158, 129159, 129160,
        129161, 129162, 129163, 129164, 129165, 129166, 129167, 129168, 129169, 129170, 129171, 129172, 129173, 129174,
        129175, 129176, 129177, 129178, 129179, 129180, 129181, 129182, 129183, 129184, 129185, 129186, 129187, 129188,
        129189, 129190, 129191, 129192, 129193, 129194, 129195, 129196, 129197, 129198, 129199, 129200, 129201, 129202,
        129203, 129204, 129205, 129206, 129207, 129208, 129209, 129210, 129211, 129212, 129214, 129215, 129216, 129217,
        129218, 129219, 129220, 129221, 129222, 129223, 129224, 129225, 129226, 129227, 129228, 129229, 129230, 129231,
        129232, 129233, 129234, 129235, 129236, 129237, 129238, 129239, 129240, 129241, 129242, 129243, 129244, 129245,
        129246, 129247, 129248, 129249, 129250, 129251, 129252, 129253, 129254, 129255, 130208, 130209, 130210, 130211,
        130212, 130213, 130214, 130215, 130216, 130217, 130218, 130219, 130220, 130221, 130224, 130225, 130226, 130228,
        130229, 130230, 130231, 131001, 131002, 131003, 131004, 131005, 131006, 131007, 131008, 131009, 131010, 131015,
        131016, 131017, 131018, 131020, 131021, 131022, 131023, 131024, 131025, 131049, 131059, 131064, 131065, 131066,
        131067, 131068, 131069, 131073, 131078, 131079, 131080, 131081, 131129, 131130, 131139, 131144, 131151, 131164,
        131165, 131167, 131201, 131202, 131228, 131229, 131232, 131255, 133001, 133002, 133003, 133004, 133005, 133006,
        133007, 133008, 133009, 133010, 133011, 133012, 133013, 133014, 133015, 133016, 133017, 133018, 133019, 133020,
        133021, 133022, 133023, 133024, 133025, 133026, 133027, 133028, 133029, 133030, 133031, 133032, 133033, 133034,
        133035, 133036, 133037, 133038, 133039, 133040, 133041, 133042, 133043, 133044, 133045, 133046, 133047, 133048,
        133049, 133050, 133051, 133052, 133053, 133054, 133055, 133056, 133057, 133058, 133059, 133060, 133061, 133062,
        133063, 133064, 133065, 133066, 133067, 133068, 133069, 133070, 133071, 133072, 133073, 133074, 133075, 133076,
        133077, 133078, 133079, 133080, 133081, 133082, 133083, 133084, 133085, 133086, 133087, 133088, 133089, 133090,
        133091, 133092, 140200, 140250, 140255, 150129, 150130, 150131, 150133, 150134, 150135, 150137, 150139, 150140,
        150141, 150142, 150143, 150144, 150145, 150146, 150147, 150148, 150152, 150153, 150154, 150155, 150168, 150169,
        150170, 150171, 150172, 150173, 150180, 150181, 150182, 150183, 150255, 151128, 151129, 151130, 151133, 151134,
        151135, 151136, 151137, 151138, 151139, 151140, 151141, 151142, 151143, 151144, 151146, 151147, 151148, 151150,
        151151, 151152, 151153, 151154, 151155, 151156, 151157, 151158, 151159, 151160, 151161, 151162, 151164, 151165,
        151166, 151167, 151168, 151169, 151170, 151176, 151177, 151178, 151179, 151180, 151181, 151182, 151183, 151184,
        151185, 151186, 151187, 151188, 151190, 151191, 151192, 151193, 151194, 151199, 151200, 151201, 151202, 151203,
        151204, 151205, 151206, 151207, 151208, 151209, 151210, 151211, 151212, 151255, 160049, 160135, 160137, 160140,
        160142, 160143, 160144, 160156, 160157, 160171, 160180, 160181, 160182, 160184, 160199, 160205, 160207, 160209,
        160210, 160212, 160213, 160214, 160216, 160217, 160218, 160219, 160221, 160222, 160223, 160224, 160225, 160231,
        160239, 160240, 160241, 160242, 160243, 160246, 160247, 160249, 160254, 162051, 162054, 162056, 162057, 162058,
        162064, 162065, 162066, 162067, 162068, 162073, 162074, 162075, 162076, 162077, 162078, 162081, 162082, 162083,
        162085, 162086, 162087, 162114, 162115, 162116, 162117, 162118, 162119, 162120, 162121, 162122, 162123, 162124,
        162125, 162126, 162127, 162128, 162129, 162130, 162131, 162132, 162133, 162134, 162135, 162136, 162137, 162138,
        162139, 162140, 162141, 162206, 162207, 162208, 162209, 162210, 162211, 162212, 162213, 162214, 162215, 162216,
        162217, 162218, 162219, 162220, 162221, 162222, 162223, 162224, 162225, 162226, 162227, 162229, 162230, 162231,
        162232, 162233, 162255, 170149, 170171, 170179, 171001, 171002, 171003, 171004, 171005, 171006, 171007, 171011,
        171012, 171013, 171014, 171021, 171022, 171023, 171026, 171027, 171028, 171029, 171030, 171031, 171032, 171033,
        171034, 171035, 171036, 171037, 171038, 171039, 171040, 171041, 171042, 171043, 171044, 171045, 171046, 171047,
        171048, 171049, 171050, 171051, 171052, 171053, 171054, 171055, 171056, 171057, 171058, 171059, 171060, 171061,
        171062, 171063, 171064, 171065, 171078, 171079, 171121, 171122, 171125, 171126, 171127, 171128, 171129, 171130,
        171131, 171132, 171133, 171134, 171135, 171136, 171137, 171138, 171139, 171140, 171141, 171142, 171143, 171144,
        171145, 171146, 171147, 171148, 171149, 171150, 171151, 171152, 171153, 171154, 171155, 171156, 171157, 171158,
        171159, 171160, 171161, 171162, 171163, 171164, 171165, 171166, 171167, 171168, 171169, 171170, 171171, 171173,
        171174, 171175, 171176, 171177, 171178, 171179, 171180, 171181, 171182, 171183, 171184, 171185, 171186, 171187,
        171188, 171189, 171190, 171191, 171192, 171193, 171194, 171195, 171196, 171197, 171198, 171199, 171200, 171201,
        171202, 171203, 171204, 171205, 171206, 171207, 171208, 171209, 171210, 171211, 171212, 171214, 171215, 171216,
        171217, 171218, 171219, 171220, 171221, 171222, 171223, 171224, 171225, 171226, 171227, 171228, 171229, 171230,
        171231, 171232, 171233, 171234, 171235, 171236, 171237, 171238, 171239, 171240, 171241, 171242, 171243, 171244,
        171245, 171246, 171247, 171248, 171249, 171250, 171251, 171252, 171253, 171254, 171255, 172044, 172045, 172153,
        172154, 172239, 172240, 172255, 173044, 173045, 173048, 173050, 173142, 173143, 173144, 173145, 173146, 173147,
        173149, 173153, 173154, 173169, 173175, 173176, 173177, 173178, 173179, 173180, 173181, 173182, 173189, 173195,
        173196, 173197, 173205, 173208, 173209, 173210, 173211, 173212, 173228, 173239, 173240, 173255, 174006, 174031,
        174034, 174039, 174040, 174041, 174042, 174049, 174055, 174083, 174085, 174086, 174087, 174088, 174089, 174090,
        174094, 174095, 174099, 174110, 174111, 174139, 174164, 174167, 174168, 174170, 174175, 174183, 174236, 174255,
        175006, 175031, 175034, 175039, 175040, 175041, 175042, 175049, 175055, 175083, 175085, 175086, 175087, 175088,
        175089, 175090, 175110, 175111, 175139, 175164, 175167, 175168, 175170, 175175, 175183, 175236, 175255, 180149,
        180176, 180177, 180178, 180179, 190170, 190171, 190173, 190229, 200001, 200002, 200003, 200004, 200005, 200011,
        200012, 200013, 200014, 200021, 200022, 200023, 200024, 200025, 200026, 200027, 200028, 200029, 200030, 200031,
        200032, 200033, 200034, 200035, 200036, 200037, 200038, 200039, 200040, 200041, 200042, 200043, 200044, 200045,
        200046, 200047, 200048, 200049, 200050, 200051, 200052, 200053, 200054, 200055, 200056, 200057, 200058, 200059,
        200060, 200061, 200062, 200063, 200064, 200065, 200066, 200067, 200070, 200071, 200078, 200079, 200080, 200081,
        200082, 200083, 200084, 200085, 200086, 200087, 200088, 200089, 200090, 200091, 200092, 200093, 200094, 200095,
        200096, 200097, 200098, 200099, 200100, 200101, 200102, 200103, 200104, 200105, 200106, 200107, 200108, 200109,
        200110, 200111, 200112, 200113, 200114, 200115, 200116, 200117, 200118, 200119, 200120, 200121, 200122, 200123,
        200125, 200126, 200127, 200128, 200129, 200130, 200131, 200132, 200133, 200134, 200135, 200136, 200137, 200138,
        200139, 200140, 200141, 200142, 200143, 200144, 200145, 200146, 200147, 200148, 200149, 200150, 200151, 200152,
        200153, 200154, 200155, 200156, 200157, 200158, 200159, 200160, 200161, 200162, 200163, 200164, 200165, 200166,
        200167, 200168, 200169, 200170, 200171, 200172, 200173, 200174, 200175, 200176, 200177, 200178, 200179, 200180,
        200181, 200182, 200183, 200184, 200185, 200186, 200187, 200188, 200189, 200190, 200191, 200192, 200193, 200194,
        200195, 200196, 200197, 200198, 200199, 200200, 200201, 200202, 200203, 200204, 200205, 200206, 200207, 200208,
        200209, 200210, 200211, 200212, 200214, 200215, 200216, 200217, 200218, 200219, 200220, 200221, 200222, 200223,
        200224, 200225, 200226, 200227, 200228, 200229, 200230, 200231, 200232, 200233, 200234, 200235, 200236, 200237,
        200238, 200239, 200240, 200241, 200242, 200243, 200244, 200245, 200246, 200247, 200248, 200249, 200250, 200251,
        200252, 200253, 200254, 200255, 201001, 201002, 201003, 201004, 201005, 201006, 201007, 201008, 201009, 201010,
        201011, 201012, 201013, 201014, 201015, 201016, 201017, 201029, 201030, 201031, 201032, 201033, 201034, 201035,
        201036, 201037, 201038, 201041, 201042, 201050, 201051, 201052, 201053, 201054, 201055, 201056, 201060, 201061,
        201062, 201063, 201064, 201065, 201066, 201067, 201068, 201069, 201070, 201071, 201072, 201073, 201074, 201075,
        201076, 201077, 201078, 201079, 201080, 201081, 201082, 201083, 201084, 201085, 201099, 201100, 201101, 201102,
        201111, 201112, 201113, 201150, 201200, 201203, 201215, 201241, 201255, 210001, 210002, 210003, 210004, 210005,
        210006, 210007, 210008, 210009, 210010, 210011, 210012, 210013, 210014, 210015, 210016, 210017, 210018, 210019,
        210020, 210021, 210022, 210023, 210024, 210025, 210026, 210027, 210028, 210029, 210030, 210031, 210032, 210033,
        210034, 210035, 210036, 210037, 210038, 210039, 210040, 210041, 210042, 210043, 210044, 210045, 210046, 210047,
        210048, 210049, 210050, 210051, 210052, 210053, 210054, 210055, 210056, 210057, 210058, 210059, 210060, 210061,
        210062, 210063, 210064, 210065, 210066, 210067, 210068, 210069, 210070, 210071, 210079, 210080, 210081, 210082,
        210083, 210084, 210085, 210086, 210087, 210088, 210089, 210090, 210091, 210092, 210093, 210094, 210095, 210096,
        210097, 210098, 210099, 210100, 210101, 210102, 210103, 210104, 210105, 210106, 210107, 210108, 210109, 210110,
        210111, 210112, 210113, 210114, 210115, 210116, 210117, 210118, 210119, 210120, 210124, 210125, 210126, 210127,
        210128, 210129, 210130, 210131, 210132, 210133, 210134, 210135, 210136, 210137, 210138, 210139, 210140, 210141,
        210142, 210143, 210144, 210145, 210146, 210147, 210148, 210149, 210150, 210151, 210152, 210153, 210154, 210155,
        210156, 210157, 210158, 210159, 210160, 210161, 210162, 210163, 210164, 210165, 210166, 210167, 210169, 210177,
        210179, 210181, 210182, 210183, 210184, 210185, 210206, 210207, 210208, 210209, 210210, 210211, 210212, 210213,
        210214, 210215, 210216, 210217, 210218, 210219, 210220, 210221, 210222, 210223, 210224, 210225, 210226, 210227,
        210228, 210229, 210230, 210231, 210232, 210233, 210234, 210235, 210236, 210237, 210238, 210239, 210240, 210241,
        210242, 210243, 210244, 210245, 210246, 211001, 211002, 211003, 211004, 211005, 211006, 211007, 211008, 211009,
        211010, 211011, 211012, 211013, 211014, 211015, 211016, 211017, 211018, 211019, 211020, 211021, 211022, 211023,
        211024, 211025, 211026, 211027, 211028, 211029, 211030, 211031, 211032, 211033, 211034, 211035, 211036, 211037,
        211038, 211039, 211040, 211041, 211042, 211043, 211044, 211045, 211046, 211047, 211048, 211049, 211050, 211051,
        211052, 211053, 211054, 211055, 211056, 211061, 211062, 211063, 211064, 211065, 211066, 211067, 211068, 211069,
        211070, 211071, 211092, 211093, 211094, 211095, 211096, 211097, 211098, 211099, 211100, 211101, 211119, 211120,
        211124, 211125, 211126, 211127, 211128, 211129, 211130, 211131, 211132, 211133, 211134, 211135, 211136, 211137,
        211138, 211139, 211140, 211141, 211142, 211143, 211144, 211145, 211146, 211147, 211148, 211149, 211150, 211151,
        211152, 211153, 211154, 211155, 211156, 211157, 211158, 211159, 211160, 211161, 211162, 211163, 211164, 211165,
        211166, 211181, 211182, 211183, 211184, 211185, 211206, 211207, 211208, 211209, 211210, 211211, 211212, 211213,
        211214, 211215, 211216, 212001, 212002, 212003, 212004, 212005, 212006, 212007, 212008, 212009, 212010, 212011,
        212012, 212013, 212014, 212015, 212016, 212017, 212018, 212019, 212020, 212021, 212022, 212023, 212024, 212025,
        212026, 212027, 212028, 212029, 212030, 212031, 212032, 212033, 212034, 212035, 212036, 212037, 212038, 212039,
        212040, 212041, 212042, 212043, 212044, 212045, 212046, 212047, 212048, 212049, 212050, 212051, 212052, 212053,
        212054, 212055, 212056, 212057, 212058, 212059, 212060, 212061, 212062, 212063, 212064, 212065, 212066, 212067,
        212068, 212069, 212070, 212071, 212072, 212073, 212074, 212075, 212076, 212077, 212078, 212079, 212080, 212081,
        212082, 212083, 212084, 212085, 212086, 212087, 212088, 212089, 212090, 212091, 212092, 212093, 212094, 212095,
        212096, 212097, 212098, 212099, 212100, 212101, 212102, 212103, 212104, 212105, 212106, 212107, 212108, 212109,
        212110, 212111, 212112, 212113, 212114, 212115, 212116, 212117, 212118, 212119, 212120, 212121, 212122, 212123,
        212124, 212125, 212126, 212127, 212128, 212129, 212130, 212131, 212132, 212133, 212134, 212135, 212136, 212137,
        212138, 212139, 212140, 212141, 212142, 212143, 212144, 212145, 212146, 212147, 212148, 212149, 212150, 212151,
        212152, 212153, 212154, 212155, 212156, 212157, 212158, 212159, 212160, 212161, 212162, 212163, 212164, 212165,
        212166, 212167, 212168, 212169, 212170, 212171, 212172, 212173, 212174, 212175, 212176, 212177, 212178, 212179,
        212180, 212181, 212182, 212183, 212184, 212185, 212186, 212187, 212188, 212189, 212190, 212191, 212192, 212193,
        212194, 212195, 212196, 212197, 212198, 212199, 212200, 212201, 212202, 212203, 212204, 212205, 212206, 212207,
        212208, 212209, 212210, 212211, 212212, 212213, 212214, 212215, 212216, 212217, 212218, 212219, 212220, 212221,
        212222, 212223, 212224, 212225, 212226, 212227, 212228, 212229, 212230, 212231, 212232, 212233, 212234, 212235,
        212236, 212237, 212238, 212239, 212240, 212241, 212242, 212243, 212244, 212245, 212246, 212247, 212248, 212249,
        212250, 212251, 212252, 212253, 212254, 212255, 213001, 213002, 213003, 213004, 213005, 213101, 213102, 213103,
        213104, 213105, 213106, 213107, 213108, 213109, 213110, 213111, 213112, 213113, 213114, 213115, 213116, 213117,
        213118, 213119, 213120, 213121, 213122, 213123, 213124, 213125, 213126, 213127, 213128, 213129, 213130, 213131,
        213132, 213133, 213134, 213135, 213136, 213137, 213138, 213139, 213140, 213141, 213142, 213143, 213144, 213145,
        213146, 213147, 213148, 213149, 213150, 213151, 213152, 213153, 213154, 213155, 213156, 213157, 213158, 213159,
        213160, 213161, 213162, 213163, 213164, 213165, 213166, 213167, 213168, 213169, 213170, 213171, 213172, 213173,
        213174, 213175, 213176, 213177, 213178, 213179, 213180, 213181, 213182, 213183, 213184, 213185, 213186, 213187,
        213188, 213189, 213190, 213191, 213192, 213193, 213194, 213195, 213196, 213197, 213198, 213199, 213200, 213201,
        213202, 213203, 213204, 213205, 213206, 213207, 213208, 213209, 213210, 213211, 213212, 213213, 213214, 213215,
        213216, 213217, 213218, 213219, 213220, 213221, 214002, 214003, 214004, 214005, 214006, 214007, 214008, 214009,
        214010, 214011, 214012, 214013, 214014, 214015, 214016, 214017, 214018, 214019, 214020, 214021, 214022, 214023,
        214024, 214025, 214026, 214027, 214028, 214029, 214030, 214031, 214032, 214033, 214034, 214035, 214036, 214037,
        214038, 214039, 214040, 214041, 214042, 214043, 214044, 214045, 214046, 214047, 214048, 214049, 214050, 214051,
        214052, 215001, 215002, 215003, 215004, 215005, 215006, 215007, 215008, 215009, 215010, 215011, 215012, 215013,
        215014, 215015, 215016, 215017, 215018, 215019, 215020, 215021, 215022, 215023, 215024, 215025, 215026, 215027,
        215028, 215029, 215030, 215031, 215032, 215033, 215034, 215035, 215036, 215037, 215038, 215039, 215040, 215041,
        215042, 215043, 215044, 215045, 215046, 215047, 215048, 215049, 215050, 215051, 215052, 215053, 215054, 215055,
        215056, 215057, 215058, 215059, 215060, 215061, 215062, 215063, 215064, 215065, 215066, 215067, 215068, 215069,
        215070, 215071, 215072, 215073, 215074, 215075, 215076, 215077, 215078, 215079, 215080, 215081, 215082, 215083,
        215084, 215085, 215086, 215087, 215088, 215089, 215090, 215091, 215092, 215093, 215094, 215095, 215096, 215097,
        215098, 215099, 215100, 215101, 215102, 215103, 215104, 215105, 215106, 215107, 215108, 215109, 215110, 215111,
        215112, 215113, 215114, 215115, 215116, 215117, 215118, 215119, 215120, 215121, 215122, 215123, 215124, 215125,
        215126, 215127, 215128, 215129, 215130, 215131, 215132, 215133, 215134, 215135, 215136, 215137, 215138, 215139,
        215140, 215141, 215142, 215143, 215144, 215145, 215146, 215147, 215148, 215149, 215150, 215151, 215152, 215153,
        215154, 215155, 215156, 215157, 215158, 215159, 215160, 215161, 215162, 215163, 215164, 215165, 215166, 215167,
        215168, 215169, 215170, 215171, 215172, 215173, 215174, 215175, 215176, 215177, 215178, 215179, 215180, 215181,
        215182, 215183, 215184, 215185, 215186, 215187, 215188, 216001, 216002, 216003, 216004, 216005, 216006, 216007,
        216008, 216009, 216010, 216011, 216012, 216013, 216014, 216015, 216016, 216017, 216018, 216019, 216020, 216021,
        216022, 216023, 216024, 216025, 216026, 216027, 216028, 216029, 216030, 216031, 216032, 216033, 216034, 216035,
        216036, 216037, 216038, 216039, 216040, 216041, 216042, 216043, 216044, 216045, 216046, 216047, 216048, 216049,
        216050, 216051, 216052, 216053, 216054, 216055, 216056, 216057, 216058, 216059, 216060, 216061, 216062, 216063,
        216064, 216065, 216066, 216067, 216068, 216069, 216070, 216071, 216072, 216073, 216074, 216075, 216076, 216077,
        216078, 216079, 216080, 216081, 216082, 216083, 216084, 216085, 216086, 216087, 216088, 216089, 216090, 216091,
        216092, 216093, 216094, 216095, 216096, 216097, 216098, 216099, 216100, 216101, 216102, 216103, 216104, 216105,
        216106, 216107, 216108, 216109, 216110, 216111, 216112, 216113, 216114, 216115, 216116, 216117, 216118, 216119,
        216120, 216121, 216122, 216123, 216124, 216125, 216126, 216127, 216128, 216129, 216130, 216131, 216132, 216133,
        216134, 216135, 216136, 216137, 216138, 216139, 216140, 216141, 216142, 216143, 216144, 216145, 216146, 216147,
        216148, 216149, 216150, 216151, 216152, 216153, 216154, 216155, 216156, 216157, 216158, 216159, 216160, 216161,
        216162, 216163, 216164, 216165, 216166, 216167, 216168, 216169, 216170, 216171, 216172, 216173, 216174, 216175,
        216176, 216177, 216178, 216179, 216180, 216181, 216182, 216183, 216184, 216185, 216186, 216187, 216188, 216189,
        216190, 216191, 216192, 216193, 216194, 216195, 216196, 216197, 216198, 216199, 216200, 216201, 216202, 216203,
        216204, 216205, 216206, 216207, 216208, 216209, 216210, 216211, 216212, 216213, 216214, 216215, 216216, 216217,
        216218, 216219, 216220, 216221, 216222, 216223, 216224, 216225, 216226, 216227, 216228, 216229, 216230, 216231,
        216232, 216233, 216234, 216235, 216236, 216237, 216238, 216239, 216240, 216241, 216242, 216243, 216244, 216245,
        216246, 216247, 216248, 216249, 216250, 216251, 216252, 216253, 216254, 216255, 217003, 217004, 217006, 217007,
        217009, 217010, 217011, 217012, 217013, 217014, 217015, 217016, 217018, 217020, 217021, 217022, 217023, 217024,
        217026, 217028, 217029, 217030, 217032, 217033, 217034, 217035, 217036, 217037, 217038, 217039, 217040, 217041,
        217042, 217043, 217044, 217045, 217046, 217047, 217048, 217049, 217050, 217051, 217052, 217053, 217054, 217055,
        217056, 217057, 217058, 217059, 217060, 217061, 217062, 217063, 217064, 217065, 217066, 217067, 217068, 217069,
        217070, 217071, 217072, 217073, 217074, 217075, 217076, 217077, 217078, 217079, 217080, 217081, 217082, 217083,
        217084, 217085, 217086, 217087, 217088, 217089, 217090, 217091, 217092, 217093, 217094, 217095, 217096, 217097,
        217098, 217099, 217100, 217101, 217102, 217103, 217104, 217105, 217106, 217107, 217108, 217109, 217110, 217111,
        217112, 217113, 217114, 217115, 217116, 217117, 217118, 217119, 217120, 217121, 217122, 217123, 217124, 217125,
        217126, 217127, 217128, 217129, 217130, 217131, 217132, 217133, 217134, 217135, 217136, 217137, 217138, 217139,
        217140, 217141, 217142, 217143, 217144, 217145, 217146, 217147, 217148, 217149, 217150, 217151, 217152, 217153,
        217154, 217155, 217156, 217157, 217158, 217159, 217160, 217161, 217162, 217163, 217164, 217165, 217166, 217167,
        217168, 217169, 217170, 217171, 217172, 217173, 217174, 217175, 217176, 217177, 217178, 217179, 217180, 217181,
        217182, 217183, 217184, 217185, 217186, 217187, 217188, 217189, 217190, 217191, 217192, 217193, 217194, 217195,
        217196, 217197, 217198, 217199, 217200, 217201, 217202, 217203, 217204, 217205, 217206, 218003, 218004, 218006,
        218007, 218009, 218010, 218011, 218012, 218013, 218014, 218015, 218016, 218018, 218019, 218020, 218021, 218022,
        218023, 218024, 218026, 218027, 218028, 218029, 218030, 218032, 218033, 218034, 218035, 218036, 218037, 218038,
        218039, 218040, 218041, 218042, 218043, 218044, 218045, 218046, 218047, 218048, 218049, 218050, 218051, 218052,
        218053, 218054, 218055, 218056, 218057, 218058, 218059, 218060, 218061, 218062, 218063, 218064, 218065, 218066,
        218067, 218068, 218069, 218070, 218071, 218072, 218073, 218074, 218075, 218076, 218077, 218078, 218079, 218080,
        218081, 218082, 218083, 218084, 218085, 218086, 218087, 218088, 218089, 218090, 218091, 218092, 218093, 218094,
        218095, 218096, 218097, 218098, 218099, 218100, 218101, 218102, 218103, 218104, 218105, 218106, 218107, 218108,
        218109, 218110, 218111, 218112, 218113, 218114, 218115, 218116, 218117, 218118, 218119, 218120, 218121, 218122,
        218123, 218124, 218125, 218126, 218127, 218128, 218129, 218130, 218131, 218132, 218133, 218134, 218135, 218136,
        218137, 218138, 218139, 218140, 218141, 218142, 218143, 218144, 218145, 218146, 218147, 218148, 218149, 218150,
        218151, 218152, 218153, 218154, 218155, 218156, 218157, 218158, 218159, 218160, 218161, 218162, 218163, 218164,
        218165, 218166, 218167, 218168, 218169, 218170, 218171, 218172, 218173, 218174, 218175, 218176, 218177, 218178,
        218179, 218180, 218181, 218182, 218183, 218184, 218185, 218186, 218187, 218188, 218189, 218190, 218191, 218192,
        218193, 218194, 218195, 218196, 218197, 218198, 218199, 218200, 218201, 218202, 218203, 218204, 218205, 218206,
        219001, 219002, 219003, 219004, 219005, 219006, 219007, 219008, 219009, 219010, 219011, 219012, 219013, 219014,
        219015, 219016, 219017, 219018, 219019, 219020, 219021, 219022, 219023, 219024, 219025, 219026, 219027, 219028,
        219029, 219030, 219031, 219032, 219033, 219034, 219035, 219036, 219037, 219038, 219039, 219040, 219041, 219042,
        219043, 219044, 219045, 219046, 219047, 219048, 219049, 219050, 219051, 219052, 219053, 219054, 219055, 219056,
        219057, 219058, 219059, 219060, 219061, 219062, 219063, 219064, 219065, 219066, 219067, 219068, 219069, 219070,
        219071, 219072, 219073, 219074, 219075, 219076, 219077, 219078, 219079, 219080, 219081, 219082, 219083, 219084,
        219085, 219086, 219087, 219088, 219089, 219090, 219091, 219092, 219093, 219094, 219095, 219096, 219097, 219098,
        219099, 219100, 219101, 219102, 219103, 219104, 219105, 219106, 219107, 219108, 219109, 219110, 219111, 219112,
        219113, 219114, 219115, 219116, 219117, 219118, 219119, 219120, 219121, 219122, 219123, 219124, 219125, 219126,
        219127, 219128, 219129, 219130, 219131, 219132, 219133, 219134, 219135, 219136, 219137, 219138, 219139, 219140,
        219141, 219142, 219143, 219144, 219145, 219146, 219147, 219148, 219149, 219150, 219151, 219152, 219153, 219154,
        219155, 219156, 219157, 219158, 219159, 219160, 219161, 219162, 219163, 219164, 219165, 219166, 219167, 219168,
        219169, 219170, 219171, 219172, 219173, 219174, 219175, 219176, 219177, 219178, 219179, 219180, 219181, 219182,
        219183, 219184, 219185, 219186, 219187, 219188, 219189, 219190, 219191, 219192, 219193, 219194, 219195, 219196,
        219197, 219198, 219199, 219200, 219201, 219202, 219203, 219204, 219205, 219206, 219207, 219208, 219209, 219210,
        219211, 219212, 219213, 219214, 219215, 219216, 219217, 219218, 219219, 219220, 220228, 221001, 221002, 221003,
        221004, 221005, 221006, 221007, 221008, 221009, 221010, 221011, 221012, 221013, 221014, 221015, 221016, 221017,
        221018, 221019, 221020, 221021, 221022, 221023, 221024, 221025, 221026, 221027, 221028, 221029, 221030, 221031,
        221032, 221033, 221034, 221035, 221036, 221037, 221038, 221039, 221040, 221041, 221042, 221043, 221044, 221045,
        221046, 221047, 221048, 221049, 221050, 221051, 221052, 221053, 221054, 221055, 221056, 221057, 221058, 221059,
        221060, 221061, 221062, 221063, 221064, 221065, 221066, 221067, 221068, 221069, 221070, 221071, 221072, 221073,
        221074, 221075, 221076, 221077, 221078, 221079, 221080, 221081, 221082, 221083, 221084, 221085, 221086, 221087,
        221088, 221089, 221090, 221091, 221092, 221093, 221094, 221095, 221096, 221097, 221098, 221099, 221100, 221101,
        221102, 221103, 221104, 221105, 221106, 221107, 221108, 221109, 221110, 221111, 221112, 221113, 221114, 221115,
        221116, 221117, 221118, 221119, 221120, 221121, 221122, 221123, 221124, 221125, 221126, 221127, 221128, 221129,
        221130, 221131, 221132, 221133, 221134, 221135, 221136, 221137, 221138, 221139, 221140, 221141, 221142, 221143,
        221144, 221145, 221146, 221147, 221148, 221149, 221150, 221151, 221152, 221153, 221154, 221155, 221156, 221157,
        221158, 221159, 221160, 221161, 221162, 221163, 221164, 221165, 221166, 221167, 221168, 221169, 221170, 221171,
        221172, 221173, 221174, 221175, 221176, 221177, 221178, 221179, 221180, 221181, 221182, 221183, 221184, 221185,
        221186, 221187, 221188, 221189, 221190, 221191, 221192, 221193, 221194, 221195, 221196, 221197, 221198, 221199,
        221200, 221201, 221202, 221203, 221204, 221205, 221206, 228025, 228040, 228041, 228042, 228043, 228091, 228092,
        228093, 228100, 228101, 228102, 228103, 228248, 228250, 228252, 228253, 228254, 228255, 234139, 234151, 234167,
        234228,
    };

    return (set.find(p) != set.end());
}


// Tool managing specific types & functions

using ValueSet = std::unordered_set<std::string>;
using FieldValueMap = std::unordered_map<std::string, ValueSet>;

FieldValueMap parseFieldValueMap(std::string s, int verbosity) {
    const std::string FIELD_DELIM = ";";
    const std::string FIELDVAL_DELIM = "=";
    const std::string VALUES_DELIM = ",";
    FieldValueMap ret;

    size_t posField = 0;
    std::string fieldAndVals;
    do {
        posField = s.find(FIELD_DELIM);
        fieldAndVals = s.substr(0, posField);
        s.erase(0, posField + FIELD_DELIM.length());

        // Separate field from values by split on =
        size_t posFieldVal = fieldAndVals.find(FIELDVAL_DELIM);
        ASSERT(posFieldVal != std::string::npos);
        std::string field = fieldAndVals.substr(0, posFieldVal);
        fieldAndVals.erase(0, posFieldVal + FIELDVAL_DELIM.length());
        // fieldAndVals should contain only values now
        if (verbosity >= 2) {
            std::cout << "Parsed field " << field << std::endl;
        }

        size_t posVals = 0;
        std::string val;
        ValueSet values;
        do {
            posVals = fieldAndVals.find(VALUES_DELIM);
            val = fieldAndVals.substr(0, posVals);
            fieldAndVals.erase(0, posVals + VALUES_DELIM.length());

            if (verbosity >= 2) {
                std::cout << "   parsed value: " << val << std::endl;
            }

            values.insert(val);
        } while (posVals != std::string::npos);

        ret.emplace(std::move(field), std::move(values));
    } while (posField != std::string::npos);

    return ret;
}

bool matches(eckit::message::Message msg, const FieldValueMap& map, int verbosity = 0) {
    for (const auto& fieldVals : map) {
        std::string fieldVal = msg.getString(fieldVals.first);

        bool has = (fieldVals.second.find(fieldVal) != fieldVals.second.end());
        if (has) {
            if (verbosity >= 2) {
                std::cout << "Matched field \"" << fieldVals.first << "\" with value \"" << fieldVal << "\""
                          << std::endl;
            }
            return true;
        }
    }
    return false;
}


enum class Discipline192Handling : std::size_t
{
    LogAndIgnore,
    Ignore,
    TryToHandle,
};

const std::unordered_map<std::string, Discipline192Handling>& discipline192HandlingMap() {
    static const std::unordered_map<std::string, Discipline192Handling> map{
        {"log-and-ignoore", Discipline192Handling::LogAndIgnore},
        {"ignore", Discipline192Handling::Ignore},
        {"try-to-handle", Discipline192Handling::TryToHandle}};
    return map;
}


Discipline192Handling parseDiscipline192Handling(const std::string& str) {
    const auto& map = discipline192HandlingMap();

    if (auto search = map.find(str); search != map.end()) {
        return search->second;
    }
    throw std::runtime_error(std::string("Unsupported discipline-192 handling: ") + str);
}

}  // namespace

class MultioMMtg2 final : public multio::MultioTool {
public:  // methods
    MultioMMtg2(int argc, char** argv);

private:
    void usage(const std::string& tool) const override {
        eckit::Log::info() << std::endl << "Usage: " << tool << " [options] inputFile outputFile " << std::endl;
        eckit::Log::info() << std::endl
                           << "\tinputFile:\t"
                           << "GRIB file" << std::endl
                           << "\toutputFile:\t"
                           << "output file location" << std::endl;
    }

    void init(const eckit::option::CmdArgs& args) override;

    void finish(const eckit::option::CmdArgs&) override;

    void execute(const eckit::option::CmdArgs& args) override;

    bool subtocExists() const;

    int numberOfPositionalArguments() const override { return 2; }
    int minimumPositionalArguments() const override { return 2; }


    bool copyGrib2Messages_ = true;
    std::string knowledgeRoot_ = "";
    std::string samplePath_ = "";
    std::string encodingFile_ = "";
    std::string mappingFile_ = "";
    long verbosity_ = 0;

    std::optional<FieldValueMap> excludeMap_ = {};
    std::optional<FieldValueMap> filterMap_ = {};
    std::optional<std::string> overwritePacking_ = {};
    Discipline192Handling discipline192Handling_ = Discipline192Handling::LogAndIgnore;
};

MultioMMtg2::MultioMMtg2(int argc, char** argv) : multio::MultioTool{argc, argv} {
    options_.push_back(new eckit::option::SimpleOption<bool>("help", "Print help"));
    options_.push_back(new eckit::option::SimpleOption<bool>(
        "all", "If specified also grib2 messages will reencoded instead of copied"));
    options_.push_back(
        new eckit::option::SimpleOption<std::string>("knowledge-root",
                                                     "Path to knowledege root dir containing grib2 sample, encoding "
                                                     "and mapping rules. Default: MULTIO_HOME/share/multiom/49r2v9"));
    options_.push_back(new eckit::option::SimpleOption<std::string>(
        "encoding-rules", "Path to encoding-rules.yaml. Default: KNOWLEDGE_ROOT/encodings/encoding-rules.yaml"));
    options_.push_back(new eckit::option::SimpleOption<std::string>(
        "mapping-rules", "Path to mapping-rules.yaml. Default: KNOWLEDGE_ROOT/mappings/mapping-rules.yaml"));
    options_.push_back(new eckit::option::SimpleOption<std::string>(
        "samples-path", "Path to grib2 samples directory. Default: KNOWLEDGE_ROOT/samples"));
    options_.push_back(new eckit::option::SimpleOption<bool>("verbose", "Sets verbosity to 2"));
    options_.push_back(
        new eckit::option::SimpleOption<long>("verbosity",
                                              "Verbosity level: 0 (print nothing), 1 (print mars keys per message), 2 "
                                              "(print additional extraction information)"));
    options_.push_back(new eckit::option::SimpleOption<std::string>(
        "exclude",
        "Keys and values to be excluded. Multiple values are separated by ','. Multiple key-values pairs are separated "
        "by ';'. Example --exclude paramId=130,131,133;levtype=pl,sfc"));
    options_.push_back(new eckit::option::SimpleOption<std::string>(
        "filter",
        "Keys and values to be included. Multiple values are separated by ','. Multiple key-values pairs are separated "
        "by ';'. Example --filter paramId=130,131,133;levtype=pl,sfc"));
    options_.push_back(new eckit::option::SimpleOption<std::string>(
        "packing", "Enforce a specific packing type. Valid values are `ccsds` and `simple`."));

    options_.push_back(new eckit::option::SimpleOption<std::string>(
        "discipline-192",
        "Options on handling fields with discipline 192 (field that are ill-formed). Values: \"log-and-ignore\" "
        "(default), \"ignore\", \"try-to-handle\""));
}

void MultioMMtg2::init(const eckit::option::CmdArgs& args) {
    bool verbose = false;
    args.get("verbose", verbose);
    if (verbose) {
        verbosity_ = 2;
    }
    args.get("verbosity", verbosity_);

    bool all = false;
    args.get("all", all);
    copyGrib2Messages_ = !all;

    std::string packing;
    args.get("packing", packing);
    if (!packing.empty()) {
        if (packing == "ccsds" || packing == "simple") {
            overwritePacking_ = packing;
        }
        else {
            throw std::runtime_error(std::string("Unsupported packing: ") + packing);
        }
    }

    args.get("knowledge-root", knowledgeRoot_);
    if (knowledgeRoot_.empty()) {
        knowledgeRoot_ = multio::LibMultio::instance().libraryHome();
    }
    args.get("samples-path", samplePath_);
    if (samplePath_.empty()) {
        samplePath_ = knowledgeRoot_ + "/share/multiom/samples";
    }
    args.get("encodingFile_", encodingFile_);
    if (encodingFile_.empty()) {
        encodingFile_ = knowledgeRoot_ + "/share/multiom/encodings/encoding-rules.yaml";
    }
    args.get("mappingFile_", mappingFile_);
    if (mappingFile_.empty()) {
        mappingFile_ = knowledgeRoot_ + "/share/multiom/mappings/mapping-rules.yaml";
    }

    if (verbosity_ > 1) {
        std::cout << "knowledge-root: " << knowledgeRoot_ << std::endl;
        std::cout << "samples-path: " << samplePath_ << std::endl;
        std::cout << "encoding-rules: " << encodingFile_ << std::endl;
        std::cout << "mapping-rules: " << mappingFile_ << std::endl;
    }
    setenv("IFS_INSTALL_DIR", knowledgeRoot_.c_str(), 0);

    std::string excludeStr = "";
    args.get("exclude", excludeStr);
    if (!excludeStr.empty()) {
        excludeMap_ = parseFieldValueMap(std::move(excludeStr), verbosity_);
    }

    std::string filterStr = "";
    args.get("filter", filterStr);
    if (!filterStr.empty()) {
        filterMap_ = parseFieldValueMap(std::move(filterStr), verbosity_);
    }

    std::string discipline192;
    args.get("discipline-192", discipline192);
    if (!discipline192.empty()) {
        discipline192Handling_ = parseDiscipline192Handling(discipline192);
    }
}

void MultioMMtg2::finish(const eckit::option::CmdArgs&) {}

void MultioMMtg2::execute(const eckit::option::CmdArgs& args) {
    using eckit::message::ValueRepresentation;
    eckit::message::Reader reader{args(0)};
    eckit::PathName outPath{args(1)};

    if (outPath.exists()) {
        const int result = remove(((std::string)outPath).c_str());
        if (result == 0) {
            if (verbosity_ > 0) {
                std::cout << "Removed existing file " << outPath << std::endl;
            }
        }
        else {
            std::cerr << "Could not remove existing file " << outPath << std::endl;
            return;
        }
    }
    eckit::FileHandle outputFileHandle{outPath, true};  // overwrite output

    outputFileHandle.openForWrite(0);

    MultiOMDict optDict{MultiOMDictKind::Options};
    void* encoder = NULL;

    optDict.set("print-whole-error-stack", std::to_string(verbosity_ > 1 ? 1 : 0));
    optDict.set("print-dictionaries", std::to_string(verbosity_ > 1 ? 1 : 0));
    optDict.set("samples-path", samplePath_);
    optDict.set("encoding-rules", encodingFile_);
    optDict.set("mapping-rules", mappingFile_);

    ASSERT(multio_grib2_encoder_open(optDict.get(), &encoder) == 0);

    eckit::message::Message msg;
    while ((msg = reader.next())) {
        // Extract message from datahandle... we expect it to be a memory handle
        // TODO: Alternative would be to explicitly create a eckit::MemoryHandle and write to it
        std::unique_ptr<eckit::DataHandle> dh{msg.readHandle()};
        eckit::MemoryHandle* mh = reinterpret_cast<eckit::MemoryHandle*>(dh.get());

        ASSERT(mh != NULL);
        std::unique_ptr<codes_handle> inputCodesHandle{codes_handle_new_from_message(NULL, mh->data(), mh->size())};

        dh.reset(nullptr);
        mh = NULL;

        metkit::codes::CodesContent* inputCodesContent = new metkit::codes::CodesContent{inputCodesHandle.get(), false};
        eckit::message::Message inputMsg{inputCodesContent};

        if (excludeMap_) {
            bool ret = matches(msg, *excludeMap_, verbosity_);
            if (ret) {
                if (verbosity_ >= 2) {
                    std::cout << "exclude map  matched... skipping message" << std::endl;
                }
                continue;
            }
        }
        if (filterMap_) {
            bool ret = matches(msg, *filterMap_, verbosity_);
            if (!ret) {
                if (verbosity_ >= 2) {
                    std::cout << "filter map did not match... skipping message" << std::endl;
                }
                continue;
            }
        }


        std::string edition = inputMsg.getString("edition");
        if (discipline192Handling_ != Discipline192Handling::TryToHandle) {
            long paramId = inputMsg.getLong("paramId");
            bool isDiscipline192
                = (edition == "1") ? isDiscipline192Param(paramId) : (inputMsg.getLong("discipline") == 192);
            if (isDiscipline192) {
                if (discipline192Handling_ == Discipline192Handling::LogAndIgnore) {
                    std::cout << "Excluding message with discipline 192 (paramId: " << paramId << ")" << std::endl;
                }
                continue;
            }
        }

        // std::string paramId = inputMsg.getString("paramId");
        if (edition == "2" && copyGrib2Messages_) {
            // Write the message directly
            if (verbosity_ > 2) {
                std::cout << "Copying grib2 message..." << std::endl;
            }
            inputMsg.write(outputFileHandle);
        }
        else {

            // now inputCodesHandle is save to use
            MultiOMDict marsDict{MultiOMDictKind::MARS};
            MultiOMDict parDict{MultiOMDictKind::Parametrization};

            Map marsKeys = getMarsKeys(inputMsg);
            if (verbosity_ > 2) {
                std::cout << "Extracting metadata..." << std::endl;
            }

            extract::grib1ToGrib2(marsKeys, (codes_handle*)inputCodesHandle.get(), marsDict, parDict);


            if (overwritePacking_) {
                if (verbosity_ > 2) {
                    std::cout << "Overwrite packing " << *overwritePacking_ << std::endl;
                }
                marsDict.set("packing", overwritePacking_->c_str());
            }

            if (verbosity_ > 2) {
                std::cout << "Extracted MARS dict:" << std::endl;
                marsDict.toYAML("stdout");
                std::cout << "Extracted PAR dict:" << std::endl;
                parDict.toYAML("stdout");
            }

            codes_handle* rawOutputCodesHandle = NULL;

            std::vector<double> values;
            inputMsg.getDoubleArray("values", values);

            if (verbosity_ > 2) {
                std::cout << "Encoding with extracted metadata..." << std::endl;
            }
            ASSERT(multio_grib2_encoder_encode64(encoder, marsDict.get(), parDict.get(), values.data(), values.size(),
                                                 (void**)&rawOutputCodesHandle)
                   == 0);
            ASSERT(rawOutputCodesHandle != NULL);

            // Apply more changes
            extract::postFixToolOnly(inputCodesHandle.get(), rawOutputCodesHandle);

            if (verbosity_ > 0) {
                char* jsonString;
                multio_grib2_dict_to_json(marsDict.get(), &jsonString);
                std::cout << "Converted " << jsonString << std::endl;
            }

            // Output by writing all to the same binary file
            eckit::message::Message outputMsg{new metkit::codes::CodesContent{rawOutputCodesHandle, true}};

            outputMsg.write(outputFileHandle);
        }
    }

    outputFileHandle.close();
    ASSERT(multio_grib2_encoder_close(&encoder) == 0);
}

}  // namespace multiom

//---------------------------------------------------------------------------------------------------------------

int main(int argc, char** argv) {
    multiom::MultioMMtg2 tool(argc, argv);
    return tool.start();
}

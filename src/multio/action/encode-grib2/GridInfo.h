/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/// @author Philipp Geier

/// @date August 2023

#pragma once

#include "multio/message/Message.h"

#include <optional>
#include <string>
#include <type_traits>
#include <variant>

// Forward declare
namespace atlas {

class Grid;
class StructuredGrid;
class GaussianGrid;
class RegularGaussianGrid;
class ReducedGaussianGrid;
class HealPixGrid;

}  // namespace atlas

namespace eckit {

class Configuration;

}


namespace multio::action::encodeGrib2 {

static const std::string ATLAS_NAMED_GRID_KEY{"atlasNamedGrid"};
static const std::string DOMAIN_KEY{"domain"};
static const std::string GLOBAL_SIZE_KEY{"globalSize"};

static const std::string CONF_EXTRACT_LAT_LON{"extract-lon-lat"};
static const std::string CONF_PARAM_ID_LON{"lon-param-id"};
static const std::string CONF_PARAM_ID_LAT{"lat-param-id"};

//-----------------------------------------------------------------------------

enum class GridType
{
    Unstructured,
    RegularGaussian,
    ReducedGaussian,
    ReducedLonLat,
    RegularLonLat,
    HealPix,
};

template <GridType t>
struct GridTypeTag {
    static constexpr GridType value = t;
};

template <typename Func, typename... Args>
decltype(auto) dispatchGridType(Func&& func, GridType gridType, Args&&... args) {
    switch (gridType) {
        case GridType::Unstructured:
            return std::forward<Func>(func)(GridTypeTag<GridType::Unstructured>{}, std::forward<Args>(args)...);
        case GridType::RegularGaussian:
            return std::forward<Func>(func)(GridTypeTag<GridType::RegularGaussian>{}, std::forward<Args>(args)...);
        case GridType::ReducedGaussian:
            return std::forward<Func>(func)(GridTypeTag<GridType::ReducedGaussian>{}, std::forward<Args>(args)...);
        case GridType::RegularLonLat:
            return std::forward<Func>(func)(GridTypeTag<GridType::RegularLonLat>{}, std::forward<Args>(args)...);
        case GridType::ReducedLonLat:
            return std::forward<Func>(func)(GridTypeTag<GridType::ReducedLonLat>{}, std::forward<Args>(args)...);
        case GridType::HealPix:
            return std::forward<Func>(func)(GridTypeTag<GridType::HealPix>{}, std::forward<Args>(args)...);
    }
}


//-----------------------------------------------------------------------------

// Functions performing mapping to enum class and allow combination with dispatching
GridType atlasNamedGridToGridType(const std::string& atlasNamedGrid);
GridType eccodesGridTypeToGridType(const std::string& eccodesGridType);


// Static Explicitly instanticated
template <GridType>
struct GridInfoCreatorPolicy;

struct GridInfoCreationOptions {
    bool extractLonLatFromUnstructuredGrid = false;
    std::optional<std::int64_t> paramIdLon = {};
    std::optional<std::int64_t> paramIdLat = {};
};


void gridInfoCreationOptionsFromConfiguration(GridInfoCreationOptions&, const eckit::Configuration& c);
GridInfoCreationOptions gridInfoCreationOptionsFromConfiguration(const eckit::Configuration& c);


// Wrapper around policy to declare methods...
template <GridType GT>
struct GridInfoCreator {
    static decltype(auto) fromAtlas(const std::string& atlasGridType, const GridInfoCreationOptions& options,
                                    const message::Metadata& md) {
        return GridInfoCreatorPolicy<GT>::fromAtlas(atlasGridType, options, md);
    }

    static decltype(auto) fromCodes(const std::string& codesGridType, const GridInfoCreationOptions& options,
                                    const message::Metadata& md) {
        return GridInfoCreatorPolicy<GT>::fromCodes(codesGridType, options, md);
    }
};


//-----------------------------------------------------------------------------

template <typename T>
struct CodesKeySetter;


template <typename T, typename KeySetter>
void codesKeySetter(const T& info, KeySetter&& keySetter) {
    CodesKeySetter<std::decay_t<T>>{}(info, std::forward<KeySetter>(keySetter));
}


//-----------------------------------------------------------------------------

// Eccodes keys
struct BaseGridInfo {
    std::string gridType;
};

template <>
struct CodesKeySetter<BaseGridInfo> {
    template <typename KeySetter>
    void operator()(const BaseGridInfo& i, KeySetter&& keySetter) {
        keySetter("gridType", i.gridType);
    }
};


//-----------------------------------------------------------------------------

struct UnstructuredGridInfo : BaseGridInfo {
    std::string unstructuredGridType;
    std::string unstructuredGridSubtype;
    std::string unstructuredGridUUID;
    std::int64_t numberOfDataPoints;

    struct LonLatEnc {
        std::int64_t paramIdLon, paramIdLat;
        std::vector<double> lon, lat;
    };
    std::optional<LonLatEnc> lonLat;
};

template <>
struct CodesKeySetter<UnstructuredGridInfo> {
    template <typename KeySetter>
    void operator()(const UnstructuredGridInfo& i, KeySetter&& keySetter) {
        CodesKeySetter<BaseGridInfo>{}(i, keySetter);
        keySetter("unstructuredGridType", i.unstructuredGridType);
        keySetter("unstructuredGridSubtype", i.unstructuredGridSubtype);
        keySetter("unstructuredGridUUID", i.unstructuredGridUUID);
        keySetter("uuidOfHGrid", i.unstructuredGridType);
        keySetter("numberOfDataPoints", i.numberOfDataPoints);
    }
};


template <>
struct GridInfoCreatorPolicy<GridType::Unstructured> {
    static UnstructuredGridInfo fromAtlas(const std::string& atlasGridType, const GridInfoCreationOptions& options,
                                          const message::Metadata& md);
    static UnstructuredGridInfo fromCodes(const std::string& codesGridType, const GridInfoCreationOptions& options,
                                          const message::Metadata& md);
};


//-----------------------------------------------------------------------------

struct BaseGaussianGridInfo : BaseGridInfo {
    std::int64_t n;
    double latitudeOfFirstGridPointInDegrees;
    double latitudeOfLastGridPointInDegrees;
    double longitudeOfFirstGridPointInDegrees;
    double longitudeOfLastGridPointInDegrees;
};

template <>
struct CodesKeySetter<BaseGaussianGridInfo> {
    template <typename KeySetter>
    void operator()(const BaseGaussianGridInfo& i, KeySetter&& keySetter) {
        CodesKeySetter<BaseGridInfo>{}(i, keySetter);
        keySetter("N", i.n);
        keySetter("latitudeOfFirstGridPointInDegrees", i.latitudeOfFirstGridPointInDegrees);
        keySetter("latitudeOfLastGridPointInDegrees", i.latitudeOfLastGridPointInDegrees);
        keySetter("longitudeOfFirstGridPointInDegrees", i.longitudeOfFirstGridPointInDegrees);
        keySetter("longitudeOfLastGridPointInDegrees", i.longitudeOfLastGridPointInDegrees);
    }
};

struct BaseGaussianGridInfoCreationPolicy {
    static void fromAtlas(BaseGaussianGridInfo& info, const atlas::GaussianGrid& grid,
                          const GridInfoCreationOptions& options, const message::Metadata& md);
    static void fromCodes(BaseGaussianGridInfo& info, const std::string& codesGridType,
                          const GridInfoCreationOptions& options, const message::Metadata& md);
};


//-----------------------------------------------------------------------------

struct GridWithPLArray {
    std::vector<std::int64_t> pl;
};

template <>
struct CodesKeySetter<GridWithPLArray> {
    template <typename KeySetter>
    void operator()(const GridWithPLArray& i, KeySetter&& keySetter) {
        keySetter("pl", i.pl);
    }
};

struct GridWithPLArrayCreationPolicy {
    static void fromAtlas(GridWithPLArray& info, const atlas::StructuredGrid& grid,
                          const GridInfoCreationOptions& options, const message::Metadata& md);
    static void fromCodes(GridWithPLArray& info, const std::string& codesGridType,
                          const GridInfoCreationOptions& options, const message::Metadata& md);
};

//-----------------------------------------------------------------------------


struct GridWithNiNj {
    std::int64_t ni;
    std::int64_t nj;
};

template <>
struct CodesKeySetter<GridWithNiNj> {
    template <typename KeySetter>
    void operator()(const GridWithNiNj& i, KeySetter&& keySetter) {
        keySetter("Ni", i.ni);
        keySetter("Nj", i.nj);
    }
};

struct GridWithNiNjCreationPolicy {
    static void fromAtlas(GridWithNiNj& info, const atlas::StructuredGrid& grid, const GridInfoCreationOptions& options,
                          const message::Metadata& md);
    static void fromCodes(GridWithNiNj& info, const std::string& codesGridType, const GridInfoCreationOptions& options,
                          const message::Metadata& md);
};


//-----------------------------------------------------------------------------

struct RegularGaussianGridInfo : BaseGaussianGridInfo {};

template <>
struct CodesKeySetter<RegularGaussianGridInfo> {
    template <typename KeySetter>
    void operator()(const RegularGaussianGridInfo& i, KeySetter&& keySetter) {
        CodesKeySetter<BaseGaussianGridInfo>{}(i, keySetter);
    }
};

template <>
struct GridInfoCreatorPolicy<GridType::RegularGaussian> {
    static RegularGaussianGridInfo fromAtlas(const std::string& atlasGridType, const GridInfoCreationOptions& options,
                                             const message::Metadata& md);
    static RegularGaussianGridInfo fromCodes(const std::string& codesGridType, const GridInfoCreationOptions& options,
                                             const message::Metadata& md);
};


//-----------------------------------------------------------------------------

struct ReducedGaussianGridInfo : BaseGaussianGridInfo, GridWithPLArray {};

template <>
struct CodesKeySetter<ReducedGaussianGridInfo> {
    template <typename KeySetter>
    void operator()(const ReducedGaussianGridInfo& i, KeySetter&& keySetter) {
        CodesKeySetter<BaseGaussianGridInfo>{}(i, keySetter);
        CodesKeySetter<GridWithPLArray>{}(i, keySetter);
    }
};

template <>
struct GridInfoCreatorPolicy<GridType::ReducedGaussian> {
    static ReducedGaussianGridInfo fromAtlas(const std::string& atlasGridType, const GridInfoCreationOptions& options,
                                             const message::Metadata& md);
    static ReducedGaussianGridInfo fromCodes(const std::string& codesGridType, const GridInfoCreationOptions& options,
                                             const message::Metadata& md);
};


//-----------------------------------------------------------------------------

struct RegularLonLatInfo : BaseGridInfo, GridWithNiNj {};

template <>
struct CodesKeySetter<RegularLonLatInfo> {
    template <typename KeySetter>
    void operator()(const RegularLonLatInfo& i, KeySetter&& keySetter) {
        CodesKeySetter<BaseGridInfo>{}(i, keySetter);
        CodesKeySetter<GridWithNiNj>{}(i, keySetter);
    }
};

template <>
struct GridInfoCreatorPolicy<GridType::RegularLonLat> {
    static RegularLonLatInfo fromAtlas(const std::string& atlasGridType, const GridInfoCreationOptions& options,
                                       const message::Metadata& md);
    static RegularLonLatInfo fromCodes(const std::string& codesGridType, const GridInfoCreationOptions& options,
                                       const message::Metadata& md);
};


//-----------------------------------------------------------------------------

struct ReducedLonLatInfo : BaseGridInfo, GridWithNiNj, GridWithPLArray {};

template <>
struct CodesKeySetter<ReducedLonLatInfo> {
    template <typename KeySetter>
    void operator()(const ReducedLonLatInfo& i, KeySetter&& keySetter) {
        CodesKeySetter<BaseGridInfo>{}(i, keySetter);
        CodesKeySetter<GridWithNiNj>{}(i, keySetter);
        CodesKeySetter<GridWithPLArray>{}(i, keySetter);
    }
};

template <>
struct GridInfoCreatorPolicy<GridType::ReducedLonLat> {
    static ReducedLonLatInfo fromAtlas(const std::string& atlasGridType, const GridInfoCreationOptions& options,
                                       const message::Metadata& md);
    static ReducedLonLatInfo fromCodes(const std::string& codesGridType, const GridInfoCreationOptions& options,
                                       const message::Metadata& md);
};


//-----------------------------------------------------------------------------

struct HealPixGridInfo : BaseGridInfo {
    std::int64_t nside;
    double longitudeOfFirstGridPointInDegrees;
    std::string orderingConvention;
};

template <>
struct CodesKeySetter<HealPixGridInfo> {
    template <typename KeySetter>
    void operator()(const HealPixGridInfo& i, KeySetter&& keySetter) {
        CodesKeySetter<BaseGridInfo>{}(i, keySetter);
        keySetter("Nside", i.nside);
        keySetter("longitudeOfFirstGridPointInDegrees", i.longitudeOfFirstGridPointInDegrees);
        keySetter("orderingConvention", i.orderingConvention);
    }
};


template <>
struct GridInfoCreatorPolicy<GridType::HealPix> {
    static HealPixGridInfo fromAtlas(const std::string& atlasGridType, const GridInfoCreationOptions& options,
                                     const message::Metadata& md);
    static HealPixGridInfo fromCodes(const std::string& codesGridType, const GridInfoCreationOptions& options,
                                     const message::Metadata& md);
};


//-----------------------------------------------------------------------------

using GridInfo = std::variant<UnstructuredGridInfo, RegularGaussianGridInfo, ReducedGaussianGridInfo, RegularLonLatInfo,
                              ReducedLonLatInfo, HealPixGridInfo>;


template <>
struct CodesKeySetter<GridInfo> {
    template <typename KeySetter>
    void operator()(const GridInfo& i, KeySetter&& keySetter) {
        std::visit([&keySetter](const auto& gi) { codesKeySetter(gi, std::forward<KeySetter>(keySetter)); }, i);
    }
};

GridInfo gridInfoFromMetadata(const GridInfoCreationOptions& options, const message::Metadata& md);

}  // namespace multio::action::encodeGrib2

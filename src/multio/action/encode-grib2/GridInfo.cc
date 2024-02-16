#include "multio/action/encode-grib2/GridInfo.h"
#include "multio/action/encode-grib2/Exception.h"

#include "atlas/grid/Grid.h"
#include "atlas/grid/Iterator.h"
#include "atlas/grid/StructuredGrid.h"
#include "atlas/grid/UnstructuredGrid.h"
#include "atlas/library.h"
#include "atlas/parallel/mpi/mpi.h"

#include "eckit/config/Configuration.h"
#include "eckit/utils/Translator.h"


namespace multio::action::encodeGrib2 {


const std::unordered_map<std::string, std::tuple<std::int64_t, std::int64_t>> gridSubtypeToLonLatParamId{
    {"T", {250004, 250003}},
    {"U", {250006, 250005}},
    {"V", {250008, 250007}},
    {"W", {250010, 250009}},
    {"F", {250012, 250011}}};


//-----------------------------------------------------------------------------

void gridInfoCreationOptionsFromConfiguration(GridInfoCreationOptions& opts, const eckit::Configuration& c) {
    if (c.has(CONF_EXTRACT_LAT_LON)) {
        opts.extractLonLatFromUnstructuredGrid = c.getBool(CONF_EXTRACT_LAT_LON);
    }
    if (c.has(CONF_PARAM_ID_LON)) {
        opts.paramIdLon = c.getInt(CONF_PARAM_ID_LON);
    }
    if (c.has(CONF_PARAM_ID_LAT)) {
        opts.paramIdLon = c.getInt(CONF_PARAM_ID_LAT);
    }
}

GridInfoCreationOptions gridInfoCreationOptionsFromConfiguration(const eckit::Configuration& c) {
    GridInfoCreationOptions opts;
    gridInfoCreationOptionsFromConfiguration(opts, c);
    return opts;
}


//-----------------------------------------------------------------------------


GridType atlasNamedGridToGridType(const std::string& atlasNamedGrid) {
    if (atlasNamedGrid.empty()) {
        std::ostringstream oss;
        oss << "Value for \"" << ATLAS_NAMED_GRID_KEY << "\" is empty";
        throw EncodeGrib2Exception(oss.str(), Here());
    }

    switch (atlasNamedGrid[0]) {
        case 'F':
            return GridType::RegularGaussian;

        case 'O':
        case 'N':
            return GridType::ReducedGaussian;

        case 'L':
            return GridType::RegularLonLat;

        case 'H':
            return GridType::HealPix;

        default: {
            auto testORCAPos = atlasNamedGrid.find("ORCA");
            if (testORCAPos == 0 || testORCAPos == 1) {
                return GridType::Unstructured;
            }

            throw EncodeGrib2Exception(std::string("Can not handle atlas named grid: ") + atlasNamedGrid, Here());
        }
    }
}

GridType eccodesGridTypeToGridType(const std::string& eccodesGridType) {
    static const std::unordered_map<std::string, GridType> map{{
        {"unstructured_grid", GridType::Unstructured},
        {"regular_ll", GridType::RegularLonLat},
        {"reduced_ll", GridType::ReducedLonLat},
        {"regular_gg", GridType::RegularGaussian},
        {"reduced_gg", GridType::ReducedGaussian},
        {"healpix", GridType::HealPix},
    }};

    if (auto searchGridType = map.find(eccodesGridType); searchGridType != map.end()) {
        return searchGridType->second;
    }

    throw EncodeGrib2Exception(std::string("Can not handle eccodes grid type : ") + eccodesGridType, Here());
}


//-----------------------------------------------------------------------------

namespace {
atlas::Grid readGrid(const std::string& name) {
    atlas::mpi::Scope mpi_scope("self");
    return atlas::Grid{name};
}
}


UnstructuredGridInfo GridInfoCreatorPolicy<GridType::Unstructured>::fromAtlas(const std::string& atlasGridType,
                                                                              const GridInfoCreationOptions& options,
                                                                              const message::Metadata& md) {
    UnstructuredGridInfo info;
    info.gridType = "unstructured_grid";
    auto uscorePos = atlasGridType.find("_");
    if (uscorePos == std::string::npos) {
        throw EncodeGrib2Exception(
            std::string("Can not infer unstructuredGridType and unstructuredGridSubtype from: ") + atlasGridType,
            Here());
    }
    info.unstructuredGridType
        = md.getOpt<std::string>("unstructuredGridType").value_or(atlasGridType.substr(0, uscorePos));
    info.unstructuredGridSubtype
        = md.getOpt<std::string>("unstructuredGridSubtype").value_or(atlasGridType.substr(uscorePos));

    eckit::Log::info() << "Multio Grib2 UnstructuredGrid - downloading " << atlasGridType << std::endl;

    const atlas::Grid grid = readGrid(atlasGridType);

    eckit::Log::info() << "Multio Grib2 UnstructuredGrid - completed " << atlasGridType << std::endl;

    info.unstructuredGridUUID = grid.uid();
    const std::size_t gridSize = grid.size();

    info.numberOfDataPoints = gridSize;

    if (options.extractLonLatFromUnstructuredGrid) {
        std::tuple<std::int64_t, std::int64_t> lonLatParamIds;

        if (options.paramIdLon && options.paramIdLat) {
            lonLatParamIds = std::make_tuple(*options.paramIdLon, *options.paramIdLat);
        }
        else {
            auto searchParamIds = gridSubtypeToLonLatParamId.find(info.unstructuredGridSubtype);
            if (searchParamIds == gridSubtypeToLonLatParamId.end()) {
                std::ostringstream oss;
                oss << "Can not encode lon lat for unstructured grid. No lon/lat paramIds given for "
                       "unstructuredGridSubtype: "
                    << info.unstructuredGridSubtype;
                oss << ". You may add \"" << CONF_PARAM_ID_LON << "\" and \"" << CONF_PARAM_ID_LAT
                    << "\" to the encoders domain options.";
                throw EncodeGrib2Exception(oss.str(), Here());
            }
            lonLatParamIds = searchParamIds->second;
        }


        info.lonLat = UnstructuredGridInfo::LonLatEnc{std::get<0>(lonLatParamIds), std::get<1>(lonLatParamIds),
                                                      std::vector<double>(gridSize), std::vector<double>(gridSize)};
        size_t n{0};

        for (const auto p : grid.lonlat()) {
            info.lonLat->lat[n] = p.lat();
            info.lonLat->lon[n] = p.lon();
            ++n;
        }

        eckit::Log::info() << "Multio Grib2 UnstructuredGrid - extracted lat lon from " << atlasGridType << std::endl;
    }

    return info;
}


UnstructuredGridInfo GridInfoCreatorPolicy<GridType::Unstructured>::fromCodes(const std::string& codesGridType,
                                                                              const GridInfoCreationOptions& options,
                                                                              const message::Metadata& md) {
    UnstructuredGridInfo info;
    info.gridType = codesGridType;
    info.unstructuredGridType = codesGridType;

    try {
        info.unstructuredGridSubtype = md.get<std::string>("unstructuredGridSubtype");
        info.unstructuredGridUUID = md.get<std::string>("unstructuredGridUUID");
        info.numberOfDataPoints = md.get<std::int64_t>("globalSize");
    }
    catch (const message::MetadataException& err) {
        std::ostringstream oss;
        oss << "UnstructuredGridInfo creator ::fromCodes: Require keys \"unstructuredGridSubtype\" (string), "
               "\"unstructuredGridUUID\" (string) and \"globalSize\" (of type int, to set \"numberOfDataPoints\")  to "
               "prepare a codes sample. Otherwise pass down \""
            << ATLAS_NAMED_GRID_KEY << "\" to retrieve all information from atlas.";
        std::throw_with_nested(EncodeGrib2Exception(oss.str(), Here()));
    }


    if (options.extractLonLatFromUnstructuredGrid) {
        std::ostringstream oss;
        oss << "UnstructuredGridInfo creator ::fromCodes: Explicitly coordinate extraction was requested. This is only "
               "possible through atlas by passing \""
            << ATLAS_NAMED_GRID_KEY << "\".";
        throw EncodeGrib2Exception(oss.str(), Here());
    }

    return info;
}

//-----------------------------------------------------------------------------

void BaseGaussianGridInfoCreationPolicy::fromAtlas(BaseGaussianGridInfo& info, const atlas::GaussianGrid& grid,
                                                   const GridInfoCreationOptions& options,
                                                   const message::Metadata& md) {

    info.n = grid.N();

    auto it = grid.lonlat().begin();
    info.latitudeOfFirstGridPointInDegrees = (*it)[1];
    info.longitudeOfFirstGridPointInDegrees = (*it)[0];

    it += grid.size() - 1;
    info.latitudeOfLastGridPointInDegrees = (*it)[1];

    const auto equator = info.n;
    info.longitudeOfLastGridPointInDegrees = grid.x(grid.nx(equator) - 1, equator);
}

void BaseGaussianGridInfoCreationPolicy::fromCodes(BaseGaussianGridInfo& info, const std::string& codesGridType,
                                                   const GridInfoCreationOptions& options,
                                                   const message::Metadata& md) {
    try {
        info.n = md.get<std::int64_t>("N");

        info.latitudeOfFirstGridPointInDegrees = md.get<double>("latitudeOfFirstGridPointInDegrees");
        info.longitudeOfFirstGridPointInDegrees = md.get<double>("longitudeOfFirstGridPointInDegrees");
        info.latitudeOfLastGridPointInDegrees = md.get<double>("latitudeOfLastGridPointInDegrees");
        info.longitudeOfLastGridPointInDegrees = md.get<double>("longitudeOfLastGridPointInDegrees");
    }
    catch (const message::MetadataException& err) {
        std::ostringstream oss;
        oss << "Base GaussianGridInfo creator ::fromCodes: Require keys \"n\" (int64)";
        oss << ", \"latitudeOfFirstGridPointInDegrees\" (double)";
        oss << ", \"longitudeOfFirstGridPointInDegrees\" (double)";
        oss << ", \"latitudeOfLastGridPointInDegrees\" (double)";
        oss << ", \"longitudeOfLastGridPointInDegrees\" (double)";
        oss << " to prepare a codes sample. Otherwise pass down \"" << ATLAS_NAMED_GRID_KEY
            << "\" to retrieve all information from atlas.";
        std::throw_with_nested(EncodeGrib2Exception(oss.str(), Here()));
    }
}


//-----------------------------------------------------------------------------

void GridWithPLArrayCreationPolicy::fromAtlas(GridWithPLArray& info, const atlas::StructuredGrid& grid,
                                              const GridInfoCreationOptions& options, const message::Metadata& md) {
    auto tmp = grid.nx();
    info.pl = std::vector<std::int64_t>(tmp.size(), 0);
    for (int i = 0; i < tmp.size(); ++i) {
        info.pl[i] = static_cast<std::int64_t>(tmp[i]);
    }
}

void GridWithPLArrayCreationPolicy::fromCodes(GridWithPLArray& info, const std::string& codesGridType,
                                              const GridInfoCreationOptions& options, const message::Metadata& md) {
    try {
        info.pl = md.get<std::vector<std::int64_t>>("pl");
    }
    catch (const message::MetadataException& err) {
        std::ostringstream oss;
        oss << "GridWithPLArray creator ::fromCodes: Require keys \"pl\" (int64[])";
        oss << " to prepare a codes sample. Otherwise pass down \"" << ATLAS_NAMED_GRID_KEY
            << "\" to retrieve all information from atlas.";
        std::throw_with_nested(EncodeGrib2Exception(oss.str(), Here()));
    }
}


//-----------------------------------------------------------------------------

void GridWithNiNjCreationPolicy::fromAtlas(GridWithNiNj& info, const atlas::StructuredGrid& grid,
                                           const GridInfoCreationOptions& options, const message::Metadata& md) {
    info.ni = grid.nxmax();
    info.nj = grid.ny();
}

void GridWithNiNjCreationPolicy::fromCodes(GridWithNiNj& info, const std::string& codesGridType,
                                           const GridInfoCreationOptions& options, const message::Metadata& md) {
    try {
        info.ni = md.get<std::int64_t>("Ni");
        info.nj = md.get<std::int64_t>("Nj");
    }
    catch (const message::MetadataException& err) {
        std::ostringstream oss;
        oss << "GridWithNiNj creator ::fromCodes: Require keys \"ni\" (int64) and \"nj\" (int64)";
        oss << " to prepare a codes sample. Otherwise pass down \"" << ATLAS_NAMED_GRID_KEY
            << "\" to retrieve all information from atlas.";
        std::throw_with_nested(EncodeGrib2Exception(oss.str(), Here()));
    }
}


//-----------------------------------------------------------------------------

RegularGaussianGridInfo GridInfoCreatorPolicy<GridType::RegularGaussian>::fromAtlas(
    const std::string& atlasGridType, const GridInfoCreationOptions& options, const message::Metadata& md) {
    RegularGaussianGridInfo info;
    info.gridType = "regular_gg";
    auto grid = atlas::GaussianGrid{atlas::StructuredGrid{atlas::Grid{atlasGridType}}};
    BaseGaussianGridInfoCreationPolicy::fromAtlas(info, grid, options, md);
    return info;
}

RegularGaussianGridInfo GridInfoCreatorPolicy<GridType::RegularGaussian>::fromCodes(
    const std::string& codesGridType, const GridInfoCreationOptions& options, const message::Metadata& md) {
    RegularGaussianGridInfo info;
    info.gridType = "regular_gg";
    BaseGaussianGridInfoCreationPolicy::fromCodes(info, codesGridType, options, md);
    return info;
}


//-----------------------------------------------------------------------------

ReducedGaussianGridInfo GridInfoCreatorPolicy<GridType::ReducedGaussian>::fromAtlas(
    const std::string& atlasGridType, const GridInfoCreationOptions& options, const message::Metadata& md) {
    ReducedGaussianGridInfo info;
    info.gridType = "reduced_gg";
    auto grid = atlas::GaussianGrid{atlas::StructuredGrid{atlas::Grid{atlasGridType}}};
    BaseGaussianGridInfoCreationPolicy::fromAtlas(info, grid, options, md);
    GridWithPLArrayCreationPolicy::fromAtlas(info, grid, options, md);
    return info;
}

ReducedGaussianGridInfo GridInfoCreatorPolicy<GridType::ReducedGaussian>::fromCodes(
    const std::string& codesGridType, const GridInfoCreationOptions& options, const message::Metadata& md) {
    ReducedGaussianGridInfo info;
    info.gridType = "reduced_gg";
    BaseGaussianGridInfoCreationPolicy::fromCodes(info, codesGridType, options, md);
    GridWithPLArrayCreationPolicy::fromCodes(info, codesGridType, options, md);
    return info;
}

//-----------------------------------------------------------------------------

RegularLonLatInfo GridInfoCreatorPolicy<GridType::RegularLonLat>::fromAtlas(const std::string& atlasGridType,
                                                                            const GridInfoCreationOptions& options,
                                                                            const message::Metadata& md) {
    RegularLonLatInfo info;
    info.gridType = "regular_ll";
    auto grid = atlas::RegularLonLatGrid{atlas::StructuredGrid{atlas::Grid{atlasGridType}}};
    GridWithNiNjCreationPolicy::fromAtlas(info, grid, options, md);
    return info;
}

RegularLonLatInfo GridInfoCreatorPolicy<GridType::RegularLonLat>::fromCodes(const std::string& codesGridType,
                                                                            const GridInfoCreationOptions& options,
                                                                            const message::Metadata& md) {
    RegularLonLatInfo info;
    info.gridType = "regular_ll";
    GridWithNiNjCreationPolicy::fromCodes(info, codesGridType, options, md);
    return info;
}


//-----------------------------------------------------------------------------

ReducedLonLatInfo GridInfoCreatorPolicy<GridType::ReducedLonLat>::fromAtlas(const std::string& atlasGridType,
                                                                            const GridInfoCreationOptions& options,
                                                                            const message::Metadata& md) {
    NOTIMP;
}

ReducedLonLatInfo GridInfoCreatorPolicy<GridType::ReducedLonLat>::fromCodes(const std::string& codesGridType,
                                                                            const GridInfoCreationOptions& options,
                                                                            const message::Metadata& md) {
    ReducedLonLatInfo info;
    info.gridType = "reduced_ll";
    GridWithNiNjCreationPolicy::fromCodes(info, codesGridType, options, md);
    GridWithPLArrayCreationPolicy::fromCodes(info, codesGridType, options, md);
    return info;
}


//-----------------------------------------------------------------------------

HealPixGridInfo GridInfoCreatorPolicy<GridType::HealPix>::fromAtlas(const std::string& atlasGridType,
                                                                    const GridInfoCreationOptions& options,
                                                                    const message::Metadata& md) {
    HealPixGridInfo info;
    // Atlas is only supporting ring curretly
    info.gridType = "healpix";

    // const atlas::HealPixGrid grid(atlasGridType);
    info.nside = eckit::translate<std::int64_t>(atlasGridType.substr(1));

    info.longitudeOfFirstGridPointInDegrees = 45.0;
    info.orderingConvention = "ring";

    return info;
}

HealPixGridInfo GridInfoCreatorPolicy<GridType::HealPix>::fromCodes(const std::string& codesGridType,
                                                                    const GridInfoCreationOptions& options,
                                                                    const message::Metadata& md) {
    HealPixGridInfo info;
    info.gridType = "healpix";

    try {
        info.nside = md.get<std::int64_t>("Nside");
        info.longitudeOfFirstGridPointInDegrees = 45.0;
        info.orderingConvention = md.get<std::string>("orderingConvention");
    }
    catch (const message::MetadataException& err) {
        std::ostringstream oss;
        oss << "HealPixGridInfo creator ::fromCodes: Require keys \"Nside\" (int) and \"orderingConvention\" (string)  "
               "to "
               "prepare a codes sample. Otherwise pass down \""
            << ATLAS_NAMED_GRID_KEY << "\" to retrieve all information from atlas.";
        std::throw_with_nested(EncodeGrib2Exception(oss.str(), Here()));
    }

    return info;
}


//-----------------------------------------------------------------------------

GridInfo gridInfoFromMetadata(const GridInfoCreationOptions& options, const message::Metadata& md) {
    if (auto atlasNamedGrid = md.getOpt<std::string>(ATLAS_NAMED_GRID_KEY); atlasNamedGrid) {
        return dispatchGridType(
            [&atlasNamedGrid, &options, &md](auto gridTypeTag) {
                using GridTypeTag = decltype(gridTypeTag);
                return GridInfo{GridInfoCreator<GridTypeTag::value>::fromAtlas(*atlasNamedGrid, options, md)};
            },
            atlasNamedGridToGridType(*atlasNamedGrid));
    }
    if (auto codesGridType = md.getOpt<std::string>("gridType"); codesGridType) {
        return dispatchGridType(
            [&codesGridType, &options, &md](auto gridTypeTag) {
                using GridTypeTag = decltype(gridTypeTag);
                return GridInfo{GridInfoCreator<GridTypeTag::value>::fromCodes(*codesGridType, options, md)};
            },
            eccodesGridTypeToGridType(*codesGridType));
    }

    std::ostringstream oss;
    oss << "gridInfoFromMetadata: Metadata does not contain key \"" << ATLAS_NAMED_GRID_KEY
        << "\" or \"gridType\" (eccodes concept)";
    throw EncodeGrib2Exception(oss.str(), Here());
}


//-----------------------------------------------------------------------------


}  // namespace multio::action::encodeGrib2

/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "GridDownloader.h"

#include "GribEncoder.h"

#include "atlas/grid/Grid.h"
#include "atlas/grid/Iterator.h"
#include "atlas/grid/SpecRegistry.h"
#include "atlas/library.h"
#include "atlas/parallel/mpi/mpi.h"

#include "eckit/exception/Exceptions.h"
#include "eckit/filesystem/TmpFile.h"
#include "eckit/io/StdFile.h"
#include "eckit/log/Log.h"
#include "eckit/mpi/Comm.h"

#include "multio/message/Glossary.h"
#include "multio/util/Substitution.h"

namespace {
const std::unordered_map<std::string, int> latParamIds{
    {"T", 250003}, {"U", 250005}, {"V", 250007}, {"W", 250009}, {"F", 250011}};

const std::unordered_map<std::string, int> lonParamIds{
    {"T", 250004}, {"U", 250006}, {"V", 250008}, {"W", 250010}, {"F", 250012}};

std::unique_ptr<multio::action::GribEncoder> createEncoder(const multio::config::ComponentConfiguration& compConf) {
    if (not compConf.parsedConfig().has("grid-downloader-template")) {
        eckit::Log::warning() << "Multio GridDownloader: configuration is missing the coordinates encoder template, "
                                 "running without encoding!"
                              << std::endl;
        return nullptr;
    }
    const auto tmplPath = compConf.parsedConfig().getString("grid-downloader-template");

    eckit::AutoStdFile fin{compConf.multioConfig().replaceCurly(tmplPath)};

    int err = 0;
    auto encoder = std::make_unique<multio::action::GribEncoder>(
        codes_handle_new_from_file(nullptr, fin, PRODUCT_GRIB, &err), compConf.parsedConfig());
    if (err != 0) {
        std::ostringstream oss;
        oss << "Could not create a GribEncoder for the grid coordinates due to an error in ecCodes: " << err;
        throw eckit::SeriousBug(oss.str(), Here());
    }

    return encoder;
}

atlas::Grid readGrid(const std::string& name) {
    atlas::mpi::Scope mpi_scope("self");
    return atlas::Grid{name};
}

std::string getUnstructuredGridType(const multio::config::ComponentConfiguration& compConf) {
    return multio::util::replaceCurly(
        compConf.parsedConfig().getString("unstructured-grid-type"),
        [](std::string_view replace) {
            std::string lookUpKey{replace};
            char* env = ::getenv(lookUpKey.c_str());
            return env ? std::optional<std::string>{env} : std::optional<std::string>{};
        });
}

}  // namespace

namespace multio::action {

AtlasInstance::AtlasInstance() {
    atlas::initialize();
};

AtlasInstance::~AtlasInstance() {
    atlas::finalize();
};

AtlasInstance& AtlasInstance::instance() {
    static AtlasInstance singleton;
    return singleton;
};

using message::glossary;

GridDownloader::GridDownloader(const config::ComponentConfiguration& compConf) :
    encoder_(createEncoder(compConf)), templateMetadata_(), gridCoordinatesCache_(), gridUIDCache_() {

    ScopedAtlasInstance scopedAtlasInstance;

    populateUIDCache(compConf);

    if (encoder_ != nullptr) {
        initTemplateMetadata();

        if (compConf.parsedConfig().has("unstructured-grid-type")) {
            const auto unstructuredGridType = getUnstructuredGridType(compConf);

            if (unstructuredGridType.find("ORCA") != std::string::npos) {
                eckit::Log::info() << "Grid downloader initialized, starting ORCA grid download!" << std::endl;

                downloadOrcaGridCoordinates(compConf);
            }
        }
    }
}

std::optional<GridCoordinates> GridDownloader::getGridCoords(const GridDownloader::DomainType& gridId, int startDate,
                                                             int startTime) {
    if (gridCoordinatesCache_.count(gridId) == 0) {
        return {};
    }

    auto coords = gridCoordinatesCache_.at(gridId);

    gridCoordinatesCache_.erase(gridId);

    auto encodedLat = encodeMessage(std::move(coords.Lat), startDate, startTime);
    auto encodedLon = encodeMessage(std::move(coords.Lon), startDate, startTime);

    return GridCoordinates{encodedLat, encodedLon};
}

void GridDownloader::populateUIDCache(const config::ComponentConfiguration& compConf) {
    if (compConf.parsedConfig().has("unstructured-grid-type")) {
        atlas::mpi::Scope mpi_scope("self");

        const auto baseGridName = getUnstructuredGridType(compConf);

        for (auto const& unstructuredGridSubtype : {"T", "U", "V", "W", "F"}) {
            const auto completeGridName = baseGridName + "_" + unstructuredGridSubtype;

            const auto gridSpec = atlas::grid::SpecRegistry::get(completeGridName);
            const auto gridUID = gridSpec.getString("uid");

            gridUIDCache_.emplace(std::piecewise_construct, std::tuple(std::string(unstructuredGridSubtype) + " grid"),
                                  std::tuple(gridUID));
        }
    }
}

void GridDownloader::initTemplateMetadata() {
    templateMetadata_.set("step", 0);
    templateMetadata_.set(glossary().typeOfLevel, "oceanSurface");
    templateMetadata_.set(glossary().level, 0);
    templateMetadata_.set("category", "ocean-grid-coordinate");
    templateMetadata_.set(glossary().bitsPerValue, 16);
    templateMetadata_.set(glossary().precision, "double");
}

multio::message::Metadata GridDownloader::createMetadataFromCoordsData(size_t gridSize,
                                                                       const std::string& unstructuredGridSubtype,
                                                                       const std::string& gridUID, int paramId) {
    multio::message::Metadata md(templateMetadata_);

    md.set<std::int64_t>(glossary().globalSize, gridSize);
    md.set(glossary().unstructuredGridSubtype, unstructuredGridSubtype);
    md.set(glossary().uuidOfHGrid, gridUID);

    md.set("param", paramId);

    return md;
}

void GridDownloader::downloadOrcaGridCoordinates(const config::ComponentConfiguration& compConf) {
    const auto baseGridName = getUnstructuredGridType(compConf);

    for (auto const& unstructuredGridSubtype : {"T", "U", "V", "W", "F"}) {
        const auto completeGridName = baseGridName + "_" + unstructuredGridSubtype;

        eckit::Log::info() << "Multio GridDownloader: starting download for grid: " << completeGridName << std::endl;

        const atlas::Grid grid = readGrid(completeGridName);

        eckit::Log::info() << "Multio GridDownloader: grid " << completeGridName << " downloaded!" << std::endl;

        const auto gridUID = grid.uid();

        if (encoder_ != nullptr) {
            const auto gridSize = grid.size();

            std::vector<double> lon(grid.size());
            std::vector<double> lat(grid.size());
            size_t n{0};

            for (const auto p : grid.lonlat()) {
                lon[n] = p.lon();
                lat[n] = p.lat();
                ++n;
            }

            eckit::Log::info() << "Multio GridDownloader: data from " << completeGridName << " extracted!" << std::endl;

            auto latMetadata = createMetadataFromCoordsData(gridSize, unstructuredGridSubtype, gridUID,
                                                            latParamIds.at(unstructuredGridSubtype));
            auto lonMetadata = createMetadataFromCoordsData(gridSize, unstructuredGridSubtype, gridUID,
                                                            lonParamIds.at(unstructuredGridSubtype));

            multio::message::Message latMessage{{multio::message::Message::Tag::Field, {}, {}, std::move(latMetadata)},
                                                {lat.data(), grid.size() * sizeof(double)}};
            multio::message::Message lonMessage{{multio::message::Message::Tag::Field, {}, {}, std::move(lonMetadata)},
                                                {lon.data(), grid.size() * sizeof(double)}};

            gridCoordinatesCache_.emplace(std::piecewise_construct,
                                          std::tuple(std::string(unstructuredGridSubtype) + " grid"),
                                          std::tuple(latMessage, lonMessage));

            eckit::Log::info() << "Multio GridDownloader: cached data for grid: " << completeGridName << std::endl;
        }
    }
}

multio::message::Message GridDownloader::encodeMessage(multio::message::Message&& message, int startDate,
                                                       int startTime) {
    multio::message::Message msg{message};
    msg.header().acquireMetadata();

    msg.modifyMetadata().set(glossary().startDate, startDate);
    msg.modifyMetadata().set(glossary().startTime, startTime);

    return encoder_->encodeOceanCoordinates(std::move(msg), message::Metadata{});
}

}  // namespace multio::action

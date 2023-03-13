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

#include "multio/util/ConfigurationContext.h"

#include "atlas/grid/Grid.h"
#include "atlas/grid/Iterator.h"
#include "atlas/library.h"

#include "eckit/exception/Exceptions.h"
#include "eckit/filesystem/TmpFile.h"
#include "eckit/io/StdFile.h"
#include "eckit/log/Log.h"
#include "eckit/mpi/Comm.h"

namespace {
const std::unordered_map<std::string, int> latParamIds{
    {"T", 250003}, {"U", 250005}, {"V", 250007}, {"W", 250009}, {"F", 250011}};

const std::unordered_map<std::string, int> lonParamIds{
    {"T", 250004}, {"U", 250006}, {"V", 250008}, {"W", 250010}, {"F", 250012}};

std::unique_ptr<multio::action::GribEncoder> createEncoder(const multio::util::ConfigurationContext& confCtx) {
    if (not confCtx.config().has("grid-downloader-template")) {
        eckit::Log::warning() << "Multio GridDownloader: configuration is missing the coordinates encoder template, running without encoding!" << std::endl;
        return nullptr;
    }
    const auto tmplPath = confCtx.config().getString("grid-downloader-template");

    eckit::AutoStdFile fin{confCtx.replaceCurly(tmplPath)};

    int err = 0;
    auto encoder = std::make_unique<multio::action::GribEncoder>(
        codes_handle_new_from_file(nullptr, fin, PRODUCT_GRIB, &err), confCtx.config());
    if (err != 0) {
        std::ostringstream oss;
        oss << "Could not create a GribEncoder for the grid coordinates due to an error in ecCodes: " << err;
        throw eckit::SeriousBug(oss.str(), Here());
    }

    return encoder;
}
}  // namespace

namespace multio {
namespace action {

GridDownloader::GridDownloader(const util::ConfigurationContext& confCtx) :
    encoder_(createEncoder(confCtx)), templateMetadata_(), gridCoordinatesCache_(), gridUIDCache_() {
    initTemplateMetadata();

    if (confCtx.config().has("grid-type")) {
        const auto gridType = confCtx.config().getString("grid-type");
        if (gridType.find("ORCA") != std::string::npos) { 
            eckit::Log::info() << "Grid downloader initialized, starting ORCA grid download!" << std::endl;

            downloadOrcaGridCoordinates(confCtx);
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

void GridDownloader::initTemplateMetadata() {
    templateMetadata_.set("step", 0);
    templateMetadata_.set("typeOfLevel", "oceanSurface");
    templateMetadata_.set("level", 0);
    templateMetadata_.set("category", "ocean-grid-coordinate");
    templateMetadata_.set("bitsPerValue", 16);
    templateMetadata_.set("precision", "double");
}

multio::message::Metadata GridDownloader::createMetadataFromCoordsData(size_t gridSize, const std::string& gridSubtype,
                                                                       const std::string& gridUID, int paramId) {
    multio::message::Metadata md(templateMetadata_);

    md.set("globalSize", gridSize);
    md.set("gridSubtype", gridSubtype);
    md.set("uuidOfHGrid", gridUID);

    md.set("param", paramId);

    return md;
}

void GridDownloader::downloadOrcaGridCoordinates(const util::ConfigurationContext& confCtx) {
    atlas::initialize();

    const auto baseGridName = confCtx.config().getString("grid-type");
    for (auto const& gridSubtype : {"T", "U", "V", "W", "F"}) {
        const auto completeGridName = baseGridName + "_" + gridSubtype;

        eckit::Log::info() << "Multio GridDownloader: starting download for grid: " << completeGridName << std::endl;

        auto& originalComm = eckit::mpi::comm();
        eckit::mpi::setCommDefault("self");

        const atlas::Grid grid(completeGridName);

        eckit::mpi::setCommDefault(originalComm.name().c_str());

        eckit::Log::info() << "Multio GridDownloader: grid " << completeGridName << " downloaded!" << std::endl;

        const auto gridUID = grid.uid();

        gridUIDCache_.emplace(std::piecewise_construct, std::tuple(std::string(gridSubtype) + " grid"), std::tuple(gridUID));
        
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

            auto latMetadata = createMetadataFromCoordsData(gridSize, gridSubtype, gridUID, latParamIds.at(gridSubtype));
            auto lonMetadata = createMetadataFromCoordsData(gridSize, gridSubtype, gridUID, lonParamIds.at(gridSubtype));

            multio::message::Message latMessage{{multio::message::Message::Tag::Field, {}, {}, std::move(latMetadata)},
                                                {lat.data(), grid.size() * sizeof(double)}};
            multio::message::Message lonMessage{{multio::message::Message::Tag::Field, {}, {}, std::move(lonMetadata)},
                                                {lon.data(), grid.size() * sizeof(double)}};

            gridCoordinatesCache_.emplace(std::piecewise_construct, std::tuple(std::string(gridSubtype) + " grid"),
                                        std::tuple(latMessage, lonMessage));

            eckit::Log::info() << "Multio GridDownloader: cached data for grid: " << completeGridName << std::endl;
        }
    }

    atlas::finalize();
}

multio::message::Message GridDownloader::encodeMessage(multio::message::Message&& message, int startDate,
                                                       int startTime) {
    multio::message::Metadata md{message.metadata()};
    md.set("startDate", startDate);
    md.set("startTime", startTime);

    auto updateMessage = message.modifyMetadata(std::move(md));

    return encoder_->encodeOceanCoordinates(std::move(updateMessage));
}

}  // namespace action
}  // namespace multio

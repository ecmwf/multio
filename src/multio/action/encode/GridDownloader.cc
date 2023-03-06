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

#include "atlas-orca/Library.h"
#include "atlas-orca/util/AtlasIOReader.h"
#include "atlas-orca/util/Download.h"
#include "atlas-orca/util/OrcaData.h"
#include "atlas/util/Config.h"

#include "eckit/exception/Exceptions.h"
#include "eckit/filesystem/TmpFile.h"
#include "eckit/io/StdFile.h"

namespace {
const std::unordered_map<std::string, int> latParamIds{
    {"T", 250003}, {"U", 250005}, {"V", 250007}, {"W", 250009}, {"F", 250011}};

const std::unordered_map<std::string, int> lonParamIds{
    {"T", 250004}, {"U", 250006}, {"V", 250008}, {"W", 250010}, {"F", 250012}};

void checkConfigAndThrowOnError(const multio::util::ConfigurationContext& confCtx) {
    if (not confCtx.config().has("grid-type")) {
        throw eckit::SeriousBug("Grid downloader configuration is missing the grid type.", Here());
    }

    const auto gridType = confCtx.config().getString("grid-type");
    if (gridType.find("ORCA") == std::string::npos) {
        throw eckit::SeriousBug("Grid downloader only supports ORCA grids.", Here());
    }

    if (not confCtx.config().has("grid-downloader-template")) {
        throw eckit::SeriousBug("Grid downloader configuration is missing the coordinates encoder template.", Here());
    }
}

std::unique_ptr<multio::action::GribEncoder> createEncoder(const multio::util::ConfigurationContext& confCtx) {
    const auto tmplPath = confCtx.config().getString("grid-downloader-template");

    eckit::AutoStdFile fin{confCtx.replaceCurly(tmplPath)};

    int err = 0;
    auto encoder = std::make_unique<multio::action::GribEncoder>(
        codes_handle_new_from_file(nullptr, fin, PRODUCT_GRIB, &err), confCtx.config());
    if (err != 0) {
        // TODO: throw something
    }

    return encoder;
}

void checkOrcaGridConfigAndThrowOnError(const atlas::util::Config& specs, const std::string& completeGridName) {
    if (not specs.has(completeGridName)) {
        throw eckit::SeriousBug("Configured grid not supported: " + completeGridName, Here());
    }

    const auto gridSpec = specs.getSubConfiguration(completeGridName);
    if (not gridSpec.has("uid")) {
        throw eckit::SeriousBug("Configured grid: " + completeGridName + " has no uid!", Here());
    }
    if (not gridSpec.has("data")) {
        throw eckit::SeriousBug("Configured grid: " + completeGridName + " has no data!", Here());
    }
}

atlas::orca::OrcaData downloadOrcaGrid(const std::string& downloadLink) {
    const eckit::TmpFile downloadedGrid{};
    const auto gridLength = atlas::orca::download(downloadLink, downloadedGrid);
    if (gridLength == 0) {
        // TODO: throw something
    }

    atlas::orca::OrcaData data;
    atlas::orca::AtlasIOReader{atlas::util::NoConfig()}.read(downloadedGrid.path(), data);

    return data;
}
}  // namespace

namespace multio {
namespace action {

GridDownloader::GridDownloader(const util::ConfigurationContext& confCtx) :
    templateMetadata_(), gridCoordinatesCache_() {
    checkConfigAndThrowOnError(confCtx);

    auto encoder = createEncoder(confCtx);

    initTemplateMetadata();

    downloadOrcaGridCoordinates(confCtx, std::move(encoder));
}

std::optional<GridDownloader::GridCoordinates> GridDownloader::getGridCoords(const GridDownloader::DomainType& gridId) {
    if (gridCoordinatesCache_.count(gridId) == 0) {
        return {};
    }

    auto coords = gridCoordinatesCache_.at(gridId);

    gridCoordinatesCache_.erase(gridId);

    return coords;
}

void GridDownloader::initTemplateMetadata() {
    templateMetadata_.set("step", 0);
    templateMetadata_.set("typeOfLevel", 0);  // TODO: where to get this?
    templateMetadata_.set("levtype", 0);      // TODO: where to get this?
    templateMetadata_.set("level", 0);
    templateMetadata_.set("timeStep", 0);                        // TODO: is this really necessary
    templateMetadata_.set("startDate", 0);                       // TODO: set this correctly
    templateMetadata_.set("date", 0);                            // TODO: set this correctly
    templateMetadata_.set("startTime", 0);                       // TODO: is this really necessary
    templateMetadata_.set("time", 0);                            // TODO: set this correctly
    templateMetadata_.set("category", "ocean-grid-coordinate");  // TODO: is this really necessary?
    templateMetadata_.set("missingValue", 0.0);                  // TODO: is this really necessary
    templateMetadata_.set("bitsPerValue", 16);
    templateMetadata_.set("toAllServers", true);  // TODO: is this really necessary?
    templateMetadata_.set("class", 0);            // TODO: where to get this?
    templateMetadata_.set("stream", 0);           // TODO: where to get this?
    templateMetadata_.set("expver", 0);           // TODO: where to get this?
    templateMetadata_.set("type", 0);             // TODO: where to get this?
    templateMetadata_.set("precision", "double");
}

multio::message::Metadata GridDownloader::createMetadataFromCoordsData(size_t gridSize, const std::string& gridSubtype,
                                                                       const std::string& gridUID,
                                                                       std::string&& paramName, int paramId) {
    multio::message::Metadata md(templateMetadata_);

    md.set("globalSize", gridSize);
    md.set("gridSubtype", gridSubtype);
    md.set("uuidOfHGrid", gridUID);

    md.set("name", std::move(paramName));  // TODO: is this really necessary

    md.set("param", paramId);

    return md;
}

void GridDownloader::downloadOrcaGridCoordinates(const util::ConfigurationContext& confCtx,
                                                 std::unique_ptr<GribEncoder> encoder) {
    const atlas::util::Config specs{atlas::orca::Library::instance().gridsPath()};

    const auto baseGridName = confCtx.config().getString("grid-type");
    for (auto const& gridSubtype : {"T", "U", "V", "W", "F"}) {
        const auto completeGridName = baseGridName + "_" + gridSubtype;

        checkOrcaGridConfigAndThrowOnError(specs, completeGridName);

        const auto gridSpec = specs.getSubConfiguration(completeGridName);

        const auto gridUID = gridSpec.getString("uid");

        const auto gridDownloadLink = gridSpec.getString("data");
        const auto data = downloadOrcaGrid(gridDownloadLink);

        const auto gridSize = data.lat.size() * data.lon.size();  // TODO: is this correct?
        auto latMetadata = createMetadataFromCoordsData(
            gridSize, gridSubtype, gridUID, std::string("lat") + "_" + gridSubtype, latParamIds.at(gridSubtype));
        auto lonMetadata = createMetadataFromCoordsData(
            gridSize, gridSubtype, gridUID, std::string("lon") + "_" + gridSubtype, lonParamIds.at(gridSubtype));

        multio::message::Message latMessage{{multio::message::Message::Tag::Field, {}, {}, std::move(latMetadata)},
                                            {data.lat.data(), data.lat.size() * sizeof(data.lat[0])}};
        multio::message::Message lonMessage{{multio::message::Message::Tag::Field, {}, {}, std::move(lonMetadata)},
                                            {data.lon.data(), data.lon.size() * sizeof(data.lon[0])}};

        auto encodedLat = encoder->encodeOceanCoordinates(std::move(latMessage));
        auto encodedLon = encoder->encodeOceanCoordinates(std::move(lonMessage));

        gridCoordinatesCache_.emplace(std::piecewise_construct, std::tuple(std::string(gridSubtype) + " grid"),
                                      std::tuple(encodedLat, encodedLon));
    }
}

}  // namespace action
}  // namespace multio
/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "OrcaGridDownloader.h"

#include "GribEncoder.h"

#include "multio/util/ConfigurationContext.h"

#include "atlas/util/Config.h"
#include "atlas-orca/Library.h"
#include "atlas-orca/util/Download.h"
#include "atlas-orca/util/OrcaData.h"
#include "atlas-orca/util/AtlasIOReader.h"

#include "eckit/exception/Exceptions.h"
#include "eckit/filesystem/TmpFile.h"
#include "eckit/io/StdFile.h"

namespace multio {
namespace action {

OrcaGridDownloader::OrcaGridDownloader(const util::ConfigurationContext& confCtx) :
    gridCoordinatesCache_()
{
    if (not confCtx.config().has("grid-downloader-template")) {
        throw eckit::SeriousBug("Grid downloader configuration is missing the coordinates encoder template.", Here());
    }

    const auto tmplPath = confCtx.config().getString("grid-downloader-template");
    eckit::AutoStdFile fin{confCtx.replaceCurly(tmplPath)};
    int err = 0;
    auto encoder = std::make_unique<GribEncoder>(codes_handle_new_from_file(nullptr, fin, PRODUCT_GRIB, &err), confCtx.config());
    if (err != 0) {
        //TODO: throw something
    }

    const atlas::util::Config specs{atlas::orca::Library::instance().gridsPath()};

    const auto baseGridName = confCtx.config().getString("grid-type");
    for (auto const& gridSubtype : {"T", "U", "V", "W", "F"}) {
        const auto completeGridName = baseGridName + "_" + gridSubtype;
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

        const auto gridUID = gridSpec.getString("uid");
        const auto gridDownloadLink = gridSpec.getString("data");
        const eckit::TmpFile downloadedGrid{};
        const auto gridLength = atlas::orca::download(gridDownloadLink, downloadedGrid);
        if (gridLength == 0) {
            //TODO: throw something 
        }

        atlas::orca::OrcaData data;
        atlas::orca::AtlasIOReader{atlas::util::NoConfig()}.read(downloadedGrid.path(), data);

        //TODO: create messages from the lat & long
        multio::message::Metadata latMetadata{};

        latMetadata.set("globalSize", data.lat.size() * data.lon.size()); //TODO: is this correct?
        latMetadata.set("step", 0);
        latMetadata.set("typeOfLevel", 0); //TODO: where to get this?
        latMetadata.set("levtype", 0); //TODO: where to get this?
        latMetadata.set("level", 0);
        latMetadata.set("gridSubtype", gridSubtype);
        latMetadata.set("uuidOfHGrid", gridUID); //TODO: update GribEncoder to use this instead of the GridInfo value.
        latMetadata.set("timeStep", 0); //TODO: is this really necessary
        latMetadata.set("startDate", 0); //TODO: set this correctly
        latMetadata.set("date", 0); //TODO: set this correctly
        latMetadata.set("startTime", 0); //TODO: is this really necessary
        latMetadata.set("time", 0); //TODO: set this correctly
        latMetadata.set("category", "ocean-grid-coordinate"); //TODO: is this really necessary?
        latMetadata.set("missingValue", 0.0); //TODO: is this really necessary
        latMetadata.set("bitsPerValue", 16);
        latMetadata.set("toAllServers", true); //TODO: is this really necessary?
        latMetadata.set("class", 0); //TODO: where to get this?
        latMetadata.set("stream", 0); //TODO: where to get this?
        latMetadata.set("expver", 0); //TODO: where to get this?
        latMetadata.set("type", 0); //TODO: where to get this?

        multio::message::Metadata lonMetadata(latMetadata);

        latMetadata.set("name", std::string("lat") + gridSubtype); //TODO: is this really necessary
        lonMetadata.set("name", std::string("lon") + gridSubtype); //TODO: is this really necessary
        
        latMetadata.set("param", 0); //TODO: compute this correctly from nemo-to-grib.yaml
        lonMetadata.set("param", 0); //TODO: compute this correctly from nemo-to-grib.yaml

        const multio::message::Message latMessage{{multio::message::Message::Tag::Field, {}, {}, std::move(latMetadata)}, {data.lat.data(), data.lat.size() * sizeof(data.lat[0])}};
        const multio::message::Message lonMessage{{multio::message::Message::Tag::Field, {}, {}, std::move(lonMetadata)}, {data.lon.data(), data.lon.size() * sizeof(data.lon[0])}};

        //TODO: send the messages through the encoder
        // auto encodedLat = encoder->encodeOceanLatitudes("");
        // auto encodedLong = encoder->encodeOceanLongitudes("");
        //TODO: add the messages to the cache
        //gridCoordinatesCache_.emplace(std::piecewise_construct, std::tuple(gridUID), std::tuple(encodedLat, encodedLong));
    }
}

} // namespace action
} // namespace multio
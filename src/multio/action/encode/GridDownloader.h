/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/// @author Razvan Aguridan

/// @date Mar 2023

#pragma once

#include <optional>
#include <unordered_map>

#include "eckit/memory/NonCopyable.h"

#include "multio/config/ComponentConfiguration.h"
#include "multio/message/Message.h"

namespace multio::action::encode {

/**
 * The Grid downloader may be used from different actions and should not call atlas::initialize() or atlas::finalize()
 * multiple times. Atlas has no ref counting - other libraries may be still using atlas, hence we should not finalize it
 * during the run. Moreover after finalizing, some Logs seems to get deallocated and can not be used after a second
 * initialization.
 */
class AtlasInstance : private eckit::NonCopyable {
private:  // methods
    AtlasInstance();

public:  // methods
    static AtlasInstance& instance();
    ~AtlasInstance();
};

class ScopedAtlasInstance {
private:  // methods
    AtlasInstance& instance_;

public:  // methods
    ScopedAtlasInstance() : instance_{AtlasInstance::instance()} {};
};


class GribEncoder;

struct GridCoordinates {
    using LatitudeCoord = multio::message::Message;
    using LongitudeCoord = multio::message::Message;

    GridCoordinates(LatitudeCoord lat, LongitudeCoord lon) : Lat(lat), Lon(lon) {}

    LatitudeCoord Lat;
    LongitudeCoord Lon;
};

class GridDownloader : eckit::NonCopyable {
public:
    using DomainType = std::string;
    using GridUIDType = std::string;

    explicit GridDownloader(const config::ComponentConfiguration& compConf);

    std::optional<GridCoordinates> getGridCoords(const DomainType& gridId, int startDate, int startTime);
    std::optional<GridUIDType> getGridUID(const DomainType& gridId) const {
        return (gridUIDCache_.count(gridId) != 0) ? gridUIDCache_.at(gridId) : std::optional<GridUIDType>{};
    }

private:
    using GridCoordinateCache = std::unordered_map<DomainType, GridCoordinates>;
    using GridUIDCache = std::unordered_map<DomainType, GridUIDType>;

    void populateUIDCache(const config::ComponentConfiguration& compConf);

    void initTemplateMetadata();
    multio::message::Metadata createMetadataFromCoordsData(size_t gridSize, const std::string& unstructuredGridSubtype,
                                                           const std::string& gridUID, int paramId);
    void downloadOrcaGridCoordinates(const config::ComponentConfiguration& compConf);
    multio::message::Message encodeMessage(multio::message::Message&& message, int startDate, int startTime);

    const std::unique_ptr<GribEncoder> encoder_;
    multio::message::Metadata templateMetadata_;
    GridCoordinateCache gridCoordinatesCache_;
    GridUIDCache gridUIDCache_;
};


}  // namespace multio::action::encode

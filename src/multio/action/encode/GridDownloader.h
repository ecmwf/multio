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

#include "eckit/memory/NonCopyable.h"

#include "multio/message/Message.h"

#include <unordered_map>

namespace multio {

namespace util {
class ConfigurationContext;
}  // namespace util

namespace action {

class GribEncoder;

class GridDownloader : eckit::NonCopyable {
public:
    using LatitudeCoord = multio::message::Message;
    using LongitudeCoord = multio::message::Message;
    using GridCoordinates = std::pair<LatitudeCoord, LongitudeCoord>;
    using DomainType = std::string;

    explicit GridDownloader(const util::ConfigurationContext& confCtx);

    std::optional<GridCoordinates> getGridCoords(const DomainType& gridId);

private:
    using GridCoordinateCache = std::unordered_map<DomainType, GridCoordinates>;

    void initTemplateMetadata();
    multio::message::Metadata createMetadataFromCoordsData(size_t gridSize, const std::string& gridSubtype,
                                                           const std::string& gridUID, std::string&& paramName,
                                                           int paramId);
    void downloadOrcaGridCoordinates(const util::ConfigurationContext& confCtx, std::unique_ptr<GribEncoder> encoder);

    multio::message::Metadata templateMetadata_;
    GridCoordinateCache gridCoordinatesCache_;
};

}  // namespace action
}  // namespace multio
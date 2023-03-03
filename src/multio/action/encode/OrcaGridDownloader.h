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
} //namespace util

namespace action {

//TODO: rename the class to grid downloader ?
class OrcaGridDownloader : eckit::NonCopyable {
public:
    explicit OrcaGridDownloader(const util::ConfigurationContext& confCtx);

    //TODO: provide accessor function for cache

private:
    using LatitudeCoord = multio::message::Message;
    using LongitudeCoord = multio::message::Message;
    using GridCoordinates = std::pair<LatitudeCoord, LongitudeCoord>;
    using GridUID = std::string;
    using GridCoordinateCache = std::unordered_map<GridUID, GridCoordinates>;

    GridCoordinateCache gridCoordinatesCache_;
};

} // namespace action
} // namespace multio
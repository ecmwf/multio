/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#pragma once

// This functionality is supposed has been added to allow a fast migration for encoding AIFS output via python.
// It is ment to be removed in the future an hence added as a separate file here
//
// Header-only and templated to inject known types of EncodeMtg2.h...
// Code is copied from mtg2 tool

#include <regex>
#include <string>

#include "multio/datamod/AtlasGeo.h"
#include "multio/datamod/ContainerInterop.h"
#include "multio/datamod/core/Record.h"
#include "multio/message/Metadata.h"
#include "multio/message/Parametrization.h"

namespace multio::action::extract {

namespace dm = multio::datamod;

struct AtlasGeoSetter {
    using GridTypeFunction = std::function<void(const std::string& scope, const std::string& gridName)>;

    static void handleGG(const std::string& scope, const std::string& gridName) {
        message::Metadata md{{scope, true}};

        auto geoGG = datamod::scopeRecord(dm::GeoGGRecord{}, scope);

        dm::setKeysFromAtlas(geoGG, gridName);

        dm::dumpRecord(geoGG, md);

        message::Parametrization::instance().update(md);
    }

    static void handleGrid(const std::string& scope, const std::string& gridName) {
        const static std::vector<std::pair<std::string, GridTypeFunction>> gridMap{
            {"^\\s*[FON]\\d+\\s*$", &handleGG},
            // {"^\\s*L\\d+x\\d+\\s*$", &updateRegularLatLonGrid}
        };

        const auto gridFunc = std::find_if(gridMap.cbegin(), gridMap.cend(), [&gridName](const auto& item) {
            std::regex r{item.first};
            return std::regex_match(gridName, r);
        });

        if (gridFunc != gridMap.cend()) {
            gridFunc->second(scope, gridName);
        }
        else {
            std::ostringstream oss;
            oss << "GeoFromAtlas: No grid function specified for grid " << gridName << std::endl;
            throw multio::message::MetadataException(oss.str(), Here());
            // TODO throw
        }
    }
};


}  // namespace multio::action::extract

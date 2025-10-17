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
#include "multio/datamod/MarsMiscGeo.h"
#include "multio/datamod/core/Record.h"
#include "multio/datamod/types/GridType.h"
#include "multio/message/Metadata.h"
#include "multio/message/Parametrization.h"

namespace multio::action::extract {

namespace dm = multio::datamod;

struct AtlasGeoSetter {
    using GridTypeFunction = std::function<void(const std::string& scope, const std::string& gridName)>;

    template <typename GGRec>
    static void handleGG(const std::string& scope, const std::string& gridName) {
        message::Metadata md{{scope, true}};

        auto geoGG = datamod::scopeRecord(GGRec{}, scope);

        dm::setKeysFromAtlas(geoGG, gridName);

        dm::dumpRecord(geoGG, md);

        message::Parametrization::instance().update(md);
    }

    static void handleLL(const std::string& scope, const std::string& gridName) {
        message::Metadata md{{scope, true}};

        auto geoLL = datamod::scopeRecord(dm::GeoRegularLLRecord{}, scope);

        dm::setKeysFromAtlas(geoLL, gridName);

        dm::dumpRecord(geoLL, md);

        message::Parametrization::instance().update(md);
    }

    static void handleGrid(const std::string& scope, const std::string& gridName) {
        auto gridType = dm::gridTypeFromGrid(gridName);
        switch (gridType) {
            case dm::GridType::ReducedGG:
                handleGG<dm::GeoReducedGGRecord>(scope, gridName);
                break;
            case dm::GridType::RegularGG:
                handleGG<dm::GeoRegularGGRecord>(scope, gridName);
                break;
            case dm::GridType::RegularLL:
                handleLL(scope, gridName);
                break;
            default: {
                std::ostringstream oss;
                oss << "GeoFromAtlas: No grid function specified for grid " << gridName << std::endl;
                throw multio::message::MetadataException(oss.str(), Here());
            }
        }
    }
};


}  // namespace multio::action::extract

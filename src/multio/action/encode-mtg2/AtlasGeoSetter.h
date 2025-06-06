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
#include <stdexcept>
#include <string>

#include "atlas/grid.h"
#include "atlas/library.h"
#include "atlas/parallel/mpi/mpi.h"

#include "multio/action/encode-mtg2/EncodeMtg2.h"
#include "multio/message/Glossary.h"
#include "multio/message/Metadata.h"
#include "multio/message/Parametrization.h"

namespace multio::action::encode_mtg2::extract {

using message::Parametrization;

atlas::Grid readGrid(const std::string& name) {
    // atlas::mpi::Scope mpi_scope("self");
    return atlas::Grid{name};
}

template <class GridType>
GridType createGrid(const std::string& atlasNamedGrid) {
    const atlas::Grid grid = readGrid(atlasNamedGrid);
    auto structuredGrid = atlas::StructuredGrid(grid);
    return GridType(structuredGrid);
}


struct AtlasGeoSetter {
    using GridTypeFunction = std::function<void(const std::string& prefix, const std::string& gridName)>;

    static void handleGG(const std::string& prefix, const std::string& gridName) {

        message::BaseMetadata& global = message::Parametrization::instance().get();

        // std::regex reducedGaussianMatch{"^\\s*[O]\\d+\\s*$"};
        // bool isReducedGaussian = std::regex_match(gridName, reducedGaussianMatch);

        // TODO use MarsKeySet in future...
        const auto gaussianGrid = createGrid<atlas::GaussianGrid>(gridName);

        // getAndSet(h, geom, "truncateDegrees", "truncate-degrees");
        using namespace message::Mtg2;
        global.set(prefix + std::string(gg::numberOfParallelsBetweenAPoleAndTheEquator), gaussianGrid.N());
        // getAndSetIfNonZero(h, geom, "numberOfPointsAlongAMeridian", "number-of-points-along-a-meridian");

        {
            auto it = gaussianGrid.lonlat().begin();

            global.set(prefix + std::string(gg::latitudeOfFirstGridPointInDegrees), (*it)[1]);
            global.set(prefix + std::string(gg::longitudeOfFirstGridPointInDegrees), (*it)[0]);

            it += gaussianGrid.size() - 1;
            global.set(prefix + std::string(gg::latitudeOfLastGridPointInDegrees), (*it)[1]);

            const auto equator = gaussianGrid.N();
            const auto maxLongitude = gaussianGrid.x(gaussianGrid.nx(equator) - 1, equator);
            global.set(prefix + std::string(gg::longitudeOfLastGridPointInDegrees), maxLongitude);
        }

        {
            auto tmp = gaussianGrid.nx();
            std::vector<long> pl(tmp.size(), 0);
            for (int i = 0; i < tmp.size(); ++i) {
                pl[i] = long(tmp[i]);
            }
            global.set(prefix + std::string(gg::pl), std::move(pl));
        }
    }

    static void handleGrid(const std::string& prefix, const std::string& gridName) {
        const static std::vector<std::pair<std::string, GridTypeFunction>> gridMap{
            {"^\\s*[FON]\\d+\\s*$", &handleGG},
            // {"^\\s*L\\d+x\\d+\\s*$", &updateRegularLatLonGrid}
        };

        const auto gridFunc = std::find_if(gridMap.cbegin(), gridMap.cend(), [&gridName](const auto& item) {
            std::regex r{item.first};
            return std::regex_match(gridName, r);
        });

        if (gridFunc != gridMap.cend()) {
            gridFunc->second(prefix, gridName);
        }
        else {
            std::ostringstream oss;
            oss << "GeoFromAtlas: No grid function specified for grid " << gridName << std::endl;
            throw multio::message::MetadataException(oss.str(), Here());
            // TODO throw
        }
    }
};


}  // namespace multio::action::encode_mtg2::extract

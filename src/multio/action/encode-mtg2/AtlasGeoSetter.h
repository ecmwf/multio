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
#include "multio/message/Metadata.h"

namespace multio::action::encode_mtg2::extract {

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

template <typename T>
std::string arrayToJSONString(const std::vector<T>& arr) {
    std::ostringstream oss;
    bool first = true;
    oss << "[";
    for (const auto& v : arr) {
        if (first) {
            first = false;
        }
        else {
            oss << ", ";
        }
        oss << v;
    }
    oss << "]";
    return oss.str();
}


template <typename MultiOMDict>
struct AtlasGeoSetter {
    using GridTypeFunction = std::function<void(const std::string& gridName, const multio::message::Metadata&,
                                                MultiOMDict&, MultiOMDict&)>;

    static void handleGG(const std::string& gridName, const multio::message::Metadata& md, MultiOMDict& mars_dict,
                         MultiOMDict& par_dict) {
        MultiOMDict geom{MultiOMDictKind::ReducedGG};

        // std::regex reducedGaussianMatch{"^\\s*[O]\\d+\\s*$"};
        // bool isReducedGaussian = std::regex_match(gridName, reducedGaussianMatch);

        // TODO use MarsKeySet in future...
        // std::string gridName = md.get<std::string>("grid");
        const auto gaussianGrid = createGrid<atlas::GaussianGrid>(gridName);

        // getAndSet(h, geom, "truncateDegrees", "truncate-degrees");
        geom.set("numberOfParallelsBetweenAPoleAndTheEquator", std::to_string(gaussianGrid.N()).c_str());
        // getAndSetIfNonZero(h, geom, "numberOfPointsAlongAMeridian", "number-of-points-along-a-meridian");

        {
            auto it = gaussianGrid.lonlat().begin();

            geom.set("latitudeOfFirstGridPointInDegrees", std::to_string((*it)[1]).data());
            geom.set("longitudeOfFirstGridPointInDegrees", std::to_string((*it)[0]).data());

            it += gaussianGrid.size() - 1;
            geom.set("latitudeOfLastGridPointInDegrees", std::to_string((*it)[1]).data());

            const auto equator = gaussianGrid.N();
            const auto maxLongitude = gaussianGrid.x(gaussianGrid.nx(equator) - 1, equator);
            geom.set("longitudeOfLastGridPointInDegrees", std::to_string(maxLongitude).data());
        }

        {
            auto tmp = gaussianGrid.nx();
            std::vector<long> pl(tmp.size(), 0);
            for (int i = 0; i < tmp.size(); ++i) {
                pl[i] = long(tmp[i]);
            }
            geom.set("pl", arrayToJSONString(pl).data());
        }

        mars_dict.set("repres", "gg");
        par_dict.set_geometry(std::move(geom));
    }
    static void handleGrid(const std::string& gridName, const multio::message::Metadata& md, MultiOMDict& mars_dict,
                           MultiOMDict& par_dict) {
        const static std::vector<std::pair<std::string, GridTypeFunction>> gridMap{
            {"^\\s*[FON]\\d+\\s*$", &handleGG},
            // {"^\\s*L\\d+x\\d+\\s*$", &updateRegularLatLonGrid}
        };

        const auto gridFunc = std::find_if(gridMap.cbegin(), gridMap.cend(), [&gridName](const auto& item) {
            std::regex r{item.first};
            return std::regex_match(gridName, r);
        });

        if (gridFunc != gridMap.cend()) {
            gridFunc->second(gridName, md, mars_dict, par_dict);
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

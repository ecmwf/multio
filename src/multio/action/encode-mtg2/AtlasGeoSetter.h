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

#include "multio/datamod/DataModelling.h"
#include "multio/datamod/ContainerInterop.h"
#include "multio/datamod/MarsMiscGeo.h"
#include "multio/message/Metadata.h"
#include "multio/message/Parametrization.h"

namespace multio::action::extract {

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

template<typename KS>
struct SetKeysFromAtlas {
    void operator()(datamod::KeyValueSet<KS>& ks, const std::string& gridName) const {
        // No error for now...
        // std::ostringstream oss;
        // oss << "GeoFromAtlas: functionality not implemented yet for " << gridName << std::endl;
        // throw multio::message::MetadataException(oss.str(), Here());
    }
};
template<>
struct SetKeysFromAtlas<datamod::KeySet<datamod::GeoGG>> {
    void operator()(datamod::KeyValueSet<datamod::KeySet<datamod::GeoGG>>& geoGG, const std::string& gridName) const {
        using namespace datamod;
        
        const auto gaussianGrid = createGrid<atlas::GaussianGrid>(gridName);
        key<GeoGG::NumberOfParallelsBetweenAPoleAndTheEquator>(geoGG).set(gaussianGrid.N());
        
        {
            auto it = gaussianGrid.lonlat().begin();

            key<GeoGG::LatitudeOfFirstGridPointInDegrees>(geoGG).set((*it)[1]);
            key<GeoGG::LongitudeOfFirstGridPointInDegrees>(geoGG).set((*it)[0]);

            it += gaussianGrid.size() - 1;
            key<GeoGG::LatitudeOfLastGridPointInDegrees>(geoGG).set((*it)[1]);

            const auto equator = gaussianGrid.N();
            const auto maxLongitude = gaussianGrid.x(gaussianGrid.nx(equator) - 1, equator);
            key<GeoGG::LongitudeOfLastGridPointInDegrees>(geoGG).set(maxLongitude);
        }

        {
            auto tmp = gaussianGrid.nx();
            std::vector<long> pl(tmp.size(), 0);
            for (int i = 0; i < tmp.size(); ++i) {
                pl[i] = long(tmp[i]);
            }
            key<GeoGG::Pl>(geoGG).set(std::move(pl));
        }

        // Explicitly validate after manual setting
        alterAndValidate(geoGG);
    }
};

template<typename KS>
void setKeysFromAtlas(datamod::KeyValueSet<KS>& gks, const std::string& gridName) {
    return SetKeysFromAtlas<KS>{}(gks, gridName);
}


struct AtlasGeoSetter {
    using GridTypeFunction = std::function<void(const std::string& scope, const std::string& gridName)>;

    static void handleGG(const std::string& scope, const std::string& gridName) {
        using namespace datamod;
        message::Metadata md{{scope, true}};

        auto geoGG = reify(keySet<GeoGG>().scoped(scope));
        
        setKeysFromAtlas(geoGG, gridName);

        write(geoGG, md);
        
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

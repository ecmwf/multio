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

#include <string>

#include "atlas/grid.h"
#include "atlas/library.h"
#include "atlas/parallel/mpi/mpi.h"

#include "multio/datamod/ContainerInterop.h"
#include "multio/datamod/DataModelling.h"
#include "multio/datamod/MarsMiscGeo.h"

namespace multio::datamod {

using message::Parametrization;

inline atlas::Grid readGrid(const std::string& name) {
    // atlas::mpi::Scope mpi_scope("self");
    return atlas::Grid{name};
}

template <class GridType>
GridType createGrid(const std::string& atlasNamedGrid) {
    const atlas::Grid grid = readGrid(atlasNamedGrid);
    auto structuredGrid = atlas::StructuredGrid(grid);
    return GridType(structuredGrid);
}

template <typename KS>
struct SetKeysFromAtlas {
    void operator()(datamod::KeyValueSet<KS>& ks, const std::string& gridName) const {}
};

template <>
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

template <typename KS>
void setKeysFromAtlas(datamod::KeyValueSet<KS>& gks, const std::string& gridName) {
    return SetKeysFromAtlas<KS>{}(gks, gridName);
}


inline Geometry makeGeometry(const MarsKeyValueSet& mars, bool inferGeo = true) {
    return std::visit(
        [&](const auto& geoKS) -> Geometry {
            // Create it unscoped...
            KeyValueSet<std::decay_t<decltype(geoKS)>> ret{};
            const auto& grid = key<MarsKeys::GRID>(mars);
            if (inferGeo && grid.has()) {
                setKeysFromAtlas(ret, grid.get());
            }
            return ret;
        },
        getGeometryKeySet(mars));
}

}  // namespace multio::datamod

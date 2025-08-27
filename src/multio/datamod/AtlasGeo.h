/*
 * (C) Copyright 2025- ECMWF.
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
#include "multio/datamod/MarsMiscGeo.h"
#include "multio/datamod/core/Record.h"

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

template <typename Record>
struct SetKeysFromAtlas {
    void operator()(Record& ks, const std::string& gridName) const {}
};

template <typename RecordType>
struct SetKeysFromAtlas<ScopedRecord<RecordType>> {
    void operator()(ScopedRecord<RecordType>& rec, const std::string& gridName) const {
        SetKeysFromAtlas<RecordType>{}(static_cast<RecordType&>(rec), gridName);
    }
};

template <>
struct SetKeysFromAtlas<GeoGGRecord> {
    void operator()(GeoGGRecord& geoGG, const std::string& gridName) const {
        const auto gaussianGrid = createGrid<atlas::GaussianGrid>(gridName);
        geoGG.numberOfParallelsBetweenAPoleAndTheEquator.set(gaussianGrid.N());

        {
            auto it = gaussianGrid.lonlat().begin();

            geoGG.latitudeOfFirstGridPointInDegrees.set((*it)[1]);
            geoGG.longitudeOfFirstGridPointInDegrees.set((*it)[0]);

            it += gaussianGrid.size() - 1;
            geoGG.latitudeOfLastGridPointInDegrees.set((*it)[1]);

            const auto equator = gaussianGrid.N();
            const auto maxLongitude = gaussianGrid.x(gaussianGrid.nx(equator) - 1, equator);
            geoGG.longitudeOfLastGridPointInDegrees.set(maxLongitude);
        }

        {
            auto tmp = gaussianGrid.nx();
            std::vector<long> pl(tmp.size(), 0);
            for (int i = 0; i < tmp.size(); ++i) {
                pl[i] = long(tmp[i]);
            }
            geoGG.pl.set(std::move(pl));
        }
        
        // Explicitly validate after manual setting
        applyRecordDefaults(geoGG);
        validateRecord(geoGG);
    }
};

template <typename Record>
void setKeysFromAtlas(Record& rec, const std::string& gridName) {
    return SetKeysFromAtlas<Record>{}(rec, gridName);
}


inline ScopedGeometry makeGeometry(const FullMarsRecord& mars) {
    auto res = getGeometryRecord(mars);
    std::visit(
        [&](auto& rec) {
            if (mars.grid.isSet()) {
                setKeysFromAtlas(rec, mars.grid.get());
            }
        },
        res);
    return res;
}

inline Geometry makeUnscopedGeometry(const FullMarsRecord& mars) {
    return std::visit([](auto&& geo) -> Geometry { return unscopeRecord(std::move(geo));}, makeGeometry(mars));
}

}  // namespace multio::datamod

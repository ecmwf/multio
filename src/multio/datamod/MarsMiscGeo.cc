/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "multio/datamod/MarsMiscGeo.h"
#include "multio/datamod/core/DataModellingException.h"


namespace multio::datamod {

ScopedGeometry getGeometryRecord(const FullMarsRecord& mars) {
    const auto& grid = mars.grid;
    const auto& trunc = mars.truncation;
    GridType gridType;

    if (grid.isSet()) {
        gridType = gridTypeFromGrid(grid.get());
    }
    else if (trunc.isSet()) {
        gridType = GridType::SH;
    }
    else {
        std::ostringstream oss;
        oss << "Neither grid nor truncation is given. Can not determine gridType to create a geometry record.";
        throw DataModellingException(oss.str(), Here());
    }

    switch (gridType) {
        case GridType::RegularGG: {
            std::string scope = std::string("geo-") + grid.get();
            return scopeRecord(GeoRegularGGRecord{}, scope);
        }
        case GridType::ReducedGG: {
            std::string scope = std::string("geo-") + grid.get();
            return scopeRecord(GeoReducedGGRecord{}, scope);
        }
        case GridType::RegularLL: {
            std::string scope = std::string("geo-") + grid.get();
            return scopeRecord(GeoRegularLLRecord{}, scope);
        }
        case GridType::HEALPix: {
            std::string scope = std::string("geo-") + grid.get();
            return scopeRecord(GeoHEALPixRecord{}, scope);
        }
        case GridType::SH: {
            std::string scope = std::string("geo-TCO") + std::to_string(trunc.get());
            return scopeRecord(GeoSHRecord{}, scope);
        }
        default:
            throw DataModellingException(
                std::string("getGeometryRecord: Unhandled gridType ") + TypeDumper<GridType>::dump(gridType), Here());
    }
}

GridType GeoSHRecord::gridType() const {
    if (this->stretchingFactor.isSet()) {
        if (this->latitudeOfStretchingPoleInDegrees.isSet() || this->longitudeOfStretchingPoleInDegrees.isSet()) {
            return GridType::StretchedRotatedSH;
        }
        else {
            return GridType::StretchedSH;
        }
    }
    else {
        return GridType::SH;
    }
};

GridType gridTypeFromGeometry(const Geometry& geo) {
    return std::visit(eckit::Overloaded{[&](const GeoRegularGGRecord&) { return GridType::RegularGG; },
                                        [&](const GeoReducedGGRecord&) { return GridType::ReducedGG; },
                                        [&](const GeoRegularLLRecord&) { return GridType::RegularLL; },
                                        [&](const GeoHEALPixRecord&) { return GridType::HEALPix; },
                                        [&](const GeoSHRecord& g) { return g.gridType(); }},
                      geo);
}

}  // namespace multio::datamod


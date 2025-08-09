/*
 * (C) Copyright 1996- ECMWF.
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

ScopedGeometry getGeometryRecord(const MarsRecord& mars) {
    const auto& grid = mars.grid;
    const auto& trunc = mars.truncation;
    const auto& repres = mars.repres;

    switch (repres.get()) {
        case Repres::GG: {
            std::string scope = std::string("geo-") + grid.get();
            return scopeRecord(GeoGGRecord{}, scope);
        }
        case Repres::HEALPix: {
            std::string scope = std::string("geo-") + grid.get();
            return scopeRecord(GeoHEALPixRecord{}, scope);
        }
        case Repres::SH: {
            std::string scope = std::string("geo-TCO") + std::to_string(trunc.get());
            return scopeRecord(GeoSHRecord{}, scope);
        }
        default:
            throw DataModellingException(
                std::string("getGeometryRecord: Unhandled repres ") + TypeDumper<Repres>::dump(repres.get()), Here());
    }
}

}  // namespace multio::datamod


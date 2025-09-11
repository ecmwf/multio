/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "GridType.h"

#include "multio/datamod/core/DataModellingException.h"

namespace multio::datamod {

std::string DumpType<GridType>::dump(GridType v) {
    switch (v) {
        case GridType::RegularGG:
            return "regular_gg";
        case GridType::ReducedGG:
            return "reduced_gg";
        case GridType::HEALPix:
            return "healpix";
        case GridType::RegularLL:
            return "regular_ll";
        case GridType::SH:
            return "sh";
        case GridType::StretchedSH:
            return "stretched_sh";
        case GridType::StretchedRotatedSH:
            return "stretched_rotated_sh";
        default:
            throw DataModellingException("DumpType<GridType>::dump: Unexpected value for GridType", Here());
    }
}

GridType gridTypeFromGrid(const std::string& grid) {
    if (grid.empty()) {
        throw datamod::DataModellingException("represFromGrid: empty grid", Here());
    }

    switch (grid[0]) {
        case 'F':
            return GridType::RegularGG;
        case 'O':
            return GridType::ReducedGG;
        case 'H':
            return GridType::HEALPix;
        case 'N':
            if (grid.rfind("x") == std::string::npos) {
                return GridType::RegularGG;
            }
            else {
                return GridType::RegularLL;
            }
        default:
            throw DataModellingException(std::string("represFromGrid: invalid grid: ") + grid, Here());
    }
}

GridType ParseType<GridType>::parse(const std::string& val) {
    if (val == "reduced_gg") {
        return GridType::ReducedGG;
    }
    if (val == "regular_gg") {
        return GridType::RegularGG;
    }
    if (val == "regular_ll") {
        return GridType::RegularLL;
    }
    if (val == "sh") {
        return GridType::SH;
    }
    if (val == "stretched_sh") {
        return GridType::SH;
    }
    if (val == "stretched_rotated_sh") {
        return GridType::SH;
    }
    if (val == "healpix") {
        return GridType::HEALPix;
    }
    throw DataModellingException(std::string("ParseType<GridType>::parse Unknown value for GridType: ") + val, Here());
}


}  // namespace multio::datamod


namespace multio::util {

void Print<datamod::GridType>::print(PrintStream& ps, const datamod::GridType& t) {
    util::print(ps, datamod::TypeDumper<datamod::GridType>::dump(t));
}

}  // namespace multio::util

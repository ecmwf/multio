/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "Repres.h"

#include "multio/datamod/core/DataModellingException.h"

namespace multio::datamod {

std::string DumpType<Repres>::dump(Repres v) {
    switch (v) {
        case Repres::GG:
            return "gg";
        case Repres::HEALPix:
            return "HEALPix";
        case Repres::LL:
            return "ll";
        case Repres::SH:
            return "sh";
        default:
            throw DataModellingException("DumpType<Repres>::dump: Unexpected value for Repres", Here());
    }
}

Repres represFromGrid(const std::string& grid) {
    if (grid.empty()) {
        throw datamod::DataModellingException("represFromGrid: empty grid", Here());
    }

    switch (grid[0]) {
        case 'F':
        case 'O':
            return Repres::GG;
        case 'H':
            return Repres::HEALPix;
        case 'N':
            if (grid.rfind("x") == std::string::npos) {
                return Repres::GG;
            }
            else {
                return Repres::LL;
            }
        case 'L':
            return Repres::LL;
        default:
            throw DataModellingException(std::string("represFromGrid: invalid grid: ") + grid, Here());
    }
}

Repres ParseType<Repres>::parse(const std::string& val) {
    if (val == "gg") {
        return Repres::GG;
    }
    if (val == "ll") {
        return Repres::LL;
    }
    if (val == "sh") {
        return Repres::SH;
    }
    if (val == "HEALPix") {
        return Repres::HEALPix;
    }
    throw DataModellingException(std::string("ParseType<Repres>::parse Unknown value for Repres: ") + val, Here());
}


}  // namespace multio::datamod


namespace multio::util {

void Print<datamod::Repres>::print(PrintStream& ps, const datamod::Repres& t) {
    util::print(ps, datamod::TypeDumper<datamod::Repres>::dump(t));
}

}  // namespace multio::util

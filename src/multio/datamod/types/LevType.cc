/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "LevType.h"

#include "multio/datamod/core/DataModellingException.h"


namespace multio::datamod {


std::string DumpType<LevType>::dump(LevType v) {
    switch (v) {
        case LevType::ML:
            return "ml";
        case LevType::PL:
            return "pl";
        case LevType::PV:
            return "pv";
        case LevType::PT:
            return "pt";
        case LevType::SOL:
            return "sol";
        case LevType::SFC:
            return "sfc";
        case LevType::O2D:
            return "o2d";
        case LevType::O3D:
            return "o3d";
        case LevType::HL:
            return "hl";
        case LevType::HHL:
            return "hhl";
        case LevType::HPL:
            return "hpl";
        case LevType::AL:
            return "al";
        case LevType::WV:
            return "wv";
        case LevType::DP:
            return "dp";
        case LevType::CAT:
            return "cat";
        case LevType::LAYER:
            return "layer";
        default:
            throw DataModellingException(
                "DumpType<LevType>::dump: Unexpected enum value for LevType " + std::to_string(std::int64_t(v)),
                Here());
    }
}

LevType ParseType<LevType>::parse(const std::string& val) {
    if (val == "ml") {
        return LevType::ML;
    }
    if (val == "pl") {
        return LevType::PL;
    }
    if (val == "pv") {
        return LevType::PV;
    }
    if (val == "pt") {
        return LevType::PT;
    }
    if (val == "sol") {
        return LevType::SOL;
    }
    if (val == "sfc") {
        return LevType::SFC;
    }
    if (val == "o2d") {
        return LevType::O2D;
    }
    if (val == "o3d") {
        return LevType::O3D;
    }
    if (val == "hl") {
        return LevType::HL;
    }
    if (val == "hhl") {
        return LevType::HHL;
    }
    if (val == "hpl") {
        return LevType::HPL;
    }
    if (val == "al") {
        return LevType::AL;
    }
    if (val == "wv") {
        return LevType::WV;
    }
    if (val == "dp") {
        return LevType::DP;
    }
    if (val == "cat") {
        return LevType::CAT;
    }
    if (val == "layer") {
        return LevType::LAYER;
    }
    throw DataModellingException(std::string("ParseType<LevType>::parse Unknown value for LevType: ") + val, Here());
}

const std::vector<LevType>& allLevTypes() {
    static const std::vector<LevType> all{
        LevType::ML, LevType::PL,  LevType::PV,  LevType::PT, LevType::SOL, LevType::SFC, LevType::O2D, LevType::O3D,
        LevType::HL, LevType::HHL, LevType::HPL, LevType::AL, LevType::WV,  LevType::DP,  LevType::CAT, LevType::LAYER};

    return all;
}

}  // namespace multio::datamod


namespace multio::util {

void util::Print<datamod::LevType>::print(PrintStream& ps, const datamod::LevType& t) {
    util::print(ps, datamod::TypeDumper<datamod::LevType>::dump(t));
}

}  // namespace multio::util


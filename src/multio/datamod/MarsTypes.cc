/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "multio/datamod/MarsTypes.h"

#include "eckit/utils/Overloaded.h"
#include "metkit/mars/Param.h"

#include "multio/datamod/core/DataModellingException.h"

#include <regex>


namespace multio::datamod {

IntOrString DumpType<TimeDuration>::dump(const TimeDuration& td) {
    using Ret = std::variant<std::int64_t, std::string>;
    return td.visit(eckit::Overloaded{
        [&](const std::chrono::hours& h) -> Ret { return h.count(); },
        [&](const std::chrono::seconds& s) -> Ret { return std::to_string(s.count()) + std::string("s"); }});
}

TimeDuration ParseType<TimeDuration>::parse(std::int64_t hours) noexcept {
    return std::chrono::hours{hours};
}
TimeDuration ParseType<TimeDuration>::parse(const std::string& val) {
    // TODO align these units with util/DateTime.h ??
    static const std::regex timeRegex(R"((\d+)([hs]?))");  // number + optional 'h' or 's'

    std::smatch match;
    if (std::regex_match(val, match, timeRegex)) {
        long value = std::stol(match[1]);  // numeric part
        std::string unit = match[2];       // unit part (may be empty)

        if (unit.empty()) {
            unit = "h";  // default to hours
        }

        switch (unit[0]) {
            case 'h':
                return std::chrono::hours{value};
            case 's':
                return std::chrono::seconds{value};
            default:
                throw DataModellingException(std::string("Invalid unit in time duration: ") + val, Here());
        }
    }
    else {
        throw DataModellingException(std::string("Invalid time duration: ") + val, Here());
    }
}


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
    throw DataModellingException(std::string("ParseType<Repres>::parse Unknown value for Repres: ") + val, Here());
}


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
    throw DataModellingException(std::string("ParseType<LevType>::parse Unknown value for LevType: ") + val, Here());
}

const std::vector<LevType>& allLevTypes() {
    static const std::vector<LevType> all{LevType::ML,  LevType::PL,  LevType::PV,  LevType::PT,
                                          LevType::SOL, LevType::SFC, LevType::O2D, LevType::O3D,
                                          LevType::HL,  LevType::HHL, LevType::HPL, LevType::AL};

    return all;
}


namespace mapper {

std::int64_t ParamMapper::parse(std::int64_t v) noexcept {
    return v;
}
std::int64_t ParamMapper::dump(std::int64_t v) noexcept {
    return v;
}
std::int64_t ParamMapper::parse(const std::string& str) {
    return metkit::Param(str).paramId();
}

}  // namespace mapper

}  // namespace multio::datamod


namespace multio::util {

void Print<datamod::Repres>::print(PrintStream& ps, const datamod::Repres& t) {
    util::print(ps, datamod::TypeDumper<datamod::Repres>::dump(t));
}

void util::Print<datamod::LevType>::print(PrintStream& ps, const datamod::LevType& t) {
    util::print(ps, datamod::TypeDumper<datamod::LevType>::dump(t));
}

void util::Print<datamod::TimeDuration>::print(PrintStream& ps, const datamod::TimeDuration& t) {
    util::print(ps, datamod::TypeDumper<datamod::TimeDuration>::dump(t));
}


}  // namespace multio::util


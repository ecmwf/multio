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

#include "multio/datamod/DataModellingException.h"

#include <regex>


namespace multio::datamod {


IntOrString WriteSpec<TimeDuration>::write(const TimeDuration& td) {
    using Ret = std::variant<std::int64_t, std::string>;
    return std::visit(eckit::Overloaded{[&](const std::chrono::hours& h) -> Ret { return h.count(); },
                                        [&](const std::chrono::seconds& s) -> Ret {
                                            return std::to_string(s.count()) + std::string("s");
                                        }},
                      td);
}

TimeDuration ReadSpec<TimeDuration>::read(std::int64_t hours) noexcept {
    return std::chrono::hours{hours};
}
TimeDuration ReadSpec<TimeDuration>::read(const std::string& val) {
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


std::string WriteSpec<Repres>::write(Repres v) {
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
            throw DataModellingException("WriteSpec<Repres>::write: Unexpected value for Repres", Here());
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

Repres ReadSpec<Repres>::read(const std::string& val) {
    if (val == "gg") {
        return Repres::GG;
    }
    if (val == "ll") {
        return Repres::LL;
    }
    if (val == "sh") {
        return Repres::SH;
    }
    throw DataModellingException(std::string("ReadSpec<Repres>::read Unknown value for Repres: ") + val, Here());
}


std::string WriteSpec<LevType>::write(LevType v) {
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
                "WriteSpec<LevType>::write: Unexpected enum value for LevType " + std::to_string(std::int64_t(v)),
                Here());
    }
}

LevType ReadSpec<LevType>::read(const std::string& val) {
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
    throw DataModellingException(std::string("ReadSpec<LevType>::read Unknown value for LevType: ") + val, Here());
}

const std::vector<LevType>& allLevTypes() {
    static const std::vector<LevType> all{LevType::ML,  LevType::PL,  LevType::PV,  LevType::PT,
                                          LevType::SOL, LevType::SFC, LevType::O2D, LevType::O3D,
                                          LevType::HL,  LevType::HHL, LevType::HPL, LevType::AL};

    return all;
}


namespace mapper {

std::int64_t ParamMapper::read(std::int64_t v) noexcept {
    return v;
}
std::int64_t ParamMapper::write(std::int64_t v) noexcept {
    return v;
}
std::int64_t ParamMapper::read(const std::string& str) {
    return metkit::Param(str).paramId();
}

}  // namespace mapper

}  // namespace multio::datamod


namespace multio::util {

void Print<datamod::Repres>::print(std::ostream& os, const datamod::Repres& t) {
    util::print(os, datamod::Writer<datamod::Repres>::write(t));
}

void util::Print<datamod::LevType>::print(std::ostream& os, const datamod::LevType& t) {
    util::print(os, datamod::Writer<datamod::LevType>::write(t));
}

void util::Print<datamod::TimeDuration>::print(std::ostream& os, const datamod::TimeDuration& t) {
    util::print(os, datamod::Writer<datamod::TimeDuration>::write(t));
}


}  // namespace multio::util


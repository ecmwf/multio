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


std::string WriteSpec<TimeDuration>::write(const TimeDuration& td) noexcept {
    return std::visit(
        eckit::Overloaded{[&](const std::chrono::hours& h) {
                              // The fortran encoder currently don't accepts units - after that we should remove them
                              // quickly return std::to_string(h.count()) + std::string("h");
                              return std::to_string(h.count());
                          },
                          [&](const std::chrono::seconds& s) { return std::to_string(s.count()) + std::string("s"); }},
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
    throw DataModellingException(std::string("RepresMapper::read Unknown value for Repres: ") + val, Here());
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

std::ostream& operator<<(std::ostream& os, const Repres& t) {
    os << Writer<Repres>::write(t);
    return os;
}

std::ostream& operator<<(std::ostream& os, const TimeDuration& t) {
    os << Writer<TimeDuration>::write(t);
    return os;
}


}  // namespace multio::datamod

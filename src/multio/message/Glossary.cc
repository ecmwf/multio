/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "Glossary.h"

#include "metkit/mars/Param.h"

#include "multio/message/MetadataException.h"

#include <regex>


namespace multio::message {

const Glossary& glossary() {
    return Glossary::instance();
}


EncoderCacheMarsKeyValueSet getEncoderCacheKeys(const MarsKeyValueSet& mk) {
    auto cacheKeys = read(EncoderCacheMarsKeySet{}, mk);

    acquire(cacheKeys);
    validate(cacheKeys);

    const auto& levtype = key<MarsKeys::LEVTYPE>(cacheKeys);
    if (!levtype.isMissing() && levtype.get() == "ml") {
        key<MarsKeys::LEVELIST>(cacheKeys).setMissing();
    }

    return cacheKeys;
}


std::tuple<GridType, std::string> gridTypeAndScopeFromGrid(const std::string& grid) {
    if (grid.empty())
        throw MetadataException("empty grid", Here());
    using Ret = std::tuple<GridType, std::string>;

    auto fail = [&](auto loc) { return MetadataException(std::string("invalid grid: ") + grid, std::move(loc)); };

    auto handleGG = [&]() -> Ret { return Ret{GridType::GG, std::string("geo-") + grid + std::string("-")}; };
    auto handleLL = [&]() -> Ret { return Ret{GridType::LL, std::string("geo-") + grid + std::string("-")}; };

    switch (grid[0]) {
        case 'F':
            return handleGG();
        case 'O':
            return handleGG();
        case 'N':
            if (grid.rfind("x") == std::string::npos) {
                return handleGG();
            }
            else {
                return handleLL();
            }
        default:
            throw fail(Here());
    }
    throw fail(Here());
}


namespace mapper {

std::string TimeDurationMapper::write(const TimeDuration& td) const noexcept {
    return std::visit(
        eckit::Overloaded{[&](const std::chrono::hours& h) { return std::to_string(h.count()) + std::string("h"); },
                          [&](const std::chrono::seconds& s) { return std::to_string(s.count()) + std::string("s"); }},
        td);
}

TimeDuration TimeDurationMapper::read(std::int64_t hours) const noexcept {
    return std::chrono::hours{hours};
}
TimeDuration TimeDurationMapper::read(const std::string& val) const {
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
                throw MetadataException(std::string("Invalid unit in time duration: ") + val, Here());
        }
    }
    else {
        throw MetadataException(std::string("Invalid time duration: ") + val, Here());
    }
}


std::int64_t ParamMapper::read(std::int64_t v) const noexcept {
    return v;
}
std::int64_t ParamMapper::write(std::int64_t v) const noexcept {
    return v;
}
std::int64_t ParamMapper::read(const std::string& str) const {
    return metkit::Param(str).paramId();
}

}  // namespace mapper

}  // namespace multio::message

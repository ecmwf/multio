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
#include "multio/message/MetadataException.h"
#include "metkit/mars/Param.h"


namespace multio::message {

const Glossary& glossary() {
    return Glossary::instance();
}

namespace Mtg2 {

Repres parseRepres(const std::string& repres) {
    static const std::unordered_map<std::string, Repres> map{
        {"gg", Repres::GG},
        {"ll", Repres::LL},
        {"sh", Repres::SH},
    };

    if (auto search = map.find(repres); search != map.end()) {
        return search->second;
    }
    throw MetadataException(std::string("Unknown value for \"repres\": ") + repres,Here());
}

std::string toString(Repres repres) {
    switch (repres) {
        case Repres::GG: 
            return std::string{"gg"};
        case Repres::LL: 
            return std::string{"ll"};
        case Repres::SH: 
            return std::string{"sh"};
    }
    
    throw MetadataException("Unknown repres", Here());
}


std::tuple<Repres, std::string> represAndPrefixFromGridName(const std::string& gridName) {
    if (gridName.empty()) throw MetadataException("empty gridName", Here());
    using Ret = std::tuple<Repres, std::string>;
    
    auto fail = [&](auto loc){ return MetadataException(std::string("invalid gridName: ") + gridName, std::move(loc)); };
    
    auto handleGG = [&]() -> Ret {
        return Ret{Repres::GG, std::string("geo-") + gridName + std::string("-")};
    };
    auto handleLL = [&]() -> Ret {
        return Ret{Repres::LL, std::string("geo-") + gridName + std::string("-")};
    };
    auto handleSH = [&]() -> Ret {
        return Ret{Repres::SH, std::string("geo-") + gridName + std::string("-")};
    };
    
    switch (gridName[0]) {
        case 'F':
            return handleGG();
        case 'O':
            return handleGG();
        case 'N':
            if (gridName.rfind("x") == std::string::npos) {
                return handleGG();
            } else {
                return handleLL();
            }
        case 'T':
            if (gridName.rfind("TCO", 3) != std::string::npos) {
               return handleSH(); 
            }
            throw fail(Here());
        default:
            throw fail(Here());
    }
    throw fail(Here());
}


std::int64_t ParamMapper::operator()(std::int64_t v) const noexcept { return v; }
std::int64_t ParamMapper::operator()(const std::string& str) const { return metkit::Param(str).paramId(); }

}

}  // namespace multio::message

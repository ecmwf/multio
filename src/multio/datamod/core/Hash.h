/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#pragma once

#include <tuple>
#include <type_traits>

#include "multio/datamod/core/Entry.h"
#include "multio/datamod/core/Record.h"

#include "multio/util/Hash.h"


//-----------------------------------------------------------------------------
// Hashing of Records
//-----------------------------------------------------------------------------

template <>
struct std::hash<multio::datamod::UnsetType> {
    std::size_t operator()(const multio::datamod::UnsetType&) const noexcept { return 0; }
};

template <typename Val, typename Mapper>
struct std::hash<multio::datamod::Entry<Val, Mapper>> {
    std::size_t operator()(const multio::datamod::Entry<Val, Mapper>& entry) const
        noexcept(noexcept(multio::util::hash(std::declval<Val>()))) {
        return entry.visit([&](const auto& v) -> std::size_t { return multio::util::hash(v); });
    }
};

namespace multio::datamod {

// Functor to hash a record - std::hash has to be customized per record and can derive std::hash from this functor
struct HashRecord {
    template <typename RecordType, std::enable_if_t<IsRecord_v<RecordType>, bool> = true>
    std::size_t operator()(const RecordType& rec) const {
        return std::apply([&](const auto&... entryDef) { return multio::util::hashCombine(entryDef.get(rec)...); },
                          recordEntries(rec));
    }
};

}  // namespace multio::datamod

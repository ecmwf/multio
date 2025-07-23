/*
 * (C) Copyright 1996- ECMWF.
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

#include "multio/datamod/core/KeyValue.h"
#include "multio/datamod/core/KeyValueSet.h"

#include "multio/util/Hash.h"


//-----------------------------------------------------------------------------
// Hashing of KeyValueSets
//-----------------------------------------------------------------------------

template <>
struct std::hash<multio::datamod::MissingValue> {
    std::size_t operator()(const multio::datamod::MissingValue&) const noexcept { return 0; }
};

template <auto id>
struct std::hash<multio::datamod::KeyValue<id>> {
    std::size_t operator()(const multio::datamod::KeyValue<id>& kv) const
        noexcept(noexcept(multio::util::hash(std::declval<typename multio::datamod::KeyValue<id>::ValueType>()))) {
        return kv.visit([&](const auto& v) -> std::size_t { return multio::util::hash(v); });
    }
};

template <auto id, typename... KVS>
struct std::hash<std::tuple<multio::datamod::KeyValue<id>, KVS...>> {
    std::size_t operator()(const std::tuple<multio::datamod::KeyValue<id>, KVS...>& t) const
        noexcept(noexcept(multio::util::hashCombine(std::declval<multio::datamod::KeyValue<id>>(),
                                                    std::declval<KVS>()...))) {
        return std::apply([](const auto&... args) { return multio::util::hashCombine(args...); }, t);
    }
};

template <typename KeySet>
struct std::hash<multio::datamod::KeyValueSet<KeySet>> {
    std::size_t operator()(const multio::datamod::KeyValueSet<KeySet>& kvs) const
        noexcept(noexcept(multio::util::hash(std::declval<multio::datamod::KeyValueSet<KeySet>>().values))) {
        return multio::util::hash(kvs.values);
    }
};

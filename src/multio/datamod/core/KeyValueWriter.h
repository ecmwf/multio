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

#include "multio/datamod/core/Key.h"
#include "multio/datamod/core/KeySet.h"
#include "multio/datamod/core/KeyValue.h"
#include "multio/datamod/core/KeyValueSet.h"

#include "multio/util/TypeTraits.h"

namespace multio::datamod {

//-----------------------------------------------------------------------------
// Writing to other containers
//-----------------------------------------------------------------------------

// Methods to read/write tuples of KeyValues to specific types (i.e. from Metadata)
// set(KeyDefinition, KeyValue, Container)
template <typename Container>
struct KeyValueWriter {
    static constexpr bool isSpecialized = false;
};

template <typename Container>
struct BaseKeyValueWriter {
    static constexpr bool isSpecialized = true;

    template <auto id, typename KVD, typename KV, typename Cont_,
              std::enable_if_t<(IsDynamicKey_v<KVD> && IsBaseKeyValue_v<std::decay_t<KV>>
                                && std::is_base_of_v<Container, std::decay_t<Cont_>>),
                               bool>
              = true>
    static void set(KeyId<id>, const KVD& kvd, KV&& kv, Cont_& c) {
        KeyValueWriter<Container>::set(kvd, std::forward<KV>(kv), c);
    }
};


//-----------------------------------------------------------------------------
// Writing from keysets
//-----------------------------------------------------------------------------

template <auto otherId, typename... KVS>
struct KeyValueWriter<std::tuple<KeyValue<otherId>, KVS...>>
    : BaseKeyValueWriter<std::tuple<KeyValue<otherId>, KVS...>> {
    using Base = BaseKeyValueWriter<std::tuple<KeyValue<otherId>, KVS...>>;
    // using Base::set; // Not needed

    template <auto id, typename KVD, typename KV, typename KVTup,
              std::enable_if_t<(IsDynamicKey_v<std::decay_t<KVD>> && IsBaseKeyValue_v<std::decay_t<KV>>
                                && util::IsTuple_v<std::decay_t<KVTup>>
                                && util::TypeListAll_v<IsKeyValue, util::ToTypeList_t<std::decay_t<KVTup>>>),
                               bool>
              = true>
    static void set(KeyId<id>, const KVD& kvd, KV&& kv, KVTup& kvTup) {
        key<id>(kvTup).set(std::forward<KV>(kv));
    }
};

template <typename KeySet_>
struct KeyValueWriter<KeyValueSet<KeySet_>> : BaseKeyValueWriter<KeyValueSet<KeySet_>> {
    using Base = BaseKeyValueWriter<KeyValueSet<KeySet_>>;
    using BaseTup = KeyValueWriter<typename KeyValueSet<KeySet_>::TupleType>;
    // using Base::set; // Not needed

    template <auto id, typename KVD, typename KV, typename KVS,
              std::enable_if_t<(IsDynamicKey_v<std::decay_t<KVD>> && IsBaseKeyValue_v<std::decay_t<KV>>
                                && IsKeyValueSet_v<std::decay_t<KVS>>),
                               bool>
              = true>
    static void set(KeyId<id> kid, const KVD& kvd, KV&& kv, KVS& kvs) {
        BaseTup::set(kid, kvd, std::forward<KV>(kv), kvs.values);
    }
};

//-----------------------------------------------------------------------------

}  // namespace multio::datamod

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
// Reading from other containers
//-----------------------------------------------------------------------------

// Methods to read/write tuples of KeyValues to specific types (i.e. from Metadata)
// * `getByRef(DynamicKey, Container)`: Minimum to be defined
// * `getByRef(KeyId, DynamicKey, Container)`: Can be customized if KeyId is required. Otherwise only dynamic key
// information should be used to avoid too code generation for each field
// * `getByValue(DynamicKey, Container)`: Can be customized, but will use `getByRef` and perform `acquire`
// * `getByValue(KeyId, DynamicKey, Container)`: Can be customized if KeyId is required. Otherwise only dynamic key
// information should be used to avoid too code generation for each field
template <typename Container>
struct KeyValueReader {
    // Multiple methods are specialized in th reader and it will get a base class anyway.
    // For this reason it is easier ta add a flag that signals whether it has been specialized
    static constexpr bool isSpecialized = false;
};

template <typename Container>
struct BaseKeyValueReader {
    static constexpr bool isSpecialized = true;

    template <auto id, typename KVD, typename Cont_,
              std::enable_if_t<(IsDynamicKey_v<KVD> && std::is_base_of_v<Container, std::decay_t<Cont_>>), bool> = true>
    static KeyValue<id> getByRef(KeyId<id> kid, const KVD& kvd, Cont_&& c) {
        return KeyValue<id>{KeyValueReader<Container>::getByRef(kvd, std::forward<Cont_>(c))};
    }

    template <typename KVD, typename Cont_,
              std::enable_if_t<(IsDynamicKey_v<KVD> && std::is_base_of_v<Container, std::decay_t<Cont_>>), bool> = true>
    static KeyValueFromKey_t<KVD> getByValue(const KVD& kvd, Cont_&& c) {
        KeyValueFromKey_t<KVD> ret{KeyValueReader<Container>::getByRef(kvd, std::forward<Cont_>(c))};
        ret.acquire();
        return ret;
    }

    template <auto id, typename KVD, typename Cont_,
              std::enable_if_t<(IsDynamicKey_v<KVD> && std::is_base_of_v<Container, std::decay_t<Cont_>>), bool> = true>
    static KeyValue<id> getByValue(KeyId<id> kid, const KVD& kvd, Cont_&& c) {
        return KeyValue<id>{KeyValueReader<Container>::getByValue(kvd, std::forward<Cont_>(c))};
    }
};


//-----------------------------------------------------------------------------
// Reading from keysets
//-----------------------------------------------------------------------------

template <auto someId, typename... KVS>
struct KeyValueReader<std::tuple<KeyValue<someId>, KVS...>> : BaseKeyValueReader<std::tuple<KeyValue<someId>, KVS...>> {
    using Base = BaseKeyValueReader<std::tuple<KeyValue<someId>, KVS...>>;
    // using Base::getByRef; // Not needed
    using Base::getByValue;

    template <auto id, typename KVD, typename KVTup,
              std::enable_if_t<(IsDynamicKey_v<std::decay_t<KVD>> && util::IsTuple_v<std::decay_t<KVTup>>
                                && util::TypeListAll_v<IsKeyValue, util::ToTypeList_t<std::decay_t<KVTup>>>),
                               bool>
              = true>
    static KeyValue<id> getByRef(KeyId<id>, const KVD& kvd, KVTup&& kv) {
        return key<id>(std::forward<KVTup>(kv));
    }
};

template <typename KeySet_>
struct KeyValueReader<KeyValueSet<KeySet_>> : BaseKeyValueReader<KeyValueSet<KeySet_>> {
    using Base = BaseKeyValueReader<KeyValueSet<KeySet_>>;
    using BaseTup = KeyValueReader<typename KeyValueSet<KeySet_>::TupleType>;
    // using Base::getByRef; // Not needed
    using Base::getByValue;

    template <auto id, typename KVD, typename KVS,
              std::enable_if_t<(IsDynamicKey_v<std::decay_t<KVD>> && IsKeyValueSet_v<std::decay_t<KVS>>), bool> = true>
    static KeyValue<id> getByRef(KeyId<id> kid, const KVD& kvd, KVS&& kv) {
        return BaseTup::getByRef(kid, kvd, std::forward<KVS>(kv).values);
    }
};

}  // namespace multio::datamod


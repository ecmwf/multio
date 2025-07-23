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

#include <type_traits>
#include "multio/datamod/DataModellingException.h"

#include "multio/datamod/core/Key.h"
#include "multio/datamod/core/KeyDef.h"
#include "multio/datamod/core/KeySet.h"
#include "multio/datamod/core/KeySetDef.h"
#include "multio/datamod/core/KeyUtils.h"
#include "multio/datamod/core/KeyValue.h"

#include "multio/util/TypeTraits.h"

namespace multio::datamod {

//-----------------------------------------------------------------------------

// Takes a tuple of KeyDefinition and creates an instance of the keyset with all fields set to missing
template <typename DescTup,
          std::enable_if_t<(util::IsTuple_v<std::decay_t<DescTup>>
                            && util::TypeListAll_v<IsKeyDefinition, util::ToTypeList_t<std::decay_t<DescTup>>>),
                           bool>
          = true>
decltype(auto) reify(DescTup&& tup) {
    return util::map([&](const auto& kvd) -> KeyValueFromKey_t<std::decay_t<decltype(kvd)>> { return {}; },
                     std::forward<DescTup>(tup));
}


//-----------------------------------------------------------------------------


template <typename KeySet_>
struct KeyValueSet {
    using This = KeyValueSet<KeySet_>;
    using KeySetType = KeySet_;
    using TupleType = decltype(reify(std::declval<KeySetType>().keys()));

    KeySetType keySet;
    TupleType values;

    This& unscoped() {
        keySet.unscoped();
        return *this;
    };

    // Set scope in place
    This& scoped(std::optional<std::string> customScope = {}) {
        keySet.scoped(std::move(customScope));
        return *this;
    };


    // Create a new KeySet with different scope
    This makeUnscoped() const {
        This ret{*this};
        ret.unscoped();
        return ret;
    };

    // Set scope in place
    This makeScoped(std::optional<std::string> customScope = {}) const {
        This ret{*this};
        ret.scoped(customScope);
        return ret;
    };

    // Usability
    // Inline setting
    template <auto id>
    decltype(auto) key() const& {
        return datamod::key<id>(values);
    }
    template <auto id>
    decltype(auto) key() & {
        return datamod::key<id>(values);
    }
    template <auto id>
    decltype(auto) key() && {
        return datamod::key<id>(std::move(values));
    }

    template <auto id>
    decltype(auto) get() const {
        return datamod::key<id>(values).get();
    }

    template <auto id>
    decltype(auto) modify() {
        return datamod::key<id>(values).modify();
    }

    template <auto id, typename Val>
    This& set(Val&& val) {
        datamod::key<id>(values).set(std::forward<Val>(val));
        return *this;
    }

    static void alter(This& v) { AlterKeySetFunctor<KeySet_>{}(v); }
};

template <typename KeySet_>
bool operator==(const KeyValueSet<KeySet_>& lhs, const KeyValueSet<KeySet_>& rhs) noexcept {
    return lhs.values == rhs.values;
}
template <typename KeySet_>
bool operator!=(const KeyValueSet<KeySet_>& lhs, const KeyValueSet<KeySet_>& rhs) noexcept {
    return lhs.values != rhs.values;
}


template <typename T>
struct IsKeyValueSet {
    static constexpr bool value = false;
};
template <typename KeySet_>
struct IsKeyValueSet<KeyValueSet<KeySet_>> {
    static constexpr bool value = true;
};

template <typename T>
inline constexpr bool IsKeyValueSet_v = IsKeyValueSet<T>::value;


//-----------------------------------------------------------------------------


template <auto keyId, typename KVS, std::enable_if_t<IsKeyValueSet_v<std::decay_t<KVS>>, bool> = true>
decltype(auto) key(KVS&& keyValueSet) {
    return keyUtils::getById<keyId>(std::forward<KVS>(keyValueSet).values);
}


//-----------------------------------------------------------------------------


// Takes a tuple of KeyValues and converts all references to value by copying
template <typename Tup, std::enable_if_t<(util::IsTuple_v<std::decay_t<Tup>>
                                          && util::TypeListAll_v<IsAnyKeyValue, util::ToTypeList_t<std::decay_t<Tup>>>),
                                         bool>
                        = true>
Tup& acquire(Tup& tup) {
    // TODO
    // do a recursive call if contained values are KeyValueSets
    // Alternative is to create an Acquirable concept that is checked recursively
    util::forEach([](auto& kv) { kv.acquire(); }, tup);
    return tup;
}

// Takes a tuple of KeyValues and converts all references to value by copying
template <typename KVS, std::enable_if_t<(IsKeyValueSet_v<KVS>), bool> = true>
KVS& acquire(KVS& kvs) {
    acquire(kvs.values);
    return kvs;
}
}  // namespace multio::datamod

//-----------------------------------------------------------------------------

namespace multio::util {

//-----------------------------------------------------------------------------
// Extend tuple utilities (forEach, map) to KeyValueSets
//-----------------------------------------------------------------------------

// General forEach function for KeySet
template <typename Func, typename KS, std::enable_if_t<(multio::datamod::IsKeySet_v<std::decay_t<KS>>), bool> = true>
void forEach(Func&& func, KS&& ks) {
    util::forEach(std::forward<Func>(func), ks.keys());
}

// General map function for KeySet
template <typename Func, typename KS, std::enable_if_t<(multio::datamod::IsKeySet_v<std::decay_t<KS>>), bool> = true>
decltype(auto) map(Func&& func, KS&& ks) {
    return util::map(std::forward<Func>(func), ks.keys());
}

// General forEach function to iterate on KeyValueSets
template <typename Func, typename KVS,
          std::enable_if_t<(multio::datamod::IsKeyValueSet_v<std::decay_t<KVS>>), bool> = true>
void forEach(Func&& func, KVS&& kvs) {
    const auto& keys = kvs.keySet.keys();
    util::forEach(
        [&](auto&& kv) {
            func(multio::datamod::key<std::decay_t<decltype(kv)>::id>(keys), std::forward<decltype(kv)>(kv));
        },
        std::forward<KVS>(kvs).values);
}

// General map function to iterate on KeyValueSets
template <typename Func, typename KVS,
          std::enable_if_t<(multio::datamod::IsKeyValueSet_v<std::decay_t<KVS>>), bool> = true>
decltype(auto) map(Func&& func, KVS&& kvs) {
    const auto& keys = kvs.keySet.keys();
    return util::map(
        [&](auto&& kv) {
            return func(multio::datamod::key<std::decay_t<decltype(kv)>::id>(keys), std::forward<decltype(kv)>(kv));
        },
        std::forward<KVS>(kvs).values);
}

}  // namespace multio::util

namespace multio::datamod {

//-----------------------------------------------------------------------------
// Alter (apply defaults) and validation
//-----------------------------------------------------------------------------


// Takes a tuple of KeyValue and verifies that all required keys are set
template <typename KVD, typename KV,
          std::enable_if_t<(IsKeyDefinition_v<std::decay_t<KVD>> && IsKeyValue_v<std::decay_t<KV>>), bool> = true>
void validate(const KVD&, const KV& kv) {
    // Only optional tagged keys can be missing
    if constexpr (KVD::tag != KVTag::Optional) {
        if (kv.isMissing()) {
            throw DataModellingException(std::string("Missing required key: ") + key<KV::id>().keyInfo(), Here());
        }
    }

    // Check for nested validation
    if constexpr (IsKeyValueSet_v<typename std::decay_t<KV>::ValueType>) {
        if (kv.has()) {
            validate(kv.get());
        }
    }
}

// Takes a tuple of KeyValue and verifies that all required keys are set
template <typename KV, std::enable_if_t<(IsKeyValue_v<std::decay_t<KV>>), bool> = true>
void validate(const KV& kv) {
    validate(key<KV::id>(), kv);
}


// Takes a tuple of KeyValue and verifies that all required keys are set
template <typename KVD, typename KV,
          std::enable_if_t<(IsKeyDefinition_v<std::decay_t<KVD>> && IsKeyValue_v<std::decay_t<KV>>), bool> = true>
KV& alter(const KVD& kvd, KV& kv) {
    // Only optional tagged keys can be missing
    if constexpr (KVD::hasDefaultValueFunctor) {
        if (kv.isMissing()) {
            kv.set(kvd.defaultValue());
        }
    }

    // Check for nested alter recursively
    if constexpr (IsKeyValueSet_v<typename std::decay_t<KV>::ValueType>) {
        if (kv.has()) {
            alter(kv.modify());
        }
    }
    return kv;
}


// Takes a tuple of KeyValue and verifies that all required keys are set
template <typename KV, std::enable_if_t<(IsKeyValue_v<std::decay_t<KV>>), bool> = true>
KV& alter(KV& kv) {
    return alter(key<KV::id>(), kv);
}


// Takes a tuple of KeyValue and verifies that all required keys are set
template <typename KVS, std::enable_if_t<(IsKeyValueSet_v<std::decay_t<KVS>>), bool> = true>
void validate(const KVS& kvs) {
    const auto& keys = kvs.keySet.keys();
    util::forEach([&](const auto& kv) { validate(key<std::decay_t<decltype(kv)>::id>(keys), kv); }, kvs.values);
}

// Takes a tuple of KeyValue and verifies that all required keys are set
template <typename ValTup,
          std::enable_if_t<(util::IsTuple_v<std::decay_t<ValTup>>
                            && util::TypeListAll_v<IsAnyKeyValue, util::ToTypeList_t<std::decay_t<ValTup>>>),
                           bool>
          = true>
void validate(const ValTup& tup) {
    util::forEach([&](const auto& kv) { validate(kv); }, tup);
}


// Takes a tuple of KeyValue and verifies that all required keys are set
template <typename ValTup,
          std::enable_if_t<(util::IsTuple_v<std::decay_t<ValTup>>
                            && util::TypeListAll_v<IsAnyKeyValue, util::ToTypeList_t<std::decay_t<ValTup>>>),
                           bool>
          = true>
ValTup& alter(ValTup& tup) {
    util::forEach([&](const auto& kv) { alter(kv); }, tup);
    return tup;
}


// Takes a tuple of KeyValue and verifies that all required keys are set
template <typename KVS, std::enable_if_t<(IsKeyValueSet_v<std::decay_t<KVS>>), bool> = true>
KVS& alterKeys(KVS& kvs) {
    const auto& keys = kvs.keySet.keys();
    // Alter single entries first (make sure they have a default applied if given)
    util::forEach([&](auto& kv) { alter(key<std::decay_t<decltype(kv)>::id>(keys), kv); }, kvs.values);

    return kvs;
}

// Takes a tuple of KeyValue and verifies that all required keys are set
template <typename KVS, std::enable_if_t<(IsKeyValueSet_v<std::decay_t<KVS>>), bool> = true>
KVS& alter(KVS& kvs) {
    alterKeys(kvs);

    // Now call the Keyset specific alter function if given
    std::decay_t<KVS>::alter(kvs);
    return kvs;
}


// Takes a tuple of KeyValue and verifies that all required keys are set
template <typename KVS, std::enable_if_t<(IsKeyValueSet_v<std::decay_t<KVS>>), bool> = true>
KVS& alterAndValidate(KVS& kvs) {
    alter(kvs);
    validate(kvs);
    return kvs;
}

// Takes a tuple of KeyValue and verifies that all required keys are set
template <typename ValTup,
          std::enable_if_t<(util::IsTuple_v<std::decay_t<ValTup>>
                            && util::TypeListAll_v<IsAnyKeyValue, util::ToTypeList_t<std::decay_t<ValTup>>>),
                           bool>
          = true>
ValTup& alterAndValidate(ValTup& tup) {
    alter(tup);
    validate(tup);
    return tup;
}

}  // namespace multio::datamod


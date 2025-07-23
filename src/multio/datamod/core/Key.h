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

#include <string_view>
#include <type_traits>

#include "multio/datamod/DataModellingException.h"
#include "multio/datamod/ReaderWriter.h"

#include "multio/datamod/core/KeyDef.h"
#include "multio/datamod/core/KeySetDef.h"
#include "multio/datamod/core/KeyUtils.h"

#include "multio/util/PrehashedKey.h"

#include "multio/util/TypeToString.h"
#include "multio/util/TypeTraits.h"

namespace multio::datamod {

//-----------------------------------------------------------------------------

// Abstract type. Used to give back to users
struct DynKeyInfo {
    using KeyType = util::PrehashedKey<std::string>;

    virtual const KeyType& key() const = 0;
    virtual std::string keyInfo() const = 0;

    // Returns the initial scope the key was defined in
    virtual std::string_view initScope() const = 0;

    // This forces constexpr to be compiled in
    virtual const std::optional<std::string_view>& description() const = 0;
};

// Similar to BaseKeyDef but only containing the key string that is used.
// Contains all type information (type, default value, mapper) but is not fully templated with an `id`.
// Instead this information is accessed dynamically.
// This class is still abstract, but the later defined `ScopedKey` makes it fully specified.
// In general - we only need this because at some point decision was taken to have same keys prefixed in certain cases.
// It would be cleaner to avoid the prefix at all.
// 
// This type is passed around to KeyValueReader and KeyValueWriter to interact with containers.
template <typename ValueType_, typename Mapper_, KVTag tag_, bool hasDefaultValueFunctor_>
struct DynamicKey : DynKeyInfo {
    using KeyType = util::PrehashedKey<std::string>;

    using ValueType = ValueType_;
    using Mapper = Mapper_;
    using This = DynamicKey<ValueType, Mapper, tag_, hasDefaultValueFunctor_>;

    using ReadWrite = ReaderWriter<ValueType, Mapper>;

    static constexpr KVTag tag = tag_;
    static constexpr bool hasDefaultValueFunctor = hasDefaultValueFunctor_;

    // To be removed in future when glossary is refactored
    operator const KeyType&() const { return key(); }
    operator const std::string&() const { return key(); }

    virtual ValueType defaultValue() const = 0;
};

template <typename T>
struct IsDynamicKey {
    static constexpr bool value = false;
};

template <typename ValueType_, typename Mapper_, KVTag tag_, bool hasDefaultValueFunctor_>
struct IsDynamicKey<DynamicKey<ValueType_, Mapper_, tag_, hasDefaultValueFunctor_>> {
    static constexpr bool value = true;
};

template <typename T>
inline constexpr bool IsDynamicKey_v = IsDynamicKey<T>::value;


template <typename ValueType_, typename Mapper_, KVTag tag_, bool hasDefaultValueFunctor_>
struct IsKeyDefinition<DynamicKey<ValueType_, Mapper_, tag_, hasDefaultValueFunctor_>> {
    static constexpr bool value = true;
};


// The final key type that is stored in a global static const singleton and used all over the code.
// The scoping feature eventually allows rewriting the string representation of the key.
// In the later defined `StaticKeySetStore` an unscoped and scoped version is defined to avoid string operations 
// at runtime.
// This could be also extended to support different naming conventions.
template <auto id_>
struct ScopedKey
    : DynamicKey<KeyDefValueType_t<id_>, KeyDefMapper_t<id_>, KeyDefTag_v<id_>, KeyDefHasDefaultValueFunctor_v<id_>> {
    using Base = DynamicKey<KeyDefValueType_t<id_>, KeyDefMapper_t<id_>, KeyDefTag_v<id_>,
                            KeyDefHasDefaultValueFunctor_v<id_>>;
    using KeyType = typename Base::KeyType;

    using Definition = std::decay_t<decltype(keyDef<id_>())>;
    using ValueType = KeyDefValueType_t<id_>;
    using Mapper = KeyDefMapper_t<id_>;
    using This = ScopedKey<id_>;

    using ReadWrite = typename Definition::ReadWrite;

    const Base& baseRef() const noexcept { return static_cast<const Base&>(*this); };

    ScopedKey(KeyType k) : key_{std::move(k)} {}

    static const auto id = id_;
    static constexpr KVTag tag = KeyDefTag_v<id_>;

    static constexpr bool hasDefaultValueFunctor = KeyDefHasDefaultValueFunctor_v<id_>;

    // To be removed in future when glossary is refactored
    operator const KeyType&() const { return key_; }
    operator const std::string&() const { return key_; }

    const KeyType& key() const override { return key_; }
    std::string_view initScope() const override { return KeySetName_v<decltype(id_)>; }

    // Making it virtual forces the description to be added to the compilation unit...
    const std::optional<std::string_view>& description() const noexcept override {
        return keyDef<id_>().description();
    };

    ValueType defaultValue() const override {
        if constexpr (hasDefaultValueFunctor) {
            return keyDef<id_>().defaultValue();
        }
        throw DataModellingException(std::string("Critical. No default functor given for key: ") + keyInfo(), Here());
    }

    std::string keyInfo() const override {
        return std::string(key()) + std::string(" (") + util::typeToString<ValueType>() + std::string(", ")
             + toString(tag) + std::string{")"};
    }

    // Members
    KeyType key_;
};


template <typename T>
struct IsScopedKey {
    static constexpr bool value = false;
};

template <auto id>
struct IsScopedKey<ScopedKey<id>> {
    static constexpr bool value = true;
};

template <typename T>
inline constexpr bool IsScopedKey_v = IsScopedKey<T>::value;


template <auto id>
struct IsKeyDefinition<ScopedKey<id>> {
    static constexpr bool value = true;
};


template <typename KDEF,
          std::enable_if_t<IsKeyDefinition_v<std::decay_t<KDEF>> && !IsScopedKey_v<std::decay_t<KDEF>>, bool> = true>
auto toScopedKey(const KDEF& kdef) {
    return ScopedKey<std::decay_t<KDEF>::id>{kdef.key()};
}

template <typename KDEF,
          std::enable_if_t<IsKeyDefinition_v<std::decay_t<KDEF>> && !IsScopedKey_v<std::decay_t<KDEF>>, bool> = true>
auto toScopedKey(const KDEF& kdef, std::string_view scope) {
    return ScopedKey<std::decay_t<KDEF>::id>{std::string(scope) + std::string("-") + std::string(kdef.key())};
}

template <typename KDEF,
          std::enable_if_t<IsKeyDefinition_v<std::decay_t<KDEF>> && IsScopedKey_v<std::decay_t<KDEF>>, bool> = true>
auto toScopedKey(const KDEF& kdef, std::string_view scope) {
    constexpr auto id = std::decay_t<KDEF>::id;
    return ScopedKey<id>{std::string(scope) + std::string("-") + std::string(keyDef<id>().key())};
}


//-----------------------------------------------------------------------------


// Static accessor to key sets with/without scope and prehashed (may be removed in favor of constexpr in C++20)
template <typename EnumType>
struct StaticKeySetStore {
    static const auto& keys() {
        static const auto keys
            = util::map([&](const auto& kdef) { return toScopedKey(kdef); }, KeySetDefinition<EnumType>::keyDefs);
        return keys;
    }

    static const auto& scopedKeys() {
        static const auto keys = util::map([&](const auto& kdef) { return toScopedKey(kdef, KeySetName_v<EnumType>); },
                                           KeySetDefinition<EnumType>::keyDefs);
        return keys;
    }
};


template <auto keyId, typename Tup, std::enable_if_t<util::IsTuple_v<std::decay_t<Tup>>, bool> = true>
decltype(auto) key(Tup&& keySet) {
    return keyUtils::getById<keyId>(std::forward<Tup>(keySet));
}

template <auto keyId>
decltype(auto) key() {
    return key<keyId>(StaticKeySetStore<decltype(keyId)>::keys());
}

}  // namespace multio::datamod

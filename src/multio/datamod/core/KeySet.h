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
#include "multio/datamod/core/KeySetDef.h"
#include "multio/datamod/core/KeyUtils.h"

#include "multio/util/TypeToString.h"
#include "multio/util/TypeTraits.h"


namespace multio::datamod {

// KeyId tag used for overload resolution in function calls
template <auto id_>
struct KeyId {
    static constexpr auto id = id_;
};


//-----------------------------------------------------------------------------
// KeySet implementation
//-----------------------------------------------------------------------------

enum class KeySetScope : std::uint64_t
{
    None,
    Default,
    Custom,
};


template <typename Derived, typename GetKeyPolicy>
class BaseKeySet {
public:
    using This = Derived;
    using TupleType = std::decay_t<decltype(GetKeyPolicy::getKeys())>;

    This& unscoped() {
        scopeType_ = KeySetScope::None;
        scope_ = {};
        customScopedKeys_ = {};
        return static_cast<Derived&>(*this);
    };

    // Set scope in place
    This& scoped(std::optional<std::string> customScope = {}) {
        if (customScope) {
            scopeType_ = KeySetScope::Custom;
            scope_ = std::move(customScope);
            customScopedKeys_
                = util::map([&](const auto& kdef) { return toScopedKey(kdef, *scope_); }, GetKeyPolicy::getKeys());
        }
        else {
            scopeType_ = KeySetScope::Default;
            scope_ = {};
        }
        return static_cast<Derived&>(*this);
    };


    // Create a new KeySet with different scope
    This makeUnscoped() const {
        This ret{static_cast<const Derived&>(*this)};
        ret.unscoped();
        return ret;
    };

    // Set scope in place
    This makeScoped(std::optional<std::string> customScope = {}) const {
        This ret{static_cast<const Derived&>(*this)};
        ret.scoped(std::move(customScope));
        return ret;
    };


    const TupleType& keys() const {
        switch (scopeType_) {
            case KeySetScope::Default:
                return GetKeyPolicy::getScopedKeys();
            case KeySetScope::Custom:
                return customScopedKeys_.value();
            default:
                return GetKeyPolicy::getKeys();
        }
    }

    std::optional<std::string_view> getScope() const {
        if (scope_) {
            return *scope_;
        }
        return GetKeyPolicy::getScope();
    }

private:
    KeySetScope scopeType_{KeySetScope::None};
    std::optional<std::string> scope_{};
    std::optional<TupleType> customScopedKeys_{};
};


template <typename EnumType>
struct KeySetGetKeysPolicy {
    static_assert(util::IsTuple_v<std::decay_t<decltype(StaticKeySetStore<EnumType>::keys())>>, "Expected a tuple");

    static const auto& getKeys() { return StaticKeySetStore<EnumType>::keys(); }
    static const auto& getScopedKeys() { return StaticKeySetStore<EnumType>::scopedKeys(); }
    static std::optional<std::string_view> getScope() { return KeySetName_v<EnumType>; }
};


template <typename EnumType>
class KeySet : public BaseKeySet<KeySet<EnumType>, KeySetGetKeysPolicy<EnumType>> {};


template <auto... Ids>
struct CustomKeySetGetKeysPolicy {
    static const auto& getKeys() {
        static const auto keys = std::make_tuple(key<Ids>()...);
        return keys;
    }

    static const auto& getScopedKeys() {
        static const auto scopedKeys = std::make_tuple(toScopedKey(keyDef<Ids>(), KeySetName_v<decltype(Ids)>)...);
        return scopedKeys;
    }

    static std::optional<std::string_view> getScope() { return {}; }
};


template <auto... Ids>
class CustomKeySet : public BaseKeySet<CustomKeySet<Ids...>, CustomKeySetGetKeysPolicy<Ids...>> {};


template <typename T>
struct IsKeySet {
    static constexpr bool value = false;
};
template <typename EnumType>
struct IsKeySet<KeySet<EnumType>> {
    static constexpr bool value = true;
};
template <auto... Ids>
struct IsKeySet<CustomKeySet<Ids...>> {
    static constexpr bool value = true;
};

template <typename T>
inline constexpr bool IsKeySet_v = IsKeySet<T>::value;


//-----------------------------------------------------------------------------


template <typename EnumType>
constexpr auto keySet() {
    return KeySet<EnumType>{};
}

template <auto... Ids>
constexpr auto keySet() {
    return CustomKeySet<Ids...>{};
}

// This is the accossor that will be used to access individual keys
template <auto keyId, typename KS, std::enable_if_t<IsKeySet_v<std::decay_t<KS>>, bool> = true>
decltype(auto) key(KS&& keySet) {
    return keyUtils::getById<keyId>(std::forward<KS>(keySet).keys());
}

}  // namespace multio::datamod



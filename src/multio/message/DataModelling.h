/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Philipp Geier

/// @date March 2025

#pragma once

#include <tuple>
#include <type_traits>
#include "multio/message/Metadata.h"
#include "multio/util/Hash.h"
#include "multio/util/TypeTraits.h"

// This code aims to provide a mechanims to effectively describe a set of key value pairs  with
//   * a type
//   * name (string representation)
//   * possible mappers (to do conversions or checks)
//   * tags (currently if a key is required or optional)
//   * scope (for a whole key set)
//
// The advantage of this mechanism is that we get a behaviour similar to complie-time reflexion - meaning we get
//   * en/decoding/parsing/reading & writing calls to interact with other containers
//   * hashing functionality
//   * very few repetition of code (no need to repeat tho whole list of keys in various places)
//
// Fundamentally this happens by creating EnumType for KeySets and adressing keys through their enum ID. Sets of keys
// are organized in tuples. Custom key sets can be created by selecting specific keys of different key sets.
//
// The descriptions are created through the type `KeyValueDescription`.
// The whole key set for an enum type is described through template specialization of a struct `KeySetDescription`.
// Here the step of keys is described and wrapped into a tuple type, also a default scope name is given (e.g. "mars")
//
// A special type `KeySet` or `CustomKeySet` wraps a tuple of keys and provides scoping mechanims.
//
// The type `KeyValue` allows making a real value from a `KeyValueDescription`.
// The type `KeyValueSet` combines a tuple of reified values and a key set - making it a real object with values.
//
// A `KeyValue` can contain a `MissingValue`, the specific `ValueType` or a reference to the value type.
// The creating of a `KeyValue` from a `KeyValueDescription` happens throuh a function called `reify` - which defaults
// initiaties all values to missing.
//
// To interact with containers (like Metadata) a `KeyValueReader` and `KeyValueWriter` can be specialized.
// The readers are then accessed through the `read` call (which is still a reification process).
// The readers can check if a key is required or optional and alread throw exceptions.
//
// By default values are referenced and not copied. To create values, either directly use `readValue` or call `acquire`
// on a `KeyValueSet`.
//
// TBD: 
//  * There is a experimental `validate` function which is ment to be extended in the future.
//  * add (default) validation/initalization to KeySets that can post process across keys or throw on inconsistencies
//  * move to data/Modelling.h and mars keys to data/Models.h
//  * implement KeyValueReader/KeyValueWriter for eckit::LocalConfiguration and use keysets to parse and validate action configurations


namespace multio::message {

//-----------------------------------------------------------------------------
// Definitions to describe key-value pairs
//-----------------------------------------------------------------------------


// Forward declaration
enum class KVTag : std::uint64_t
{
    Required,
    Optional,
};


template <auto id, typename ValueType, KVTag tag = KVTag::Required, typename Mapper = void>
struct KeyValueDescription;


template <auto id_, typename ValueType_, KVTag tag_>
struct KeyValueDescription<id_, ValueType_, tag_, void> {
    using KeyType = typename MetadataTypes::KeyType;
    using ValueType = ValueType_;
    using Mapper = void;

    KeyType key;

    static constexpr auto id = id_;
    static constexpr KVTag tag = tag_;
    static constexpr bool hasMapper = false;

    template <KVTag newTag>
    constexpr KeyValueDescription<id_, ValueType_, newTag, void> withTag() const {
        if constexpr (newTag == tag) {
            return *this;
        }
        else {
            return KeyValueDescription<id_, ValueType_, newTag, void>{key};
        }
    }


    operator KeyType&() { return key; }
    operator const KeyType&() const { return key; }
    operator std::string&() { return key; }
    operator const std::string&() const { return key; }
};


template <auto id_, typename ValueType_, KVTag tag_, typename Mapper_>
struct KeyValueDescription {
    using KeyType = typename MetadataTypes::KeyType;
    using ValueType = ValueType_;
    using Mapper = Mapper_;

    KeyType key;
    Mapper mapper;

    static const auto id = id_;
    static constexpr KVTag tag = tag_;
    static constexpr bool hasMapper = true;

    template <KVTag newTag>
    constexpr KeyValueDescription<id_, ValueType_, newTag, Mapper> withTag() const {
        if constexpr (newTag == tag) {
            return *this;
        }
        else {
            return KeyValueDescription<id_, ValueType_, newTag, Mapper>{key, mapper};
        }
    }

    operator KeyType&() { return key; }
    operator const KeyType&() const { return key; }
    operator std::string&() { return key; }
    operator const std::string&() const { return key; }
};


//-----------------------------------------------------------------------------


template <auto id_, typename ValueType, KVTag tag = KVTag::Required, typename KeyType>
KeyValueDescription<id_, ValueType, tag> describeKeyValue(KeyType&& key) {
    return KeyValueDescription<id_, ValueType, tag>{std::forward<KeyType>(key)};
}
template <auto id_, typename ValueType, KVTag tag = KVTag::Required, typename KeyType, typename Mapper>
KeyValueDescription<id_, ValueType, tag, std::decay_t<Mapper>> describeKeyValue(KeyType&& key, Mapper&& mapper) {
    return KeyValueDescription<id_, ValueType, tag, std::decay_t<Mapper>>{std::forward<KeyType>(key),
                                                                          std::forward<Mapper>(mapper)};
}

//-----------------------------------------------------------------------------

template <typename T>
struct IsKeyValueDescription {
    static constexpr bool value = false;
};
template <auto id, typename ValueType, KVTag tag, typename Mapper>
struct IsKeyValueDescription<KeyValueDescription<id, ValueType, tag, Mapper>> {
    static constexpr bool value = true;
};

template <typename T>
inline constexpr bool IsKeyValueDescription_v = IsKeyValueDescription<T>::value;


//-----------------------------------------------------------------------------
// Definitions to handle key sets
//-----------------------------------------------------------------------------

/* To be specialized to retrieve a keyset with all keys for a Enum and further information
 */
template <typename EnumType>
struct KeySetDescription;


template <typename T>
struct IsKeySetDescription {
    static constexpr bool value = false;
};
template <typename EnumType>
struct IsKeySetDescription<KeySetDescription<EnumType>> {
    static constexpr bool value = true;
};

template <typename T>
inline constexpr bool IsKeySetDescription_v = IsKeySetDescription<T>::value;


// Get name of a keyset
template <typename EnumType>
inline constexpr std::string_view KeySetDescriptionName_v = KeySetDescription<EnumType>::name;


//-----------------------------------------------------------------------------


namespace keyUtils {
template <auto val, typename Tup, std::size_t I1, std::size_t... I, std::enable_if_t<(sizeof...(I) == 0), bool> = true>
constexpr std::size_t getKeyIndexById(std::index_sequence<I1, I...>) {
    using T1 = std::tuple_element_t<I1, Tup>;
    static_assert(std::is_same_v<std::decay_t<decltype(T1::id)>, decltype(val)>);
    static_assert(T1::id == val, "Non of the types match the key");
    return 0;
}
template <auto val, typename Tup, std::size_t I1, std::size_t... I, std::enable_if_t<(sizeof...(I) > 0), bool> = true>
constexpr std::size_t getKeyIndexById(std::index_sequence<I1, I...>) {
    using T1 = std::tuple_element_t<I1, Tup>;
    if constexpr (std::is_same_v<std::decay_t<decltype(T1::id)>, decltype(val)>) {
        if constexpr (T1::id == val) {
            return 0;
        }
        else {
            return 1 + getKeyIndexById<val, Tup>(std::index_sequence<I...>{});
        }
    }
    else {
        return 1 + getKeyIndexById<val, Tup>(std::index_sequence<I...>{});
    }
    return 0;  // Unreachable - avoid compiler warning
}


template <auto keyId, typename Tup, std::enable_if_t<util::IsTuple_v<std::decay_t<Tup>>, bool> = true>
decltype(auto) getById(Tup&& tup) {
    return std::get<getKeyIndexById<keyId, std::decay_t<Tup>>(
        std::make_index_sequence<std::tuple_size_v<std::decay_t<Tup>>>{})>(std::forward<Tup>(tup));
}


}  // namespace keyUtils


template <auto keyId, typename Tup, std::enable_if_t<util::IsTuple_v<std::decay_t<Tup>>, bool> = true>
decltype(auto) key(Tup&& keySet) {
    using EnumType = decltype(keyId);
    return keyUtils::getById<keyId>(std::forward<Tup>(keySet));
}

template <auto keyId>
decltype(auto) key() {
    return key<keyId>(KeySetDescription<decltype(keyId)>::keys());
}


//-----------------------------------------------------------------------------

template <typename KVD, std::enable_if_t<IsKeyValueDescription_v<std::decay_t<KVD>>, bool> = true>
std::decay_t<KVD> applyScope(KVD&& kvd, const std::string_view scope) {
    std::decay_t<KVD> newKvd{std::forward<KVD>(kvd)};
    newKvd.key = std::string(scope) + std::string("-") + std::string(kvd.key);
    return newKvd;
}
template <typename Tup,
          std::enable_if_t<(util::IsTuple_v<std::decay_t<Tup>>
                            && IsKeyValueDescription_v<std::decay_t<std::tuple_element_t<0, std::decay_t<Tup>>>>),
                           bool>
          = true>
std::decay_t<Tup> applyScope(Tup&& tup, const std::string_view scope) {
    return util::map([&](auto&& kvd) { return applyScope(std::forward<decltype(kvd)>(kvd), scope); },
                     std::forward<Tup>(tup));
}


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
        scope = KeySetScope::None;
        return static_cast<Derived&>(*this);
    };

    // Set scope in place
    This& scoped(std::optional<std::string> customScope = {}) {
        scope = KeySetScope::Default;
        if (customScope) {
            scope = KeySetScope::Custom;
            customScopedKeys = applyScope(GetKeyPolicy::getKeys(), *customScope);
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
        ret.scoped(customScope);
        return ret;
    };


    const TupleType& keys() const {
        switch (scope) {
            case KeySetScope::Default:
                return GetKeyPolicy::getScopedKeys();
            case KeySetScope::Custom:
                return customScopedKeys.value();
            default:
                return GetKeyPolicy::getKeys();
        }
    }

private:
    KeySetScope scope{KeySetScope::None};
    std::optional<TupleType> customScopedKeys{};
};


template <typename EnumType>
struct KeySetGetKeysPolicy {
    static const auto& getKeys() { return KeySetDescription<EnumType>::keys(); }
    static const auto& getScopedKeys() {
        static const auto scopedKeys = applyScope(getKeys(), KeySetDescriptionName_v<EnumType>);
        return scopedKeys;
    }
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
        static const auto scopedKeys
            = std::make_tuple(applyScope(key<Ids>(), KeySetDescriptionName_v<decltype(Ids)>)...);
        return scopedKeys;
    }
};

// TODO allow customizing tags
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
    static_assert(util::IsTuple_v<std::decay_t<decltype(KeySetDescription<EnumType>::keys())>>, "Expected a tuple");
    return KeySet<EnumType>{};
}

template <auto... Ids>
constexpr auto keySet() {
    return CustomKeySet<Ids...>{};
}

template <auto keyId, typename KS, std::enable_if_t<IsKeySet_v<std::decay_t<KS>>, bool> = true>
decltype(auto) key(KS&& keySet) {
    return keyUtils::getById<keyId>(std::forward<KS>(keySet).keys());
}


//-----------------------------------------------------------------------------
// Value containers
//-----------------------------------------------------------------------------

struct MissingValue {};

// Operators for missing value ... use SFINAE to let this code remain header only
// Also implement operator for other types to simplify comparison implementation for KeyValue
template <typename MV1, typename MV2,
          std::enable_if_t<(std::is_same_v<MV1, MissingValue> && std::is_same_v<MV2, MissingValue>), bool> = true>
bool operator==(const MV1& lhs, const MV2& rhs) noexcept {
    return true;
}
template <typename MV1, typename MV2,
          std::enable_if_t<(std::is_same_v<MV1, MissingValue> && !std::is_same_v<MV2, MissingValue>), bool> = true>
bool operator==(const MV1& lhs, const MV2& rhs) noexcept {
    return false;
}
template <typename MV1, typename MV2,
          std::enable_if_t<(!std::is_same_v<MV1, MissingValue> && std::is_same_v<MV2, MissingValue>), bool> = true>
bool operator==(const MV1& lhs, const MV2& rhs) noexcept {
    return false;
}

template <typename MV1, typename MV2,
          std::enable_if_t<(std::is_same_v<MV1, MissingValue> && std::is_same_v<MV2, MissingValue>), bool> = true>
bool operator!=(const MV1& lhs, const MV2& rhs) noexcept {
    return false;
}
template <typename MV1, typename MV2,
          std::enable_if_t<(std::is_same_v<MV1, MissingValue> && !std::is_same_v<MV2, MissingValue>), bool> = true>
bool operator!=(const MV1& lhs, const MV2& rhs) noexcept {
    return true;
}
template <typename MV1, typename MV2,
          std::enable_if_t<(!std::is_same_v<MV1, MissingValue> && std::is_same_v<MV2, MissingValue>), bool> = true>
bool operator!=(const MV1& lhs, const MV2& rhs) noexcept {
    return true;
}

template <auto id_>
struct KeyValue {
    using Description = std::decay_t<decltype(key<id_>())>;
    using ValueType = typename Description::ValueType;
    using Mapper = typename Description::Mapper;
    using This = KeyValue<id_>;

    static const auto id = id_;

    using RefType = std::reference_wrapper<const ValueType>;
    using Container = std::variant<MissingValue, ValueType, RefType>;

    // Actual value
    Container value;

    bool isMissing() const { return std::holds_alternative<MissingValue>(value); }
    bool holdsReference() const { return std::holds_alternative<const ValueType>(value); }

    // Function to get the contained value if it's not missing - due to the possibility of containing a reference, only
    // const& versions can get Optimized rvalue handling is achieved through visit
    const ValueType& get() const {
        return std::visit(eckit::Overloaded{
                              [&](const ValueType& val) -> const ValueType& { return val; },
                              [&](const RefType& val) -> const ValueType& { return val.get(); },
                              [&](const MissingValue&) -> const ValueType& {
                                  throw MetadataException(
                                      std::string("Value is missing for key ") + std::string(key<id>().key), Here());
                              },
                          },
                          value);
    }
    operator const ValueType&() const { return get(); }

    void setMissing() noexcept { value = MissingValue{}; }
    template <typename V>
    void set(V&& v) noexcept {
        value = std::forward<V>(v);
    }

    // Visit function that handles RValues and references properly
    template <typename Func, typename Val>
    static decltype(auto) visitHelper(Func&& func, Val&& val) {
        return std::visit(eckit::Overloaded{
                              [&](RefType ref) { return std::forward<Func>(func)(ref.get()); },
                              [&](auto&& v) { return std::forward<Func>(func)(std::forward<decltype(v)>(v)); },
                          },
                          std::forward<Val>(val));
    }

    template <typename Func>
    decltype(auto) visit(Func&& func) & {
        return visitHelper(std::forward<Func>(func), value);
    }
    template <typename Func>
    decltype(auto) visit(Func&& func) const& {
        return visitHelper(std::forward<Func>(func), value);
    }
    template <typename Func>
    decltype(auto) visit(Func&& func) && {
        return visitHelper(std::forward<Func>(func), std::move(value));
    }

    // This& operator=(const This&) = default;
    // This& operator=(This&&) noexcept = default;

    // template<typename V>
    // This& operator=(V&& v) noexcept { value = std::forward<V>(v); }


    // Make sure no reference is hold and value is owned
    void acquire() {
        std::visit(eckit::Overloaded{
                       [&](const RefType& val) { this->value = val.get(); },
                       [&](auto) {},
                   },
                   value);
    }
};


template <auto Id>
bool operator==(const KeyValue<Id>& lhs, const KeyValue<Id>& rhs) noexcept {
    return lhs.visit(
        [&](const auto& lhsVal) { return rhs.visit([&](const auto& rhsVal) { return lhsVal == rhsVal; }); });
}
template <auto Id>
bool operator!=(const KeyValue<Id>& lhs, const KeyValue<Id>& rhs) noexcept {
    return lhs.visit(
        [&](const auto& lhsVal) { return rhs.visit([&](const auto& rhsVal) { return lhsVal != rhsVal; }); });
}


//-----------------------------------------------------------------------------


template <typename T>
struct IsKeyValue {
    static constexpr bool value = false;
};
template <auto id>
struct IsKeyValue<KeyValue<id>> {
    static constexpr bool value = true;
};

template <typename T>
inline constexpr bool IsKeyValue_v = IsKeyValue<T>::value;


template <auto id>
decltype(auto) toMissingKeyValue() {
    return KeyValue<id>{MissingValue{}};
}

template <typename KVD, std::enable_if_t<IsKeyValueDescription_v<KVD>, bool> = true>
decltype(auto) toMissingKeyValue(const KVD&) {
    return toMissingKeyValue<KVD::id>();
}


template <auto id, typename V, std::enable_if_t<!IsKeyValue_v<std::decay_t<V>>, bool> = true>
decltype(auto) toKeyValueRef(V&& v) {
    using KV = KeyValue<id>;
    if constexpr (util::IsOptional_v<std::decay_t<V>>) {
        if (v) {
            return toKeyValueRef<id>(std::forward<V>(v).value());
        }
        else {
            return KV{MissingValue{}};
        }
    }
    else {
        if constexpr (!std::is_lvalue_reference_v<V> || std::is_same_v<std::decay_t<V>, MissingValue>) {
            return KV{std::move(v)};
        }
        else {
            return KV{typename KV::RefType(v)};
        }
    }
    return KV{MissingValue{}};  // unreachable - prevent compiler warning
}
template <typename KVD, typename V, std::enable_if_t<IsKeyValueDescription_v<KVD>, bool> = true>
decltype(auto) toKeyValueRef(const KVD&, V&& v) {
    return toKeyValueRef<KVD::id>(std::forward<V>(v));
}

template <auto id, typename V, std::enable_if_t<!IsKeyValue_v<std::decay_t<V>>, bool> = true>
decltype(auto) toKeyValue(V&& v) {
    using KV = KeyValue<id>;
    if constexpr (util::IsOptional_v<std::decay_t<V>>) {
        if (v) {
            return toKeyValue<id>(std::forward<V>(v).value());
        }
        else {
            return KV{descr, MissingValue{}};
        }
    }
    else {
        if constexpr (!std::is_lvalue_reference_v<V> || std::is_same_v<std::decay_t<V>, MissingValue>) {
            return KV{std::move(v)};
        }
        else {
            return KV{v};
        }
    }
    return KV{MissingValue{}};  // unreachable - prevent compiler warning
}


//-----------------------------------------------------------------------------

template <typename KVD, typename V, std::enable_if_t<IsKeyValueDescription_v<KVD>, bool> = true>
decltype(auto) toKeyValue(const KVD&, V&& v) {
    return toKeyValue<KVD::id>(std::forward<V>(v));
}


// Takes a tuple of KeyValueDescription and creates an instance of the keyset with all fields set to missing
template <typename DescTup,
          std::enable_if_t<(util::IsTuple_v<std::decay_t<DescTup>>
                            && IsKeyValueDescription_v<std::tuple_element_t<0, std::decay_t<DescTup>>>),
                           bool>
          = true>
decltype(auto) reify(DescTup&& tup) {
    return util::map([&](const auto& kvd) { return toMissingKeyValue(kvd); }, std::forward<DescTup>(tup));
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
        keySet.scoped(std::move(customeScope));
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
template <typename Tup,
          std::enable_if_t<(util::IsTuple_v<std::decay_t<Tup>> && IsKeyValue_v<std::tuple_element_t<0, Tup>>), bool>
          = true>
Tup& acquire(Tup& tup) {
    util::forEach([](auto& kv) { kv.acquire(); }, tup);
    return tup;
}

// Takes a tuple of KeyValues and converts all references to value by copying
template <typename KVS, std::enable_if_t<(IsKeyValueSet_v<KVS>), bool> = true>
KVS& acquire(KVS& kvs) {
    acquire(kvs.values);
    return kvs;
}


//-----------------------------------------------------------------------------
// Reading from other containers
//-----------------------------------------------------------------------------

// Methods to read/write tuples of KeyValues to specific types (i.e. from Metadata)
// getByValue(description, container)
// getByRef(description, container)
template <typename Container>
struct KeyValueReader;

template <typename Container>
struct BaseKeyValueReader {
    template <
        typename KVD, typename Cont_,
        std::enable_if_t<(IsKeyValueDescription_v<KVD> && std::is_base_of_v<Cont_, std::decay_t<Container>>), bool>
        = true>
    static decltype(auto) getByValue(const KVD& kvd, Cont_&& c) {
        auto ret = KeyValueReader<Container>::getByRef(kvd, std::forward<Cont_>(c));
        acquire(ret);
        return ret;
    }
};


//-----------------------------------------------------------------------------
// Reading from keysets
//-----------------------------------------------------------------------------

template <auto id, typename... KVS>
struct KeyValueReader<std::tuple<KeyValue<id>, KVS...>> : BaseKeyValueReader<std::tuple<KeyValue<id>, KVS...>> {
    using Base = BaseKeyValueReader<std::tuple<KeyValue<id>, KVS...>>;
    using Base::getByValue;

    template <typename KVD, typename KVTup,
              std::enable_if_t<(IsKeyValueDescription_v<std::decay_t<KVD>> && util::IsTuple_v<std::decay_t<KVTup>>
                                && IsKeyValue_v<std::tuple_element_t<0, std::decay_t<KVTup>>>),
                               bool>
              = true>
    static decltype(auto) getByRef(const KVD& kvd, KVTup&& kv) {
        return key<KVD::id>(std::forward<KVTup>(kv));
    }
};

template <typename KeySet_>
struct KeyValueReader<KeyValueSet<KeySet_>> : BaseKeyValueReader<KeyValueSet<KeySet_>> {
    using Base = BaseKeyValueReader<KeyValueSet<KeySet_>>;
    using BaseTup = KeyValueReader<typename KeyValueSet<KeySet_>::TupleType>;
    using Base::getByValue;

    template <typename KVD, typename KVS,
              std::enable_if_t<(IsKeyValueDescription_v<std::decay_t<KVD>> && IsKeyValueSet_v<std::decay_t<KVS>>), bool>
              = true>
    static decltype(auto) getByRef(const KVD& kvd, KVS&& kv) {
        return BaseTup::getByRef(kvd, std::forward<KVS>(kv).values);
    }
};


//-----------------------------------------------------------------------------
// Reading from metadata
//-----------------------------------------------------------------------------

template <>
struct KeyValueReader<BaseMetadata> : BaseKeyValueReader<BaseMetadata> {
    using Base = BaseKeyValueReader<BaseMetadata>;
    using Base::getByValue;

    template <
        typename KVD, typename MD,
        std::enable_if_t<(IsKeyValueDescription_v<KVD> && std::is_base_of_v<BaseMetadata, std::decay_t<MD>>), bool>
        = true>
    static decltype(auto) getByRef(const KVD& kvd, MD&& md) {
        if constexpr (KVD::tag == KVTag::Optional) {
            if constexpr (KVD::hasMapper) {
                if (auto search = std::forward<MD>(md).find(kvd.key); search != md.end()) {
                    return toKeyValueRef(kvd, search->second.visit([&](auto&& v) {
                        return kvd.mapper.read(std::forward<decltype(v)>(v));
                    }));
                }
                return toMissingKeyValue(kvd);
            }
            else {
                if (auto search = std::forward<MD>(md).find(kvd.key); search != md.end()) {
                    // If container is not an lvalue we can move from
                    if constexpr (!std::is_lvalue_reference_v<MD>) {
                        return toKeyValueRef(kvd, std::move(search->second.template get<typename KVD::ValueType>()));
                    }
                    else {
                        return toKeyValueRef(kvd, search->second.template get<typename KVD::ValueType>());
                    }
                }
                return toKeyValueRef(kvd, std::optional<typename KVD::ValueType>{});
            }
        }
        else {
            if constexpr (KVD::hasMapper) {
                return toKeyValueRef(kvd, std::forward<MD>(md).get(kvd.key).visit(
                                              [&](auto&& v) { return kvd.mapper.read(std::forward<decltype(v)>(v)); }));
            }
            else {
                return toKeyValueRef(kvd, std::forward<MD>(md).template get<typename KVD::ValueType>(kvd.key));
            }
        }
        return toMissingKeyValue(kvd);  // unreachable - prevent compiler warning
    }
};

template <>
struct KeyValueReader<Metadata> : KeyValueReader<BaseMetadata> {
    using Base = KeyValueReader<BaseMetadata>;
    using Base::getByRef;
    using Base::getByValue;
};


//-----------------------------------------------------------------------------
// Writing to other containers
//-----------------------------------------------------------------------------


// Methods to read/write tuples of KeyValues to specific types (i.e. from Metadata)
// set(KeyValueDescription, KeyValue, Container)
template <typename Container>
struct KeyValueWriter;


//-----------------------------------------------------------------------------
// Writing from keysets
//-----------------------------------------------------------------------------

template <auto id, typename... KVS>
struct KeyValueWriter<std::tuple<KeyValue<id>, KVS...>> {
    template <typename KVD, typename KV, typename KVTup,
              std::enable_if_t<(IsKeyValueDescription_v<std::decay_t<KVD>> && IsKeyValue_v<std::decay_t<KV>>
                                && util::IsTuple_v<std::decay_t<KVTup>>
                                && IsKeyValue_v<std::tuple_element_t<0, std::decay_t<KVTup>>>),
                               bool>
              = true>
    static decltype(auto) set(const KVD& kvd, KV&& kv, KVTup& kvTup) {
        return key<KVD::id>(kvTup).set(std::forward<KV>(kv));
    }
};

template <typename KeySet_>
struct KeyValueWriter<KeyValueSet<KeySet_>> {
    using BaseTup = KeyValueWriter<typename KeyValueSet<KeySet_>::TupleType>;

    template <typename KVD, typename KV, typename KVS,
              std::enable_if_t<(IsKeyValueDescription_v<std::decay_t<KVD>> && IsKeyValue_v<std::decay_t<KV>>
                                && IsKeyValueSet_v<std::decay_t<KVS>>),
                               bool>
              = true>
    static decltype(auto) set(const KVD& kvd, KV&& kv, KVS& kvs) {
        return BaseTup::set(kvd, std::forward<KV>(kv), kvs.values);
    }
};


//-----------------------------------------------------------------------------
// Writing to metadata
//-----------------------------------------------------------------------------

template <>
struct KeyValueWriter<BaseMetadata> {
    template <typename KVD, typename KV_, typename MD,
              std::enable_if_t<(IsKeyValueDescription_v<std::decay_t<KVD>> && IsKeyValue_v<std::decay_t<KV_>>
                                && std::is_base_of_v<BaseMetadata, std::decay_t<MD>>),
                               bool>
              = true>
    static void set(const KVD& kvd, KV_&& kv, MD& md) {
        using KV = std::decay_t<KV_>;
        // TODO think about handling missing value by setting Null ?
        if constexpr (KVD::hasMapper) {
            std::forward<KV_>(kv).visit(
                eckit::Overloaded{[&](MissingValue v) {},
                                  [&](auto&& v) { md.set(kvd.key, kvd.mapper.write(std::forward<decltype(v)>(v))); }});
        }
        else {
            std::forward<KV_>(kv).visit(eckit::Overloaded{
                [&](MissingValue v) {}, [&](auto&& v) { md.set(kvd.key, std::forward<decltype(v)>(v)); }});
        }
    }
};

template <>
struct KeyValueWriter<Metadata> : KeyValueWriter<BaseMetadata> {
    using Base = KeyValueWriter<BaseMetadata>;
    using Base::set;
};


template <typename KS, std::enable_if_t<(IsKeySet_v<std::decay_t<KS>>), bool> = true>
decltype(auto) reify(KS&& ks) {
    auto values = reify(ks.keys());
    return KeyValueSet<std::decay_t<KS>>{std::forward<KS>(ks), std::move(values)};
}


// Takes a tuple of KeyValueDescription and a container from which to read/create an object from
template <typename DescTup, typename Container,
          std::enable_if_t<(util::IsTuple_v<std::decay_t<DescTup>>
                            && IsKeyValueDescription_v<std::tuple_element_t<0, std::decay_t<DescTup>>>),
                           bool>
          = true>
decltype(auto) read(DescTup&& tup, Container&& c) {
    return util::map(
        [&](const auto& kvd) {
            return KeyValueReader<std::decay_t<Container>>::getByRef(kvd, std::forward<Container>(c));
        },
        std::forward<DescTup>(tup));
}
template <typename DescTup, typename Container,
          std::enable_if_t<(util::IsTuple_v<std::decay_t<DescTup>>
                            && IsKeyValueDescription_v<std::tuple_element_t<0, std::decay_t<DescTup>>>),
                           bool>
          = true>
decltype(auto) readValue(DescTup&& tup, Container&& c) {
    return util::map(
        [&](const auto& kvd) {
            return KeyValueReader<std::decay_t<Container>>::getByValue(kvd, std::forward<Container>(c));
        },
        std::forward<DescTup>(tup));
}


template <typename KS, typename Container, std::enable_if_t<(IsKeySet_v<std::decay_t<KS>>), bool> = true>
decltype(auto) read(KS&& ks, Container&& c) {
    auto values = read(ks.keys(), std::forward<Container>(c));
    return KeyValueSet<std::decay_t<KS>>{std::forward<KS>(ks), std::move(values)};
}

template <typename KS, typename Container, std::enable_if_t<(IsKeySet_v<std::decay_t<KS>>), bool> = true>
decltype(auto) readValue(KS&& ks, Container&& c) {
    auto values = readValue(ks.keys(), std::forward<Container>(c));
    return KeyValueSet<std::decay_t<KS>>{std::forward<KS>(ks), std::move(values)};
}


// TODO take a proper KeyValue set to get the description from
// Takes a tuple of KeyValueDescription and a container from which to read/create an object from
template <
    typename KVTup, typename Container,
    std::enable_if_t<
        (util::IsTuple_v<std::decay_t<KVTup>> && IsKeyValue_v<std::tuple_element_t<0, std::decay_t<KVTup>>>), bool>
    = true>
void write(KVTup&& tup, Container& c) {
    util::forEach(
        [&](auto&& kv) {
            return KeyValueWriter<std::decay_t<Container>>::set(key<std::decay_t<decltype(kv)>::id>(),
                                                                std::forward<decltype(kv)>(kv), c);
        },
        std::forward<KVTup>(tup));
}

template <
    typename Container, typename KVTup,
    std::enable_if_t<
        (util::IsTuple_v<std::decay_t<KVTup>> && IsKeyValue_v<std::tuple_element_t<0, std::decay_t<KVTup>>>), bool>
    = true>
Container write(KVTup&& tup) {
    Container c{};
    write(std::forward<KVTup>(tup), c);
    return c;
}

}  // namespace multio::message

//-----------------------------------------------------------------------------

namespace multio::util {


// General forEach function for KeySet
template <typename Func, typename KS, std::enable_if_t<(multio::message::IsKeySet_v<std::decay_t<KS>>), bool> = true>
void forEach(Func&& func, KS&& ks) {
    util::forEach(std::forward<Func>(Func), ks.keys());
}

// General map function for KeySet
template <typename Func, typename KS, std::enable_if_t<(multio::message::IsKeySet_v<std::decay_t<KS>>), bool> = true>
decltype(auto) map(Func&& func, KS&& ks) {
    return util::map(std::forward<Func>(Func), ks.keys());
}

// General forEach function to iterate on KeyValueSets
template <typename Func, typename KVS,
          std::enable_if_t<(multio::message::IsKeyValueSet_v<std::decay_t<KVS>>), bool> = true>
void forEach(Func&& func, KVS&& kvs) {
    const auto& keys = kvs.keySet.keys();
    util::forEach(
        [&](auto&& kv) {
            func(multio::message::key<std::decay_t<decltype(kv)>::id>(keys), std::forward<decltype(kv)>(kv));
        },
        std::forward<KVS>(kvs).values);
}

// General map function to iterate on KeyValueSets
template <typename Func, typename KVS,
          std::enable_if_t<(multio::message::IsKeyValueSet_v<std::decay_t<KVS>>), bool> = true>
decltype(auto) map(Func&& func, KVS&& kvs) {
    const auto& keys = kvs.keySet.keys();
    return util::map(
        [&](auto&& kv) {
            return func(multio::message::key<std::decay_t<decltype(kv)>::id>(keys), std::forward<decltype(kv)>(kv));
        },
        std::forward<KVS>(kvs).values);
}

}  // namespace multio::util

//-----------------------------------------------------------------------------


namespace multio::message {


template <typename KVS, typename Container, std::enable_if_t<(IsKeyValueSet_v<std::decay_t<KVS>>), bool> = true>
void write(KVS&& kvs, Container& c) {
    util::forEach(
        [&](const auto& kvd, auto&& kv) {
            return KeyValueWriter<std::decay_t<Container>>::set(kvd, std::forward<decltype(kv)>(kv), c);
        },
        std::forward<KVS>(kvs));
}

template <typename Container, typename KVS, std::enable_if_t<(IsKeyValueSet_v<std::decay_t<KVS>>), bool> = true>
Container write(KVS&& kvs) {
    Container c{};
    write(std::forward<KVS>(kvs), c);
    return c;
}


// TODO refactor with validation function on a whole keyset

// Takes a tuple of KeyValue and verifies that all required keys are set
template <typename KVD, typename KV,
          std::enable_if_t<(IsKeyValueDescription_v<std::decay_t<KVD>> && IsKeyValue_v<std::decay_t<KV>>), bool> = true>
decltype(auto) validate(const KVD&, const KV& kv) {
    if constexpr (KVD::tag == KVTag::Required) {
        if (kv.isMissing()) {
            // TODO create an own exception type
            throw MetadataException(std::string("Missing required key: ") + std::string(key<KV::id>().key), Here());
        }
    }
}

// Takes a tuple of KeyValue and verifies that all required keys are set
template <typename KV, std::enable_if_t<(IsKeyValue_v<std::decay_t<KV>>), bool> = true>
decltype(auto) validate(const KV& kv) {
    return validate(key<KV::id>(), kv);
}

// Takes a tuple of KeyValue and verifies that all required keys are set
template <typename KVS, std::enable_if_t<(IsKeyValueSet_v<std::decay_t<KVS>>), bool> = true>
decltype(auto) validate(const KVS& kvs) {
    const auto& keys = kvs.keySet.keys();
    util::forEach([&](const auto& kv) { validate(key<std::decay_t<decltype(kv)>::id>(keys), kv); }, kvs.values);
}

// Takes a tuple of KeyValue and verifies that all required keys are set
template <
    typename ValTup,
    std::enable_if_t<(util::IsTuple_v<std::decay_t<ValTup>> && IsKeyValue_v<std::tuple_element_t<0, ValTup>>), bool>
    = true>
decltype(auto) validate(const ValTup& tup) {
    util::forEach([&](const auto& kv) { validate(kv); }, tup);
}


//-----------------------------------------------------------------------------

}  // namespace multio::message


//-----------------------------------------------------------------------------
// Hashing of KeyValueSets
//-----------------------------------------------------------------------------

template <>
struct std::hash<multio::message::MissingValue> {
    std::size_t operator()(const multio::message::MissingValue&) const noexcept { return 0; }
};

template <auto id>
struct std::hash<multio::message::KeyValue<id>> {
    std::size_t operator()(const multio::message::KeyValue<id>& kv) const
        noexcept(noexcept(multio::util::hash(std::declval<typename multio::message::KeyValue<id>::ValueType>()))) {
        return kv.visit([&](const auto& v) -> std::size_t { return multio::util::hash(v); });
    }
};

template <auto id, typename... KVS>
struct std::hash<std::tuple<multio::message::KeyValue<id>, KVS...>> {
    std::size_t operator()(const std::tuple<multio::message::KeyValue<id>, KVS...>& t) const
        noexcept(noexcept(multio::util::hashCombine(std::declval<multio::message::KeyValue<id>>(),
                                                    std::declval<KVS>()...))) {
        return std::apply([](const auto&... args) { return multio::util::hashCombine(args...); }, t);
    }
};

template <typename KeySet>
struct std::hash<multio::message::KeyValueSet<KeySet>> {
    std::size_t operator()(const multio::message::KeyValueSet<KeySet>& kvs) const
        noexcept(noexcept(multio::util::hash(std::declval<multio::message::KeyValueSet<KeySet>>().values))) {
        return multio::util::hash(kvs.values);
    }
};

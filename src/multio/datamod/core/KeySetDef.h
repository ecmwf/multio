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

#include "multio/datamod/core/KeyDef.h"
#include "multio/datamod/core/KeyUtils.h"


#include "multio/util/TypeTraits.h"


namespace multio::datamod {

//-----------------------------------------------------------------------------
// Definitions to handle key sets as a tuple of key definitions
//-----------------------------------------------------------------------------

// To be specialized to retrieve a keyset with all keys for a Enum and further information
template <typename EnumType>
struct KeySetDefinition;


template <typename T>
struct IsKeySetDefinition {
    static constexpr bool value = false;
};
template <typename EnumType>
struct IsKeySetDefinition<KeySetDefinition<EnumType>> {
    static constexpr bool value = true;
};

template <typename T>
inline constexpr bool IsKeySetDefinition_v = IsKeySetDefinition<T>::value;


// Get name of a keyset
template <typename EnumType>
inline constexpr std::string_view KeySetName_v = KeySetDefinition<EnumType>::name;


// TODO Remove this macro
#define MULTIO_KEY_SET_DESCRIPTION(EnumName, keySetName, ...)         \
    template <>                                                       \
    struct KeySetDefinition<EnumName> {                               \
        static constexpr std::string_view name = keySetName;          \
                                                                      \
        static constexpr auto keyDefs = std::make_tuple(__VA_ARGS__); \
    };


// To be specialized by Enums to provide custom alter function with KeyValueSet<KeySet<EnumType>>
//
// Example
// ```
// template <>
// struct KeySetAlter<KeySet<MyEnum>> {
//    static void alter(KeyValueSet<KeySet<MyEnum>>& kvs) {
//       ... custom operators like setting dependent default values
//           or performing complex consistency checks
//    }
// };
// ```
template <typename KeySet_>
struct KeySetAlter;


template <typename KeySet_, typename KeyValueSet_, class = void>
struct HasAlterKeySetFunction : std::false_type {};

template <typename KeySet_, typename KeyValueSet_>
struct HasAlterKeySetFunction<KeySet_, KeyValueSet_,
                              std::void_t<decltype(KeySetAlter<KeySet_>::alter(std::declval<KeyValueSet_&>()))>>
    : std::true_type {};


template <typename KeySet_, typename KeyValueSet_>
inline constexpr bool HasAlterKeySetFunction_v = HasAlterKeySetFunction<KeySet_, KeyValueSet_>::value;


template <typename KeySet_>
struct AlterKeySetFunctor {
    template <typename KVS_, std::enable_if_t<(HasAlterKeySetFunction_v<KeySet_, KVS_>), bool> = true>
    void operator()(KVS_& kvs) const {
        KeySetAlter<KeySet_>::alter(kvs);
    }

    template <typename KVS_, std::enable_if_t<(!HasAlterKeySetFunction_v<KeySet_, KVS_>), bool> = true>
    void operator()(KVS_&) const {}
};


//-----------------------------------------------------------------------------
// Direct accessors to key definitions - expected to be used internally only
//-----------------------------------------------------------------------------

template <auto keyId, typename Tup, std::enable_if_t<util::IsTuple_v<std::decay_t<Tup>>, bool> = true>
decltype(auto) keyDef(Tup&& keySet) {
    return keyUtils::getById<keyId>(std::forward<Tup>(keySet));
}

template <auto keyId>
decltype(auto) keyDef() {
    return keyDef<keyId>(KeySetDefinition<decltype(keyId)>::keyDefs);
}


template <auto id_>
using KeyDef_t = std::decay_t<decltype(keyDef<id_>())>;

template <auto id_>
using KeyDefValueType_t = typename KeyDef_t<id_>::ValueType;

template <auto id_>
using KeyDefMapper_t = typename KeyDef_t<id_>::Mapper;

template <auto id_>
inline constexpr KVTag KeyDefTag_v = KeyDef_t<id_>::tag;

template <auto id_>
inline constexpr bool KeyDefHasDefaultValueFunctor_v = KeyDef_t<id_>::hasDefaultValueFunctor;


}  // namespace multio::datamod


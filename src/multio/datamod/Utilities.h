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
#include <variant>
#include "multio/datamod/ReaderWriter.h"

#include "multio/datamod/core/KeyDef.h"
#include "multio/datamod/core/KeySetDef.h"
#include "multio/datamod/core/Key.h"
#include "multio/datamod/core/KeySet.h"
#include "multio/datamod/core/KeyValue.h"
#include "multio/datamod/core/KeyValueSet.h"
#include "multio/datamod/core/KeyValueReader.h"
#include "multio/datamod/core/KeyValueWriter.h"

#include "multio/util/Hash.h"
#include "multio/util/TypeToString.h"
#include "multio/util/TypeTraits.h"

// TLDR:
// The following combination of mechanims is used
//   * Use enum tagged tuples instead of struct/classes to describe product types for incoming metadata model or
//     configuration.
//     Benefit: Allow compile time iteration over members to generate parsers/validators, emitters, printers etc...
//     Ugly: Some compilation error messages can be long. Otherwise accessing etc. is just different but not necessarily
//     worse over a struct. Principle: `template<auto id_>` is used to pass any entry of any enum, making each key
//     representing an own type. With `decltype(id)` the enum itself can be determined.
//                In the embracing struct a `constexpr` field `id` is declared. Accessing is done at compile time by
//                iterating each `T` in the tuple and checking `std::is_same_v<decltype(id), T::id>` and `id == T::id`.
//   * Keys are described at compile time (using KeyDef) - an enum `id` is accosiated with `string_view` key name,
//   `string_view` description (do generate doc or meaningful error messages),
//     a `ValueType`, custom mapper (to read/write from containers) and an enum to tag if they are required, optional
//     etc....
//   * Custom mappers allow customizing reading/writer (decoding/encoding...) of existing types on specific fields
//   * `Reader`/`Writer` are used to build accosiated `ValueTypes` and either call a specialized
//   `ReaderSpec<>::read`/`WriterSpec<>::write`, custom mapper `::read/::write` or
//     try to use convertion/construction via usual C++ mechanims.
//   * All Keys are defined within a `KeySet`. `KeySets` can be instantiated to `KeyValueSet` - the eventual usable
//   object
//   * A KeySet can have a custom `alter` function to set further dependent defaults or do consistency checks.
//   Eventually `KeyValueSets`
//     are validated to make sure all non-optional keys are present.
//   * Customizing reading & writing of KeyValueSets from specific containers is done with specializing `KeyValueReader`
//   and `KeyValueWriter`
//
// # Why: C++ is lacking compile time reflective features.
//
// MultIO is a lot of glue code that needs to handle various set of keys (data models) for
//   * validating and specifying expected metadata
//   * configuration for a lot of actions
//   * interfacing metadata & configuration to different libraries
//
// *Metadata*: Subset of mars keys, grib specific keys & custom data models to sort things ous - unfortunately even
// after years we can not define a limited set of classes that are reused among packages...
//
// *Configuration*: In the end
// a big YAML file is describing a plan. It should be possible to describe and verify the plan "easily". Often
// configuration contains metadata keys or content for third-party configuration/metadata
//
// Advantage:
//  * The mechanism implies some constraints and requirements on what we need to describe. Then we generate a lot of
//  code.
//  * Exchanging interfacing types (i.e. removing `eckit::LocalConfiguration` of other YAML parser) is simple
//  * Possibility to generate more detailed documentation and python code (i.e. via json-schema)
//  * In future we can to more analysis on metadata requirements on a pipeline of actions
// Disadvantage:
//  * Some error messages can be ugly.
//  * Need to use some of the features here
//
// Personal comment (Philipp Geier): After two years of resisting going that way, I'm bored of writing the same glue
// code again and again.
//   With generated code, validation and error messages are much more consistent.
//   On the long run it should allow focussing more on essential things in the actions (execution & handling state...)
//
//
// # Essentials & How to migrate away from this:
//
// The keysets described through this mechanism already contain the most important things:
//   * keys (stringified)
//   * types
//   * defaults
//   * mappers
//   * initialization
//
// Type mappers & initialization can be natually described through `struct` and `classes` - but may require creating
// more types for specific keys. Defaults can also be implied explicitly in the code (it's just not organized then). For
// interfacing with other containers (metadata, configuration, third-party...) specialized code has to be written,
// mostly because all members need to be iterated or accessed. This is the most errorprone & heavyweighted part when a
// lot of keys are involved, because it involves a lot of repetition. Suggestion: At least provide a `forEach` function
// that iterates all members with a descriptive type.
//
//
// # Content
//
// This code aims to provide a mechanims to effectively describe a set of key value pairs  with
//   * a type
//   * name (string representation)
//   * possible mappers (to do conversions or checks)
//   * tags (if a key is required, defaulted or optional)
//   * scope (for a whole key set)
//
// The advantage of this mechanism is that we get a behaviour similar to complie-time reflexion - meaning we get
//   * en/decoding/parsing/reading & writing calls to interact with other containers
//   * hashing functionality
//   * very few repetition of code (no need to repeat tho whole list of keys in various places)
//   * doc strings as constexpr string_view (which mean they are not compiled into binary unless used - we can split
//   json-schema generating binaries...)
//
// Fundamentally this happens by creating EnumType for KeySets and adressing keys through their enum ID. Sets of keys
// are organized in tuples. Custom key sets can be created by selecting specific keys of different key sets.
//
// The descriptions are created through the type `KeyDef` as constexpr, a
// more lightweight type `ScopedKey` which is tagged by an enum and can have modifications on string representation, and
// a `DynamicKey` which is passed to `KeyValueWriter` and `KeyValueReader` for interopration with conainers. For most
// containers no detail on the enum tag is required, hence the DynamicKey is used to for type erasure to avoid
// generating too much unnecessary code.
// The whole key set for an enum type is described through template specialization
// of a struct `KeySetDefinition`. Here the step of keys is described and wrapped into a tuple type, also a default
// scope name is given (e.g. "mars")
//
// A special type `KeySet` or `CustomKeySet` wraps a tuple of keys and provides scoping mechanims.
//
// The types `BaseKeyValue` (untagged) and `KeyValue` (tagged with enum) allows making a real value from a `KeyDef`.
// The type `KeyValueSet` combines a tuple of reified values and a key set - making it a real object with values.
//
// A `KeyValue` can contain a `MissingValue`, the specific `ValueType` or a reference to the value type.
// The creating of a `KeyValue` from a `KeyDef` happens throuh a function called `reify` - which defaults
// initiaties all values to missing.
//
// To interact with containers (like Metadata) a `KeyValueReader` and `KeyValueWriter` can be specialized.
// The readers are then accessed through the `read` call. The readers can check if a key is required and alread throw
// exceptions. Otherwise after the read `alterAndValidate` is called which first tryes to apply defaults to specific
// keys, then calls a user provided `alter` function that may do more complex operations, and eventually validates that
// all optional or defaulted keys are given.
//
// By default values are referenced and not copied. To create values, either directly use `readByValue` or call
// `acquire` on a `KeyValueSet`.
//
// TBD:
//  * Clean up some functions; reorganize reify, read, write function....
//  * Reify can be kept internal - all KeyValueSets should be handled as structs (to be replaced be them in future)
//  * All operations on KeyValueSets can and should have explicit compilation units (shrink down code generation)
//
// TODO
// * Further strip key information (tag does not need to be templated)
// * All printing and stringifying should be put in a compilation unit
// * Reading with id only useful for mapping groups of keys - it can have a special operation
// * Mappers may not be needed or properly typed??
// * It may be worth forcing types to express values they can be read and written from by a variant or typelist? as 
//   suitable intermediate type representation to read from containers? (don't know how references are handled then)
//
// * properly implement something like `is visitable`
//




namespace multio::datamod {

template <typename KS, std::enable_if_t<(IsKeySet_v<std::decay_t<KS>>), bool> = true>
decltype(auto) reify(KS&& ks) {
    auto values = reify(ks.keys());
    return KeyValueSet<std::decay_t<KS>>{std::forward<KS>(ks), std::move(values)};
}


// Takes a KeyDefinition and a container from which to read/create an object from
template <
    typename KVD, typename Container,
    std::enable_if_t<(IsScopedKey_v<std::decay_t<KVD>> && KeyValueReader<std::decay_t<Container>>::isSpecialized), bool>
    = true>
decltype(auto) read(KVD&& kvd, Container&& c) {
    return KeyValueReader<std::decay_t<Container>>::getByRef(KeyId<std::decay_t<KVD>::id>{}, kvd.baseRef(),
                                                             std::forward<Container>(c));
}
template <
    typename KVD, typename Container,
    std::enable_if_t<(IsScopedKey_v<std::decay_t<KVD>> && KeyValueReader<std::decay_t<Container>>::isSpecialized), bool>
    = true>
decltype(auto) readByValue(KVD&& kvd, Container&& c) {
    return KeyValueReader<std::decay_t<Container>>::getByValue(KeyId<std::decay_t<KVD>::id>{}, kvd.baseRef(),
                                                               std::forward<Container>(c));
}


// Takes a tuple of KeyDefinition and a container from which to read/create an object from
template <typename DescTup, typename Container,
          std::enable_if_t<(util::IsTuple_v<std::decay_t<DescTup>>
                            && util::TypeListAll_v<IsScopedKey, util::ToTypeList_t<std::decay_t<DescTup>>>
                            && KeyValueReader<std::decay_t<Container>>::isSpecialized),
                           bool>
          = true>
decltype(auto) read(DescTup&& tup, Container&& c) {
    return util::map([&](const auto& kvd) { return read(kvd, std::forward<Container>(c)); },
                     std::forward<DescTup>(tup));
}
template <typename DescTup, typename Container,
          std::enable_if_t<(util::IsTuple_v<std::decay_t<DescTup>>
                            && util::TypeListAll_v<IsScopedKey, util::ToTypeList_t<std::decay_t<DescTup>>>
                            && KeyValueReader<std::decay_t<Container>>::isSpecialized),
                           bool>
          = true>
decltype(auto) readByValue(DescTup&& tup, Container&& c) {
    return util::map([&](const auto& kvd) { return readByValue(kvd, std::forward<Container>(c)); },
                     std::forward<DescTup>(tup));
}


template <
    typename KS, typename Container,
    std::enable_if_t<(IsKeySet_v<std::decay_t<KS>> && KeyValueReader<std::decay_t<Container>>::isSpecialized), bool>
    = true>
KeyValueSet<std::decay_t<KS>> read(KS&& ks, Container&& c) {
    auto values = read(ks.keys(), std::forward<Container>(c));
    KeyValueSet<std::decay_t<KS>> ret{std::forward<KS>(ks), std::move(values)};
    alterAndValidate(ret);
    return ret;
}

template <
    typename KS, typename Container,
    std::enable_if_t<(IsKeySet_v<std::decay_t<KS>> && KeyValueReader<std::decay_t<Container>>::isSpecialized), bool>
    = true>
KeyValueSet<std::decay_t<KS>> readByValue(KS&& ks, Container&& c) {
    auto values = readByValue(ks.keys(), std::forward<Container>(c));
    KeyValueSet<std::decay_t<KS>> ret{std::forward<KS>(ks), std::move(values)};
    alterAndValidate(ret);
    return ret;
}


template <typename VKS, typename Container,
          std::enable_if_t<(util::IsVariant_v<std::decay_t<VKS>>
                            && util::TypeListAll_v<IsKeySet, util::ToTypeList_t<std::decay_t<VKS>>>
                            && KeyValueReader<std::decay_t<Container>>::isSpecialized),
                           bool>
          = true>
decltype(auto) read(VKS&& vks, Container&& c) {
    using RetValue
        = util::ApplyTypeList_t<std::variant, util::MapTypeList_t<KeyValueSet, util::ToTypeList_t<std::decay_t<VKS>>>>;
    return std::visit(
        [&](auto&& ks) -> RetValue { return read(std::forward<decltype(ks)>(ks), std::forward<Container>(c)); },
        std::forward<VKS>(vks));
}

template <typename VKS, typename Container,
          std::enable_if_t<(util::IsVariant_v<std::decay_t<VKS>>
                            && util::TypeListAll_v<IsKeySet, util::ToTypeList_t<std::decay_t<VKS>>>
                            && KeyValueReader<std::decay_t<Container>>::isSpecialized),
                           bool>
          = true>
decltype(auto) readByValue(VKS&& vks, Container&& c) {
    using RetValue
        = util::ApplyTypeList_t<std::variant, util::MapTypeList_t<KeyValueSet, util::ToTypeList_t<std::decay_t<VKS>>>>;
    return std::visit(
        [&](auto&& ks) -> RetValue { return readByValue(std::forward<decltype(ks)>(ks), std::forward<Container>(c)); },
        std::forward<VKS>(vks));
}


//-----------------------------------------------------------------------------
// ReadSpec for KeyValueSet - make it possible to nest things
//

template <typename KeySet_>
struct ReadSpec<KeyValueSet<KeySet_>> {
    template <typename Val, std::enable_if_t<KeyValueReader<std::decay_t<Val>>::isSpecialized, bool> = true>
    static KeyValueSet<KeySet_> read(Val&& val) noexcept(noexcept(datamod::read(KeySet_{}, std::forward<Val>(val)))) {
        // TODO find a mechanism to determine whether by value or ref is the better default
        // Currently, for nested structures this behaviour can not be propagated unless the `ReadSpec<>` is extended to a readRef call.
        // The outer call may be `read` or `readByValue`, at this point we don't know, hence it's safer to `readByValue`
        // Also key modifications may be considered?
        return datamod::readByValue(KeySet_{}, std::forward<Val>(val));
    }
};


//-----------------------------------------------------------------------------


// Takes a tuple of KeyDefinition and a container from which to read/create an object from
template <typename KVD, typename KV, typename Container,
          std::enable_if_t<(IsScopedKey_v<std::decay_t<KVD>> && IsKeyValue_v<std::decay_t<KV>>
                            && KeyValueWriter<std::decay_t<Container>>::isSpecialized),
                           bool>
          = true>
void write(const KVD& kvd, KV&& kv, Container& c) {
    KeyValueWriter<std::decay_t<Container>>::set(KeyId<std::decay_t<KV>::id>{}, kvd.baseRef(),
                                                 std::forward<decltype(kv)>(kv).baseRef(), c);
}

// Takes a tuple of KeyDefinition and a container from which to read/create an object from
template <
    typename KV, typename Container,
    std::enable_if_t<(IsKeyValue_v<std::decay_t<KV>> && KeyValueWriter<std::decay_t<Container>>::isSpecialized), bool>
    = true>
void write(KV&& kv, Container& c) {
    write(key<std::decay_t<KV>::id>(), std::forward<decltype(kv)>(kv), c);
}

template <typename KVTup, typename Container,
          std::enable_if_t<(util::IsTuple_v<std::decay_t<KVTup>>
                            && util::TypeListAll_v<IsKeyValue, util::ToTypeList_t<std::decay_t<KVTup>>>
                            && KeyValueWriter<std::decay_t<Container>>::isSpecialized),
                           bool>
          = true>
void write(KVTup&& tup, Container& c) {
    util::forEach([&](auto&& kv) { write(std::forward<decltype(kv)>(kv), c); }, std::forward<KVTup>(tup));
}

template <typename Container, typename KVTup,
          std::enable_if_t<(util::IsTuple_v<std::decay_t<KVTup>>
                            && util::TypeListAll_v<IsKeyValue, util::ToTypeList_t<std::decay_t<KVTup>>>
                            && KeyValueWriter<std::decay_t<Container>>::isSpecialized),
                           bool>
          = true>
Container write(KVTup&& tup) {
    Container c{};
    write(std::forward<KVTup>(tup), c);
    return c;
}


template <typename KVS, typename Container,
          std::enable_if_t<
              (IsKeyValueSet_v<std::decay_t<KVS>> && KeyValueWriter<std::decay_t<Container>>::isSpecialized), bool>
          = true>
void write(KVS&& kvs, Container& c) {
    util::forEach([&](const auto& kvd, auto&& kv) { write(kvd, std::forward<decltype(kv)>(kv), c); },
                  std::forward<KVS>(kvs));
}

template <typename Container, typename KVS,
          std::enable_if_t<
              (IsKeyValueSet_v<std::decay_t<KVS>> && KeyValueWriter<std::decay_t<Container>>::isSpecialized), bool>
          = true>
Container write(KVS&& kvs) {
    Container c{};
    write(std::forward<KVS>(kvs), c);
    return c;
}


//-----------------------------------------------------------------------------
// WriteSpec for KeyValueSet - make it possible to nest things
//
// ALso note that this is overloaded for some types

template <typename KeySet_, typename Container>
struct WriteSpec<KeyValueSet<KeySet_>, Container> {
    template <typename Val, std::enable_if_t<(std::is_same_v<std::decay_t<Val>, KeyValueSet<KeySet_>>
                                              && KeyValueWriter<std::decay_t<Container>>::isSpecialized),
                                             bool>
                            = true>
    static Container write(Val&& val) noexcept(noexcept(datamod::write<Container>(std::forward<Val>(val)))) {
        return datamod::write<Container>(std::forward<Val>(val));
    }
};


//-----------------------------------------------------------------------------
// Shorter way to nest keysets in each other
//-----------------------------------------------------------------------------


template <auto id_, typename NestedEnum>
constexpr auto nestedOptKeyDef(std::string_view keyName = KeySetName_v<NestedEnum>) {
    return KeyDef<id_, KeyValueSet<KeySet<NestedEnum>>>{keyName}.tagOptional();
}

template <auto id_, typename NestedEnum>
constexpr auto nestedKeyDef(std::string_view keyName = KeySetName_v<NestedEnum>) {
    return KeyDef<id_, KeyValueSet<KeySet<NestedEnum>>>{keyName}.withDefault([]() {
        // Default is to set the KeyValueSet and call alter on it
        KeyValueSet<KeySet<NestedEnum>> ret{};
        alterKeys(ret);
        return ret;
    });
}


// Access a nested key more easily, alter unset keys if missing
template <auto id_, auto... idx, typename KVS, std::enable_if_t<(sizeof...(idx) == 0), bool> = true>
decltype(auto) alteredKeyPath(KVS& conf) {
    return datamod::key<id_>(conf);
}

template <auto id1, auto... idx, typename KVS, std::enable_if_t<(sizeof...(idx) > 0), bool> = true>
decltype(auto) alteredKeyPath(KVS& conf) {
    auto& v1 = datamod::key<id1>(conf);
    // a solid alter(v1) is not enough
    // this accessor should also put in values even if nested keys are not defaulted
    if (v1.isMissing()) {
        using ValueType = datamod::KeyDefValueType_t<id1>;
        v1.set(ValueType{});
        alterKeys(v1.modify());
    }
    return alteredKeyPath<idx...>(v1.modify());
}


// Read key path
template <auto id_, auto... idx, typename KVS, std::enable_if_t<(sizeof...(idx) == 0), bool> = true>
decltype(auto) keyPath(KVS& conf) {
    return datamod::key<id_>(conf);
}

template <auto id1, auto... idx, typename KVS,
          std::enable_if_t<((sizeof...(idx) > 0) && std::is_const_v<std::remove_reference_t<KVS>>), bool> = true>
decltype(auto) keyPath(KVS& conf) {
    auto& v1 = datamod::key<id1>(conf);
    return keyPath<idx...>(v1.get());
}
template <auto id1, auto... idx, typename KVS,
          std::enable_if_t<((sizeof...(idx) > 0) && !std::is_const_v<std::remove_reference_t<KVS>>), bool> = true>
decltype(auto) keyPath(KVS& conf) {
    auto& v1 = datamod::key<id1>(conf);
    return keyPath<idx...>(v1.modify());
}

//-----------------------------------------------------------------------------
// Utilities
//-----------------------------------------------------------------------------


template <typename KeySet_>
std::ostream& operator<<(std::ostream& os, const multio::datamod::KeyValueSet<KeySet_>& kvs) {
    os << "{ ";
    bool first = true;
    multio::util::forEach(
        [&](const auto& key, const auto& value) {
            using ReadWrite = typename std::decay_t<decltype(value)>::ReadWrite;

            if (first) {
                first = false;
            }
            else {
                os << ", ";
            }

            os << key.key() << ": ";
            if (value.isMissing()) {
                os << "<MISSING>";
            }
            else {
                os << ReadWrite::template write<std::ostream>(value.get());
            }
            os << "\n";
        },
        kvs);
    os << "}";
    return os;
}


// Printing something readable to ostream
template <auto id_>
std::ostream& operator<<(std::ostream& os, const multio::datamod::KeyValue<id_>& kv) {
    using ReadWrite = typename multio::datamod::KeyValue<id_>::ReadWrite;

    // os << KeySetName_v<decltype(id_)> << "::" << key<id_>().key() << "=";
    os << util::typeToString<KeyId<id_>>() << "=";
    if (kv.isMissing()) {
        os << "<MISSING>";
    }
    else {
        os << ReadWrite::template write<std::ostream>(kv.get());
    }
    return os;
}


}  // namespace multio::datamod


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

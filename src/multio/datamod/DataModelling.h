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
#include "eckit/utils/Overloaded.h"
#include "multio/datamod/DataModellingException.h"
#include "multio/datamod/ReaderWriter.h"
#include "multio/util/Hash.h"
#include "multio/util/PrehashedKey.h"
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


namespace multio::datamod {

//-----------------------------------------------------------------------------
// Helpers to perform access on tuple through enum
// Requires contained types to have a `::id` defined
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


//-----------------------------------------------------------------------------
// Definitions to describe key-value pairs
//-----------------------------------------------------------------------------


// Forward declaration
enum class KVTag : std::uint64_t
{
    Required,   // Strictly required and can not be defaulted or conditionally depending on other keys
    Defaulted,  // Can be missing after reading from container but then may be defaulted through a custom alter function
    Optional,   // Can be missing after validation
};

template <typename KVTag_, std::enable_if_t<std::is_same_v<KVTag_, KVTag>, bool> = true>
std::string toString(KVTag_ t) {
    switch (t) {
        case KVTag::Required:
            return "required";
        case KVTag::Defaulted:
            return "defaulted";
        default:
            return "optional";
    }
}


// Dummy type as default
struct NoDefaultFunctor {};

template <typename ValueType, typename Mapper>
struct ReadWriteSpecs {
    template <typename V>
    inline static constexpr bool CanCreateFromValue_v = HasRead_v<Reader<ValueType, Mapper>, V>;

    template <typename Val, std::enable_if_t<CanCreateFromValue_v<Val>, bool> = true>
    static decltype(auto) read(Val&& val) {
        return Reader<ValueType, Mapper>::read(std::forward<Val>(val));
    }

    template <typename Container, typename Val>
    static decltype(auto) write(Val&& val) {
        return Writer<ValueType, Container, Mapper>::write(std::forward<Val>(val));
    }
};

// Base key definition without id specialization
template <typename ValueType_, typename Mapper_, KVTag tag_, typename DefaultValueFunctor>
struct BaseKeyDef {
    using ValueType = ValueType_;
    using Mapper = Mapper_;
    using This = BaseKeyDef<ValueType_, Mapper_, tag_, DefaultValueFunctor>;

    using ReadWrite = ReadWriteSpecs<ValueType, Mapper>;

    static constexpr KVTag tag = tag_;

    static constexpr bool hasDefaultValueFunctor = !std::is_same_v<DefaultValueFunctor, NoDefaultFunctor>;

    //---------------------------------
    // Accessors
    //---------------------------------

    // To be removed in the future, used as for string replacement
    const std::string_view& key() const noexcept { return key_; }
    ValueType defaultValue() const noexcept {
        static_assert(hasDefaultValueFunctor, "No default functor given");
        return defaultFunctor_();
    }
    const std::optional<std::string_view>& description() const noexcept { return description_; }


    //---------------------------------
    // Static methods
    //---------------------------------

    // Members - all "simple" to be constexpr constructable. Would be more relaxed with C++20, but it's all we need
    std::string_view key_;
    std::optional<std::string_view> description_;
    DefaultValueFunctor defaultFunctor_;
};


// KeyDef with ID specialization. Used for definition only.
template <auto id_, typename ValueType_, typename Mapper_ = DefaultMapper, KVTag tag_ = KVTag::Required,
          typename DefaultValueFunctor = NoDefaultFunctor>
struct KeyDef : BaseKeyDef<ValueType_, Mapper_, tag_, DefaultValueFunctor> {
    using Base = BaseKeyDef<ValueType_, Mapper_, tag_, DefaultValueFunctor>;
    using This = KeyDef<id_, ValueType_, Mapper_, tag_, DefaultValueFunctor>;
    using Mapper = typename Base::Mapper;
    using ValueType = typename Base::ValueType;
    using ReadWrite = typename Base::ReadWrite;

    static const auto id = id_;

    const Base& baseDef() const { return static_cast<const Base&>(*this); }

    //---------------------------------
    // Mutation
    //---------------------------------

    // Make the key-value pair optional - meaning it can be missing after alter & validation
    constexpr auto tagOptional() const {
        static_assert(tag_ != KVTag::Defaulted, "Definition is already defaulted and can not be made optional");
        return KeyDef<id_, ValueType_, Mapper_, KVTag::Optional, DefaultValueFunctor>{Base::key_, Base::description_,
                                                                                      Base::defaultFunctor_};
    }

    // Make the key-value pair defaulted - meaning it will be set through default functor or alter function and is
    // guaranteed to contain a value after validation
    constexpr auto tagDefaulted() const {
        static_assert(tag_ != KVTag::Defaulted, "Definition is already defaulted");
        return KeyDef<id_, ValueType_, Mapper_, KVTag::Optional, DefaultValueFunctor>{Base::key_, Base::description_,
                                                                                      Base::defaultFunctor_};
    }

    // Make the key-value pair defaulted and set a functon that generates a default value
    template <typename NewDefValFtor,
              std::enable_if_t<!std::is_convertible_v<NewDefValFtor, ValueType>
                                   && std::is_convertible_v<std::invoke_result_t<NewDefValFtor>, ValueType>,
                               bool>
              = true>
    constexpr auto withDefault(NewDefValFtor&& ftor) const {
        return KeyDef<id_, ValueType_, Mapper_, KVTag::Defaulted, std::decay_t<NewDefValFtor>>{
            Base::key_, Base::description_, std::forward<NewDefValFtor>(ftor)};
    }

    // Make the key-value pair defaulted and set a default value (need to be constexpr literal type, or wrap generation
    // of ValueType in a lambda otherwise)
    template <typename Val_, std::enable_if_t<std::is_convertible_v<Val_, ValueType>, bool> = true>
    constexpr auto withDefault(Val_ v) const {
        return withDefault([v = std::move(v)]() { return v; });
    }
    // Sets the description of the value
    constexpr auto withDescription(std::string_view descr) const {
        return This{Base::key_, Base::description_, std::move(Base::defaultFunctor_)};
    }
};


//-----------------------------------------------------------------------------

template <typename T>
struct IsBaseKeyDef {
    static constexpr bool value = false;
};
template <typename ValueType, typename Mapper, KVTag tag, typename DefFunctor>
struct IsBaseKeyDef<BaseKeyDef<ValueType, Mapper, tag, DefFunctor>> {
    static constexpr bool value = true;
};

template <typename T>
inline constexpr bool IsBaseKeyDef_v = IsBaseKeyDef<T>::value;


template <typename T>
struct IsKeyDefinition {
    static constexpr bool value = false;
};
template <typename ValueType, typename Mapper, KVTag tag, typename DefFunctor>
struct IsKeyDefinition<BaseKeyDef<ValueType, Mapper, tag, DefFunctor>> {
    static constexpr bool value = true;
};
template <auto id, typename ValueType, typename Mapper, KVTag tag, typename DefFunctor>
struct IsKeyDefinition<KeyDef<id, ValueType, Mapper, tag, DefFunctor>> {
    static constexpr bool value = true;
};

template <typename T>
inline constexpr bool IsKeyDefinition_v = IsKeyDefinition<T>::value;


//-----------------------------------------------------------------------------
// Definitions to handle key sets
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


//-----------------------------------------------------------------------------

// Virtual key description to avoid to much code specialization for io with containers
template <typename ValueType_, typename Mapper_, KVTag tag_, bool hasDefaultValueFunctor_>
struct DynamicKey {
    using KeyType = util::PrehashedKey<std::string>;

    using ValueType = ValueType_;
    using Mapper = Mapper_;
    using This = DynamicKey<ValueType, Mapper, tag_, hasDefaultValueFunctor_>;

    using ReadWrite = ReadWriteSpecs<ValueType, Mapper>;

    static constexpr KVTag tag = tag_;
    static constexpr bool hasDefaultValueFunctor = hasDefaultValueFunctor_;

    // To be removed in future when glossary is refactored
    operator const KeyType&() const { return key(); }
    operator const std::string&() const { return key(); }

    virtual const KeyType& key() const = 0;
    virtual ValueType defaultValue() const = 0;
    virtual std::string keyInfo() const = 0;
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

    ValueType defaultValue() const override {
        if constexpr (hasDefaultValueFunctor) {
            return keyDef<id_>().defaultValue();
        }
        throw DataModellingException(std::string("Critical. No default functor given for key: ") + keyInfo(), Here());
    }
    const std::optional<std::string_view>& description() const noexcept { return keyDef<id_>().description(); }


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
        scope_ = KeySetScope::None;
        return static_cast<Derived&>(*this);
    };

    // Set scope in place
    This& scoped(std::optional<std::string> customScope = {}) {
        if (customScope) {
            scope_ = KeySetScope::Custom;
            customScopedKeys_
                = util::map([&](const auto& kdef) { return toScopedKey(kdef, *customScope); }, GetKeyPolicy::getKeys());
        }
        else {
            scope_ = KeySetScope::Default;
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
        switch (scope_) {
            case KeySetScope::Default:
                return GetKeyPolicy::getScopedKeys();
            case KeySetScope::Custom:
                return customScopedKeys_.value();
            default:
                return GetKeyPolicy::getKeys();
        }
    }


private:
    KeySetScope scope_{KeySetScope::None};
    std::optional<TupleType> customScopedKeys_{};
};


template <typename EnumType>
struct KeySetGetKeysPolicy {
    static_assert(util::IsTuple_v<std::decay_t<decltype(StaticKeySetStore<EnumType>::keys())>>, "Expected a tuple");

    static const auto& getKeys() { return StaticKeySetStore<EnumType>::keys(); }
    static const auto& getScopedKeys() { return StaticKeySetStore<EnumType>::scopedKeys(); }
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


//-----------------------------------------------------------------------------
// Value containers
//-----------------------------------------------------------------------------

struct MissingValue {};

// Operators for missing value ... use SFINAE to let this code remain header only
// Also implement operator for other types to simplify comparison implementation for KeyValue
template <typename MV1, typename MV2,
          std::enable_if_t<(std::is_same_v<MV1, MissingValue> || std::is_same_v<MV2, MissingValue>), bool> = true>
bool operator==(const MV1& lhs, const MV2& rhs) noexcept {
    return std::is_same_v<MV1, MissingValue> && std::is_same_v<MV2, MissingValue>;
}

template <typename MV1, typename MV2,
          std::enable_if_t<(std::is_same_v<MV1, MissingValue> || std::is_same_v<MV2, MissingValue>), bool> = true>
bool operator!=(const MV1& lhs, const MV2& rhs) noexcept {
    return !(lhs == rhs);
}


template <typename ValueType_, typename Mapper_>
struct BaseKeyValue {
    using ValueType = ValueType_;
    using Mapper = Mapper_;
    using ReadWrite = ReadWriteSpecs<ValueType, Mapper>;
    using This = BaseKeyValue<ValueType, Mapper>;

    using RefType = std::reference_wrapper<const ValueType>;
    using Container = std::variant<MissingValue, ValueType, RefType>;

    // Actual value
    Container value;

    bool isMissing() const { return std::holds_alternative<MissingValue>(value); }
    bool has() const { return !std::holds_alternative<MissingValue>(value); }
    bool holdsReference() const { return std::holds_alternative<const ValueType>(value); }

    // Using polymorphism for keyInfo to avoid needing too finegrained types for each key
    // virtual std::string keyInfo() const { return ""; };

    // Function to get the contained value if it's not missing - due to the possibility of containing a reference,
    // only const& versions can get Optimized rvalue handling is achieved through visit
    const ValueType& get() const {
        return std::visit(
            eckit::Overloaded{
                [&](const ValueType& val) -> const ValueType& { return val; },
                [&](const RefType& val) -> const ValueType& { return val.get(); },
                [&](const MissingValue&) -> const ValueType& {
                    throw DataModellingException(
                        std::string("Unchecked call to `BaseKeyValue::get()` (missing value). Seems like an "
                                    "unvalidated object has been accessed?"),
                        Here());
                },
            },
            value);
    }
    operator const ValueType&() const { return get(); }

    ValueType& modify() {
        return std::visit(
            eckit::Overloaded{
                [&](ValueType& val) -> ValueType& { return val; },
                [&](RefType& val) -> ValueType& {
                    this->value = val.get();
                    return std::get<ValueType>(this->value);
                },
                [&](const MissingValue&) -> ValueType& {
                    throw DataModellingException(
                        std::string("Unchecked call to `BaseKeyValue::modify()` (missing value). Seems like an "
                                    "unvalidated object has been accessed?"),
                        Here());
                },
            },
            value);
    }
    operator ValueType&() { return modify(); }

    void setMissing() noexcept { value = MissingValue{}; }

    template <typename V, std::enable_if_t<(std::is_same_v<std::decay_t<V>, ValueType>), bool> = true>
    void set(V&& v) {
        value = std::forward<V>(v);
    }
    template <typename V, std::enable_if_t<(!std::is_same_v<std::decay_t<V>, MissingValue>
                                            && !std::is_same_v<std::decay_t<V>, RefType>
                                            && !std::is_same_v<std::decay_t<V>, ValueType>),
                                           bool>
                          = true>
    void set(V&& v) {
        value = ReadWrite::read(std::forward<V>(v));
    }
    template <typename V,
              std::enable_if_t<
                  (std::is_same_v<std::decay_t<V>, MissingValue> || std::is_same_v<std::decay_t<V>, RefType>), bool>
              = true>
    void set(V&& v) {
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


    // Make sure no reference is hold and value is owned
    void acquire() {
        std::visit(eckit::Overloaded{
                       [&](const RefType& val) { this->value = val.get(); },
                       [&](auto) {},
                   },
                   value);
    }

    template <typename Func>
    This& withDefault(Func&& func) {
        if (isMissing()) {
            this->set(std::forward<Func>(func)());
        }
        return *this;
    }
};


template <typename T>
struct KeyValueFromKey;

template <typename T>
using KeyValueFromKey_t = typename KeyValueFromKey<T>::type;

template <typename ValueType, typename Mapper, KVTag tag, bool hasDefaultValueFunctor>
struct KeyValueFromKey<DynamicKey<ValueType, Mapper, tag, hasDefaultValueFunctor>> {
    using type = BaseKeyValue<ValueType, Mapper>;
};


template <auto id_>
struct KeyValue : BaseKeyValue<KeyDefValueType_t<id_>, KeyDefMapper_t<id_>> {
    using Base = BaseKeyValue<KeyDefValueType_t<id_>, KeyDefMapper_t<id_>>;
    using This = KeyValue<id_>;
    using Definition = KeyDef_t<id_>;
    using ValueType = KeyDefValueType_t<id_>;
    using Mapper = KeyDefMapper_t<id_>;
    using ReadWrite = ReadWriteSpecs<ValueType, Mapper>;

    static const auto id = id_;

    // using Base::Base;
    // std::string keyInfo() const override { return key<id_>().keyInfo(); }

    const Base& baseRef() const& noexcept { return static_cast<const Base&>(*this); };
    Base& baseRef() & noexcept { return static_cast<Base&>(*this); };
    Base baseRef() && noexcept { return std::move(*this); };

    const ValueType& get() const {
        try {
            return Base::get();
        }
        catch (...) {
            std::throw_with_nested(DataModellingException(
                std::string("`Nested exception while calling KeyValue::get()` for key " + key<id_>().keyInfo()),
                Here()));
        }
    }
    operator const ValueType&() const { return get(); }

    ValueType& modify() {
        try {
            return Base::modify();
        }
        catch (...) {
            std::throw_with_nested(DataModellingException(
                std::string("`Nested exception while calling KeyValue::modify()` for key " + key<id_>().keyInfo()),
                Here()));
        }
    }

    This& setMissingOrDefaultValue() {
        if constexpr (Definition::hasDefaultValueFunctor) {
            this->set(key<id_>().defaultValue());
        }
        else {
            this->set(MissingValue{});
        }
        return *this;
    }

    // Set default if value is missing
    // TODO remove in favor of global function...
    [[deprecated]]
    This& alter() {
        if constexpr (Definition::hasDefaultValueFunctor) {
            if (this->isMissing()) {
                this->set(key<id_>().defaultValue());
            }
        }
        return *this;
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

template <auto id>
struct KeyValueFromKey<ScopedKey<id>> {
    using type = KeyValue<id>;
};


//-----------------------------------------------------------------------------


template <typename T>
struct IsBaseKeyValue {
    static constexpr bool value = false;
};
template <typename Val, typename Mapper>
struct IsBaseKeyValue<BaseKeyValue<Val, Mapper>> {
    static constexpr bool value = true;
};

template <typename T>
inline constexpr bool IsBaseKeyValue_v = IsBaseKeyValue<T>::value;


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


template <typename KVD, std::enable_if_t<IsKeyDefinition_v<KVD>, bool> = true>
KeyValueFromKey_t<KVD> toMissingValue(const KVD&) {
    return KeyValueFromKey_t<KVD>{MissingValue{}};
}


template <typename KVD, std::enable_if_t<IsKeyDefinition_v<KVD>, bool> = true>
KeyValueFromKey_t<KVD> toMissingOrDefaultValue(const KVD& kvd) {
    if constexpr (KVD::hasDefaultValueFunctor) {
        return KeyValueFromKey_t<KVD>{kvd.defaultValue()};
    }
    return KeyValueFromKey_t<KVD>{MissingValue{}};
}


// Creates a KeyValue and uses a reference_wrapper if possible
template <typename KVD, typename V,
          std::enable_if_t<IsKeyDefinition_v<KVD> && !IsKeyValue_v<std::decay_t<V>>, bool> = true>
KeyValueFromKey_t<KVD> toKeyValueRef(const KVD& kvd, V&& v) {
    using KV = KeyValueFromKey_t<KVD>;
    if constexpr (util::IsOptional_v<std::decay_t<V>>) {
        if (v) {
            return toKeyValueRef(kvd, std::forward<V>(v).value());
        }
        else {
            return KV{};
        }
    }
    else {
        if constexpr (std::is_same_v<std::decay_t<V>, MissingValue>) {
            return KV{};
        }
        else if constexpr (std::is_same_v<std::decay_t<V>, typename KV::ValueType>) {
            if constexpr (!std::is_lvalue_reference_v<V>) {
                return KV{std::move(v)};
            }
            else {
                return KV{typename KV::RefType(v)};
            }
        }
        else {
            if constexpr (!std::is_lvalue_reference_v<V>) {
                return KV{KV::ReadWrite::read(std::move(v))};
            }
            else {
                return KV{KV::ReadWrite::read(v)};
            }
        }
    }
    return KV{};  // unreachable - prevent compiler warning
}

// Creates a KeyValue and always copies values
template <typename KVD, typename V,
          std::enable_if_t<IsKeyDefinition_v<KVD> && !IsKeyValue_v<std::decay_t<V>>, bool> = true>
decltype(auto) toKeyValue(const KVD& kvd, V&& v) {
    auto res = toKeyValueRef(kvd, std::forward<V>(v));
    res.acquire();
    return res;
}


//-----------------------------------------------------------------------------


// Takes a tuple of KeyDefinition and creates an instance of the keyset with all fields set to missing
template <typename DescTup, std::enable_if_t<(util::IsTuple_v<std::decay_t<DescTup>>
                                              && IsKeyDefinition_v<std::tuple_element_t<0, std::decay_t<DescTup>>>),
                                             bool>
                            = true>
decltype(auto) reify(DescTup&& tup) {
    return util::map([&](const auto& kvd) { return toMissingValue(kvd); }, std::forward<DescTup>(tup));
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
                                          && (IsKeyValue_v<std::tuple_element_t<0, Tup>>
                                              || IsBaseKeyValue_v<std::tuple_element_t<0, Tup>>)),
                                         bool>
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
template <
    typename ValTup,
    std::enable_if_t<(util::IsTuple_v<std::decay_t<ValTup>> && IsKeyValue_v<std::tuple_element_t<0, ValTup>>), bool>
    = true>
void validate(const ValTup& tup) {
    util::forEach([&](const auto& kv) { validate(kv); }, tup);
}


// Takes a tuple of KeyValue and verifies that all required keys are set
template <
    typename ValTup,
    std::enable_if_t<(util::IsTuple_v<std::decay_t<ValTup>> && IsKeyValue_v<std::tuple_element_t<0, ValTup>>), bool>
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
template <
    typename ValTup,
    std::enable_if_t<(util::IsTuple_v<std::decay_t<ValTup>> && IsKeyValue_v<std::tuple_element_t<0, ValTup>>), bool>
    = true>
ValTup& alterAndValidate(ValTup& tup) {
    alter(tup);
    validate(tup);
    return tup;
}


//-----------------------------------------------------------------------------
// Reading from other containers
//-----------------------------------------------------------------------------

// KeyId tag used for overload resolution in function calls
template <auto id_>
struct KeyId {
    static constexpr auto id = id_;
};


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
                                && IsKeyValue_v<std::tuple_element_t<0, std::decay_t<KVTup>>>),
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
                                && IsKeyValue_v<std::tuple_element_t<0, std::decay_t<KVTup>>>),
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
                            && IsScopedKey_v<std::tuple_element_t<0, std::decay_t<DescTup>>>
                            && KeyValueReader<std::decay_t<Container>>::isSpecialized),
                           bool>
          = true>
decltype(auto) read(DescTup&& tup, Container&& c) {
    return util::map([&](const auto& kvd) { return read(kvd, std::forward<Container>(c)); },
                     std::forward<DescTup>(tup));
}
template <typename DescTup, typename Container,
          std::enable_if_t<(util::IsTuple_v<std::decay_t<DescTup>>
                            && IsScopedKey_v<std::tuple_element_t<0, std::decay_t<DescTup>>>
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
decltype(auto) read(KS&& ks, Container&& c) {
    auto values = read(ks.keys(), std::forward<Container>(c));
    KeyValueSet<std::decay_t<KS>> ret{std::forward<KS>(ks), std::move(values)};
    alterAndValidate(ret);
    return ret;
}

template <
    typename KS, typename Container,
    std::enable_if_t<(IsKeySet_v<std::decay_t<KS>> && KeyValueReader<std::decay_t<Container>>::isSpecialized), bool>
    = true>
decltype(auto) readByValue(KS&& ks, Container&& c) {
    auto values = readByValue(ks.keys(), std::forward<Container>(c));
    KeyValueSet<std::decay_t<KS>> ret{std::forward<KS>(ks), std::move(values)};
    alterAndValidate(ret);
    return ret;
}


//-----------------------------------------------------------------------------
// ReadSpec for KeyValueSet - make it possible to nest things
//

template <typename KeySet_>
struct ReadSpec<KeyValueSet<KeySet_>> {
    template <typename Val, std::enable_if_t<KeyValueReader<std::decay_t<Val>>::isSpecialized, bool> = true>
    static KeyValueSet<KeySet_> read(Val&& val) noexcept(noexcept(datamod::read(KeySet_{}, std::forward<Val>(val)))) {
        // TODO find a mechanism to determine whether by value or ref is the better default
        // Currently, for nested structures this behaviour is not controlled proprely.
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

template <
    typename KVTup, typename Container,
    std::enable_if_t<(util::IsTuple_v<std::decay_t<KVTup>> && IsKeyValue_v<std::tuple_element_t<0, std::decay_t<KVTup>>>
                      && KeyValueWriter<std::decay_t<Container>>::isSpecialized),
                     bool>
    = true>
void write(KVTup&& tup, Container& c) {
    util::forEach([&](auto&& kv) { write(std::forward<decltype(kv)>(kv), c); }, std::forward<KVTup>(tup));
}

template <
    typename Container, typename KVTup,
    std::enable_if_t<(util::IsTuple_v<std::decay_t<KVTup>> && IsKeyValue_v<std::tuple_element_t<0, std::decay_t<KVTup>>>
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
    template <typename Val, std::enable_if_t<(std::is_same_v<std::decay_t<Val>, KeyValueSet<KeySet_>> && KeyValueWriter<std::decay_t<Container>>::isSpecialized), bool> = true>
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

template <auto id1, auto... idx, typename KVS, std::enable_if_t<(sizeof...(idx) > 0), bool> = true>
decltype(auto) keyPath(KVS& conf) {
    auto& v1 = datamod::key<id1>(conf);
    return keyPath<idx...>(v1.get());
}

//-----------------------------------------------------------------------------
// Utilities
//-----------------------------------------------------------------------------


template <typename KeySet_>
std::ostream& operator<<(std::ostream& os, const multio::datamod::KeyValueSet<KeySet_>& kvs) {
    os << "{";
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

            os << key.key() << "=";
            if (value.isMissing()) {
                os << "<MISSING>";
            }
            else {
                os << ReadWrite::template write<std::ostream>(value.get());
            }
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


namespace multio::util {
template <typename EnumType>
struct TypeToString<datamod::KeySet<EnumType>> {
    std::string operator()() const {
        return std::string("datamod::KeySet<") + std::string(datamod::KeySetName_v<EnumType>) + std::string(">");
    };
};


template <auto id>
struct TypeToString<datamod::KeyId<id>> {
    std::string operator()() const {
        return std::string(datamod::KeySetName_v<decltype(id)>) + std::string("::") + std::string(datamod::key<id>().key());
    };
};

template <auto... Ids>
struct TypeToString<datamod::CustomKeySet<Ids...>> {
    std::string operator()() const {
        return std::string("datamod::CustomKeySet<") + ((typeToString<datamod::KeyId<Ids>>() + std::string(", ")), ...)
             + std::string(">");
    };
};

template <typename KeySet_>
struct TypeToString<datamod::KeyValueSet<KeySet_>> {
    std::string operator()() const {
        return std::string("datamod::KeyValueSet<") + util::typeToString<KeySet_>() + std::string(">");
    };
};
}  // namespace multio::util


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

/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#pragma once

#include <functional>
#include <optional>
#include <string_view>
#include <type_traits>
#include "multio/datamod/core/Entry.h"
#include "multio/datamod/core/Record.h"
#include "multio/datamod/core/TypeParserDumper.h"

#include "multio/util/PrehashedKey.h"
#include "multio/util/TypeToString.h"


namespace multio::datamod {

//-----------------------------------------------------------------------------
// Definitions to describe key-value pairs
//-----------------------------------------------------------------------------

/// \defgroup datamod_core_entrydef EntryDef
/// \ingroup datamod_core
/// # Using entry definitions
///
/// The following methods/members can be used/accessed on an entry definition:
///   * `.tag`: Access the constexpr `EntryTag`
///   * `.key()`: Returns the string accessor
///   * `.keyInfo()`: String with information about the entry name, its type and tag.
///   * `.description()`: Returns the provided description (may be used for documentation? Still unsure)
///   * `.get(MyRecord)`: Accesses the entry on a record.
///   * `.defaultValue()`: Returns the passed default value. Only possible if `::hasDefaultValueFunctor` is `false`.
///   * `.applyDefaults(Entry)`: Apply defaults if any defaults are given
///   * `.validate(Entry)`: Validates an entry - throws `DataModellingException` if a non optional entry is missing.
///   * `.makeEntry(V&&)`: Creates an entry from a value - may perform conversion.
///   * `.makeEntryRef(V&&)`: Creates an entry explicitly as reference, if the passed type is the same as the internal
///   representation.
///
/// # Defining entries
///
/// Entries are defined via the template `EntryDef<Type, Mapper>{"string-accessor"}.withAcessor([](auto&& v){return
/// &v.structAccessor;})`, where
///   * `Type` is the used type used to represent values for the entry
///   * `Mapper` is an optional mapping functor that allows custom mapping from other types.
///      E.g. `ParamMapper` for `param` that handles string representations.
///      It is also possible that `Type` is more complex and supports constructions from other types.
///   * the first argument in the constructor is a constexpr string that is used to access (parse/dump) the entry
///     from/to containers (i.e. `JSON`)
///   * `withAccesosr(...)` accepts an indefinite lambda that allows accessing the entry in a record.
///     It is expected to return a pointer to an entry - usally by accessing the proper `struct` member.
///     Through this mechanisms, an entry can occur in different places and is not strictly bound to one composed type.
///
/// ```
/// #include "multio/datamod/core/EntryDef.h"
/// namespace dm = multio::datamod;
///
/// constexpr auto MyEntry = dm::EntryDef<std::string>{"my-entry"}.withAccessor([](auto&& v){return &v.myEntry;});
/// ```
///
/// Entries hold a tag `EntryTag` that can be accesed via `.tag` and can hold either:
///   * `EntryTag::Required` (default) - must be specified when parsing from a container
///   * `EntryTag::Defaulted` - has a direct default value or it`s default value may be depended on other entries -
///      that means it may be set through the `applyRecord(RecordType&)` specialization.
///   * `EntryTag::Optional` to declare an entry is optional.
///   * or `EntryTag::Disallowed` to declare an entry is not allowed to be present.
///
/// When calling `EntryDef.validate(Entry)`, an `DataModellingException` is throw if the entry definition is not tagged
/// optional or if a disallowed entry is set.
///
/// ```
/// // Make an optional entry
/// constexpr auto MyOptionalEntry = dm::EntryDef<std::string>{"my-entry"}
///     .withAccessor([](auto&& v){ return &v.myEntry; })
///     .tagOptional();
///
/// // Provide default value
/// constexpr auto MyDefaultedEntry = dm::EntryDef<std::string>{"my-entry"}
///     .withAccessor([](auto&& v){ return &v.myEntry; })
///     .withDefault("lalelu");
///
/// // Provide default value for non-constexpress types
/// constexpr auto MyDefaultedEntry = dm::EntryDef<std::string>{"my-entry"}
///     .withAccessor([](auto&& v){ return &v.myEntry; })
///     .withDefault([](){ return  "lalelu"; });
///
/// // You may also retag existing entry definitions, for example to make optional entries required again, or to make
/// // entries disallowed
/// myOptionalEntry.tagRequired();
/// myOptionalEntry.tagDisallowed();
/// ```
///

// Forward declaration
enum class EntryTag : std::uint64_t
{
    Required,    // Strictly required and can not be defaulted or conditionally depending on other keys
    Defaulted,   // Can be missing after reading from container but then may be defaulted through a custom alter function
    Optional,    // Can be missing after validation
    Disallowed,  // Must be missing after validation
};

template <typename EntryTag_, std::enable_if_t<std::is_same_v<EntryTag_, EntryTag>, bool> = true>
std::string toString(EntryTag_ t) {
    switch (t) {
        case EntryTag::Required:
            return "required";
        case EntryTag::Defaulted:
            return "defaulted";
        case EntryTag::Optional:
            return "optional";
        case EntryTag::Disallowed:
            return "disallowed";
    }
    throw DataModellingException("Cannot convert EntryTag to string", Here());
}


// Dummy type as default value functor
// indication that no directy default value is assigned to that key
struct NoDefaultFunctor {};


template <typename DefaultValueFunctor>
struct ApplyDefaultValueFunctor {
    static constexpr bool hasDefaultValueFunctor = !std::is_same_v<DefaultValueFunctor, NoDefaultFunctor>;

    template <typename EntryType_>
    void operator()(EntryType_& v, const DefaultValueFunctor& f) const {
        // Only optional tagged keys can be missing
        if constexpr (hasDefaultValueFunctor) {
            if (!v.isSet()) {
                v.set(f());
            }
        }

        // Check for nested alter recursively
        if constexpr (IsRecord_v<typename std::decay_t<EntryType_>::ValueType>) {
            if (v.isSet()) {
                applyRecordDefaults(v.modify());
            }
        }
    }
};

// Dummy accessor ... used until accessor is set at compile time
struct DefaultPointerAccessor {};

// Helper to get members from a record
// The `PointerAccesser` is ment to be a indefinite lambda function `[](auto&&) -> auto&&`
// that returns a pointer to a given member.
template <typename PointerAccessor, typename EntryType>
struct AccessFunctor {
    // Get const&
    template <typename Container,
              std::enable_if_t<
                  std::is_lvalue_reference_v<Container> && std::is_const_v<std::remove_reference_t<Container>>, bool>
              = true>
    const EntryType& operator()(Container&& cont) const {
        static_assert(
            std::is_same_v<const EntryType*,
                           decltype(std::declval<PointerAccessor>()(std::declval<const std::decay_t<Container>&>()))>,
            "The accessor is expected to return a pointer to an entry");
        return *(accessor_(cont));
    }  // namespace multio::datamod

    // Get &
    template <typename Container,
              std::enable_if_t<
                  std::is_lvalue_reference_v<Container> && !std::is_const_v<std::remove_reference_t<Container>>, bool>
              = true>
    EntryType& operator()(Container&& cont) const {
        static_assert(
            std::is_same_v<EntryType*,
                           decltype(std::declval<PointerAccessor>()(std::declval<std::decay_t<Container>&>()))>,
            "The accessor is expected to return a pointer to an entry");
        return *(accessor_(cont));
    }

    // Get move
    template <typename Container, std::enable_if_t<!std::is_lvalue_reference_v<Container>, bool> = true>
    EntryType operator()(Container&& cont) const {
        static_assert(
            std::is_same_v<EntryType*,
                           decltype(std::declval<PointerAccessor>()(std::declval<std::decay_t<Container>&&>()))>,
            "The accessor is expected to return a pointer to an entry");
        return *(accessor_(std::move(cont)));
    }

    PointerAccessor accessor_;
};


/// Base key definition without id specialization
/// This definition is ment to be constructed as constexpr and in static const objects without beeing modified.
/// It is used to retrieve information like type, customized mappers, default value and a description description.
///
/// The BaseKeyDef has no "key specific" accessor and is used with Parser/Dumper.
/// This significantly reduces the binary size, otherwise every single key with same types will trigger generation of
/// code again. E.g. the test_multio_datamod_models where a lot of marskeys have the same type signature reduces
/// from 3.9 MB -> 1.7 MB (intel)
/// from 1.3 MB -> 0.7 MB (gcc)
///      It should be also noted that due to the branching with the Metadata a lot of detailed branches are created -
///       often this is basically a big jump table with specialized code instead of many conditional jumps
///
template <typename ValueType_, typename Mapper_, EntryTag tag_>
struct BaseEntryDef {
    using KeyType = util::PrehashedKey<std::string_view>;
    using ValueType = ValueType_;
    using Mapper = Mapper_;
    using This = BaseEntryDef<ValueType_, Mapper_, tag_>;

    using ParserDumper = TypeParserDumper<ValueType, Mapper>;
    using EntryType = Entry<ValueType_, Mapper_>;

    static constexpr EntryTag tag = tag_;


    //---------------------------------
    // Accessors
    //---------------------------------

    operator std::string() { return std::string(key_); }

    KeyType key() const noexcept { return key_; }


    const std::optional<std::string_view>& description() const noexcept { return description_; }

    std::string keyInfo() const {
        return std::string(key()) + std::string(" (") + util::typeToString<ValueType>() + std::string(", ")
             + toString(tag) + std::string{")"};
    }

    // Functions to create Entries

    // Creates a Entry and uses a reference_wrapper if possible
    template <typename V, std::enable_if_t<!IsEntry_v<std::decay_t<V>>, bool> = true>
    EntryType makeEntryRef(V&& v) const {
        if constexpr (util::IsOptional_v<std::decay_t<V>>) {
            if (v) {
                return makeEntryRef(std::forward<V>(v).value());
            }
            else {
                return EntryType{};
            }
        }
        else {
            if constexpr (std::is_same_v<std::decay_t<V>, UnsetType>) {
                return EntryType{};
            }
            else if constexpr (std::is_same_v<std::decay_t<V>, ValueType_>) {
                if constexpr (!std::is_lvalue_reference_v<V>) {
                    return EntryType{std::move(v)};
                }
                else {
                    return EntryType{typename EntryType::RefType(v)};
                }
            }
            else {
                if constexpr (!std::is_lvalue_reference_v<V>) {
                    return EntryType{ParserDumper::parse(std::move(v))};
                }
                else {
                    return EntryType{ParserDumper::parse(v)};
                }
            }
        }
        return {};  // Unreachable
    }

    // Creates a Entry and always copies values
    template <typename V, std::enable_if_t<!IsEntry_v<std::decay_t<V>>, bool> = true>
    EntryType makeEntry(V&& v) const {
        auto res = makeEntryRef(std::forward<V>(v));
        res.acquire();
        return res;
    }

    EntryType makeEntry() const { return EntryType{}; }


    // Validate an entry
    void validate(const EntryType& v) const {
        // Only optional tagged keys can be missing
        if constexpr (tag == EntryTag::Disallowed) {
            if (v.isSet()) {
                throw DataModellingException(std::string("Set disallowed key: ") + this->keyInfo(), Here());
            }
        }
        else if constexpr (tag != EntryTag::Optional) {
            if (!v.isSet()) {
                throw DataModellingException(std::string("Unset required key: ") + this->keyInfo(), Here());
            }
        }

        // Check for nested validation
        if constexpr (IsRecord_v<ValueType>) {
            if (v.isSet()) {
                validateRecord(v.get());
            }
        }
    }

    //-----------------------------------------------------


    // Members - all "simple" to be constexpr constructable. Would be more relaxed with C++20, but it's all we need
    KeyType key_;
    std::optional<std::string_view> description_{};
};


// EntryDef with ID specialization.
// Build on top of the BaseEntryDef and has modifiers that always create new constexpr types.
// It is used to specify a single key within a list of keys on a short and readable way.
//
// For interaction with containers the `id` is usually stripped away and the BaseEntryDef is passed around
// to avoid to fine-grined type specialization.
template <typename ValueType_, typename Mapper_ = DefaultMapper, EntryTag tag_ = EntryTag::Required,
          typename PointerAccessor = DefaultPointerAccessor, typename DefaultValueFunctor_ = NoDefaultFunctor>
struct EntryDef : BaseEntryDef<ValueType_, Mapper_, tag_> {
    using Base = BaseEntryDef<ValueType_, Mapper_, tag_>;
    using This = EntryDef<ValueType_, Mapper_, tag_, PointerAccessor, DefaultValueFunctor_>;
    using Mapper = typename Base::Mapper;
    using KeyType = typename Base::KeyType;
    using ValueType = typename Base::ValueType;
    using ParserDumper = typename Base::ParserDumper;

    using EntryType = typename Base::EntryType;
    using DefaultValueFunctor = DefaultValueFunctor_;
    using Accessor = AccessFunctor<PointerAccessor, EntryType>;

    static constexpr bool hasDefaultValueFunctor = !std::is_same_v<DefaultValueFunctor, NoDefaultFunctor>;

    EntryDef(const This& other) = default;
    EntryDef(This&& other) = default;
    This& operator=(const This&) = default;
    This& operator=(This&&) noexcept = default;

    constexpr EntryDef(KeyType key, std::optional<Accessor> accessor = {},
                       std::optional<DefaultValueFunctor> defaultFunctor = {},
                       std::optional<std::string_view> description = {}) :
        Base{key, description}, accessor_{std::move(accessor)}, defaultFunctor_{std::move(defaultFunctor)} {}

    // The only additional member
    std::optional<Accessor> accessor_{};                   // This is optional to savely allow default initialization
    std::optional<DefaultValueFunctor> defaultFunctor_{};  // This is optional to savely allow default initialization


    const Base& toBase() const { return static_cast<const Base&>(*this); }


    //-----------------------------------------------------


    ValueType defaultValue() const noexcept {
        static_assert(hasDefaultValueFunctor, "No default functor given");
        return (*defaultFunctor_)();
    }

    //-----------------------------------------------------

    // Apply defaults on entry

    // Takes a tuple of KeyValue and verifies that all required keys are set
    void applyDefaults(EntryType& v) const { ApplyDefaultValueFunctor<DefaultValueFunctor>{}(v, *defaultFunctor_); }


    // Entry specific accessors

    template <typename Container>
    decltype(auto) get(Container&& cont) const {
        return (*accessor_)(std::forward<Container>(cont));
    }

    //-----------------------------------------------------

    // Compile time modifiers

    // Make the key-value pair required - meaning it cannot be missing after alter & validation
    // Required is the default and this method should only be used to cancel out tagOptional(), for example, in action
    // specific metadata records
    constexpr auto tagRequired() const {
        static_assert(tag_ != EntryTag::Required, "Definition is already required");
        static_assert(tag_ != EntryTag::Defaulted, "Definition is already defaulted and can not be made required");
        return EntryDef<ValueType_, Mapper_, EntryTag::Required, PointerAccessor, DefaultValueFunctor>{
            Base::key_, accessor_, defaultFunctor_, Base::description_};
    }

    // Make the key-value pair defaulted - meaning it will be set through default functor or alter function and is
    // guaranteed to contain a value after validation
    constexpr auto tagDefaulted() const {
        static_assert(tag_ != EntryTag::Defaulted, "Definition is already defaulted");
        return EntryDef<ValueType_, Mapper_, EntryTag::Optional, PointerAccessor, DefaultValueFunctor>{
            Base::key_,
            accessor_,
            defaultFunctor_,
            Base::description_,
        };
    }

    // Make the key-value pair optional - meaning it can be missing after alter & validation
    constexpr auto tagOptional() const {
        static_assert(tag_ != EntryTag::Optional, "Definition is already optional");
        static_assert(tag_ != EntryTag::Defaulted, "Definition is already defaulted and can not be made optional");
        return EntryDef<ValueType_, Mapper_, EntryTag::Optional, PointerAccessor, DefaultValueFunctor>{
            Base::key_, accessor_, defaultFunctor_, Base::description_};
    }

    // Make the key-value pair optional - meaning it can be missing after alter & validation
    constexpr auto tagDisallowed() const {
        static_assert(tag_ != EntryTag::Disallowed, "Definition is already disallowed");
        static_assert(tag_ != EntryTag::Defaulted, "Definition is already defaulted and can not be made disallowed");
        return EntryDef<ValueType_, Mapper_, EntryTag::Disallowed, PointerAccessor, DefaultValueFunctor>{
            Base::key_, accessor_, defaultFunctor_, Base::description_};
    }

    // Make the key-value pair defaulted and set a functon that generates a default value
    template <typename NewDefValFtor,
              std::enable_if_t<!std::is_convertible_v<NewDefValFtor, ValueType>
                                   && std::is_convertible_v<std::invoke_result_t<NewDefValFtor>, ValueType>,
                               bool>
              = true>
    constexpr auto withDefault(NewDefValFtor&& ftor) const {
        return EntryDef<ValueType_, Mapper_, EntryTag::Defaulted, PointerAccessor, std::decay_t<NewDefValFtor>>{
            Base::key_, std::move(accessor_), std::forward<NewDefValFtor>(ftor), Base::description_};
    }

    // Make the key-value pair defaulted and set a default value (need to be constexpr literal type, or wrap
    // generation of ValueType in a lambda otherwise)
    template <typename Val_, std::enable_if_t<std::is_convertible_v<Val_, ValueType>, bool> = true>
    constexpr auto withDefault(Val_ v) const {
        return withDefault([v = std::move(v)]() { return v; });
    }

    // Sets the key of the value (to scope)
    constexpr auto withKey(KeyType key) const { return This{key, accessor_, defaultFunctor_, Base::description_}; }

    // Sets the description of the value
    constexpr auto withDescription(std::string_view descr) const {
        return This{Base::key_, accessor_, defaultFunctor_, descr};
    }


    // Make the key-value pair defaulted and set a functon that generates a default value
    template <typename NewAcc>
    constexpr auto withAccessor(NewAcc&& newAcc) const {
        return EntryDef<ValueType_, Mapper_, tag_, std::decay_t<NewAcc>, DefaultValueFunctor>{
            Base::key_, AccessFunctor<std::decay_t<NewAcc>, EntryType>{std::forward<NewAcc>(newAcc)}, defaultFunctor_,
            Base::description_};
    }
};


//-----------------------------------------------------------------------------

template <typename T>
struct IsBaseEntryDefinition : std::false_type {};
template <typename ValueType, typename Mapper, EntryTag tag>
struct IsBaseEntryDefinition<BaseEntryDef<ValueType, Mapper, tag>> : std::true_type {};

template <typename T>
inline constexpr bool IsBaseEntryDefinition_v = IsBaseEntryDefinition<T>::value;

/// C++20 Concept
// template <typename T>
// concept BaseEntryDefinitionType = IsBaseEntryDefinition<std::remove_cvref_t<T>>::value;

//-----------------------------------------------------------------------------

template <typename T>
struct IsEntryDefinition : std::false_type {};
template <typename ValueType, typename Mapper, EntryTag tag, typename Acc, typename DefFunctor>
struct IsEntryDefinition<EntryDef<ValueType, Mapper, tag, Acc, DefFunctor>> : std::true_type {};

template <typename T>
inline constexpr bool IsEntryDefinition_v = IsEntryDefinition<T>::value;

/// C++20 Concept
// template <typename T>
// concept EntryDefinitionType = IsEntryDefinition<std::remove_cvref_t<T>>::value;

//-----------------------------------------------------------------------------

template <typename T>
using EntryType_t = typename std::decay_t<T>::EntryType;

template <typename T>
using EntryValueType_t = typename std::decay_t<T>::ValueType;

//-----------------------------------------------------------------------------

/// Implicit EntryDefs are the usual way how to use "reflections" to model data structures.
/// However, given that in ECMWF MARS and GRIB keys are are often used key by key, there is a need
/// To make the definitions usable first, and then group them.
/// The implicit definition is something more naturally as it defines an accessor only for a specific
/// `struct`.
/// Whenever it is possible to define it is strongly encouraged to do so, e.g. for configuration objects.

template <typename T, typename M>
struct PointerToMemberAccessor {
    M T::*member;

    template <typename U>
    decltype(auto) operator()(U&& obj) const {
        // U can be T&, const T&, or T&&
        return &(std::forward<U>(obj).*member);
    }
};

template <typename T, typename M>
constexpr auto entryDef(std::string_view key, M T::*member) {
    static_assert(IsEntry_v<M>);
    using ValueType = typename M::ValueType;
    using Mapper = typename M::Mapper;

    return EntryDef<ValueType, Mapper>{key}.withAccessor(PointerToMemberAccessor<T, M>{member});
}


//-----------------------------------------------------------------------------

// NOTE: This may be removed once the metadata is cleaned up again. We probably will use nested metadata instead of
// having prefixes (too cumbersome)
//
// ScopedEntryDef ... written on a cumbersome way to reuse the BaseEntryDef to reduced code generation.
// The accessor is not copyable - hence we reference it to the const version - moreover it should never carry state.
// The base key_ string_view is tried to kept update on a rather complex way.
// But this solution is simpler than introducing virtual functions and creating different EntryDef types
// for their declartion (constexpr or static) and their usage.
//
// The importance is that the ScopedEntryDef is used for the Parser/Dumper without the accessor

template <typename EntryDef_>
struct ScopedEntryDef {
    using This = ScopedEntryDef<EntryDef_>;


    ScopedEntryDef(const std::string& key, const EntryDef_& entryDef) :
        key_{key},
        baseEntryDef_{entryDef.withKey(key_).toBase()},
        accessor_{std::cref(*entryDef.accessor_)},
        defaultFunctor_{std::cref(*entryDef.defaultFunctor_)} {}

    ScopedEntryDef(const This& other) :
        key_{other.key_},
        baseEntryDef_{other.baseEntryDef_},
        accessor_{other.accessor_},
        defaultFunctor_{other.defaultFunctor_} {
        // IMPORTANT ... we use string views in base to allow constexpr. Hence here we have to update the reference.
        baseEntryDef_.key_ = key_;
    }
    ScopedEntryDef(This&& other) :
        key_{std::move(other.key_)},
        baseEntryDef_{std::move(other.baseEntryDef_)},
        accessor_{std::move(other.accessor_)},
        defaultFunctor_{std::move(other.defaultFunctor_)} {
        // IMPORTANT ... we use string views in base to allow constexpr. Hence here we have to update the reference.
        baseEntryDef_.key_ = key_;
    }
    This& operator=(const This& other) {
        key_ = other.key_;
        baseEntryDef_ = other.baseEntryDef_;
        // IMPORTANT
        baseEntryDef_.key_ = key_;
        accessor_ = other.accessor_;
        defaultFunctor_ = other.defaultFunctor_;
        return *this;
    }
    This& operator=(This&& other) {
        key_ = std::move(other.key_);
        baseEntryDef_ = std::move(other.baseEntryDef_);
        // IMPORTANT
        baseEntryDef_.key_ = key_;
        accessor_ = other.accessor_;
        defaultFunctor_ = other.defaultFunctor_;
        return *this;
    }


    const auto& toBase() const { return baseEntryDef_; }


    using KeyType = typename EntryDef_::KeyType;
    using ValueType = typename EntryDef_::ValueType;
    using Mapper = typename EntryDef_::Mapper;

    using ParserDumper = typename EntryDef_::ParserDumper;
    using EntryType = typename EntryDef_::EntryType;
    using Accessor = typename EntryDef_::Accessor;

    static constexpr EntryTag tag = EntryDef_::tag;
    static constexpr bool hasDefaultValueFunctor = EntryDef_::hasDefaultValueFunctor;

    // Delegate accessors
    template <typename Container>
    decltype(auto) get(Container&& cont) const {
        return (accessor_.get())(std::forward<Container>(cont));
    }

    ValueType defaultValue() const noexcept {
        static_assert(hasDefaultValueFunctor, "No default functor given");
        return (*(defaultFunctor_.get()))();
    }

    const std::optional<std::string_view>& description() const noexcept { return baseEntryDef_.description(); }
    std::string keyInfo() const { return baseEntryDef_.keyInfo(); }

    // Customize key
    KeyType key() const noexcept { return key_; }

    void applyDefaults(EntryType& v) const {
        ApplyDefaultValueFunctor<typename EntryDef_::DefaultValueFunctor>{}(v, defaultFunctor_.get());
    }


    // Takes a tuple of KeyValue and verifies that all required keys are set
    void validate(const EntryType& v) const { baseEntryDef_.validate(v); }

    util::PrehashedKey<std::string> key_;
    typename EntryDef_::Base baseEntryDef_;
    std::reference_wrapper<const typename EntryDef_::Accessor> accessor_;
    std::reference_wrapper<const typename EntryDef_::DefaultValueFunctor> defaultFunctor_;
};


template <typename EntryDef_, std::enable_if_t<IsEntryDefinition_v<EntryDef_>, bool> = true>
ScopedEntryDef<EntryDef_> scopedEntryDef(const EntryDef_& entryDef, const std::string& scopedKey) {
    return ScopedEntryDef<EntryDef_>(scopedKey, entryDef);
}


// template <typename EntryDef_>
// struct IsBaseEntryDefinition<ScopedBaseEntryDef<EntryDef_>> : std::true_type {};
template <typename EntryDef_>
struct IsEntryDefinition<ScopedEntryDef<EntryDef_>> : std::true_type {};

//-----------------------------------------------------------------------------

}  // namespace multio::datamod

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
#include "multio/datamod/ReaderWriter.h"


namespace multio::datamod {

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


// Dummy type as default value functor
// indication that no directy default value is assigned to that key
struct NoDefaultFunctor {};

// Base key definition without id specialization
// This definition is ment to be constructed as constexpr and in static const objects without beeing modified.
// It is used to retrieve information like type, customized mappers, default value and a description description.
template <typename ValueType_, typename Mapper_, KVTag tag_, typename DefaultValueFunctor>
struct BaseKeyDef {
    using ValueType = ValueType_;
    using Mapper = Mapper_;
    using This = BaseKeyDef<ValueType_, Mapper_, tag_, DefaultValueFunctor>;

    using ReadWrite = ReaderWriter<ValueType, Mapper>;

    static constexpr KVTag tag = tag_;

    static constexpr bool hasDefaultValueFunctor = !std::is_same_v<DefaultValueFunctor, NoDefaultFunctor>;

    //---------------------------------
    // Accessors
    //---------------------------------

    // To be removed in the future, used as for string replacement
    const std::string_view& key() const noexcept { return key_; }
    ValueType defaultValue() const noexcept {
        static_assert(hasDefaultValueFunctor, "No default functor given");
        return (*defaultFunctor_)();
    }
    const std::optional<std::string_view>& description() const noexcept { return description_; }


    //---------------------------------
    // Static methods
    //---------------------------------

    // Members - all "simple" to be constexpr constructable. Would be more relaxed with C++20, but it's all we need
    std::string_view key_;
    std::optional<DefaultValueFunctor> defaultFunctor_{}; // This is optional to savely allow default initialization
    std::optional<std::string_view> description_{};
};


// KeyDef with ID specialization.
// Build on top of the BaseKeyDef and has modifiers that always create new constexpr types.
// It is used to specify a single key within a list of keys on a short and readable way.
//
// For interaction with containers the `id` is usually stripped away and the BaseKeyDef is passed around
// to avoid to fine-grined type specialization.
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
        return KeyDef<id_, ValueType_, Mapper_, KVTag::Optional, DefaultValueFunctor>{Base::key_, Base::defaultFunctor_,
                                                                                      Base::description_};
    }

    // Make the key-value pair defaulted - meaning it will be set through default functor or alter function and is
    // guaranteed to contain a value after validation
    constexpr auto tagDefaulted() const {
        static_assert(tag_ != KVTag::Defaulted, "Definition is already defaulted");
        return KeyDef<id_, ValueType_, Mapper_, KVTag::Optional, DefaultValueFunctor>{
            Base::key_,
            Base::defaultFunctor_,
            Base::description_,
        };
    }

    // Make the key-value pair defaulted and set a functon that generates a default value
    template <typename NewDefValFtor,
              std::enable_if_t<!std::is_convertible_v<NewDefValFtor, ValueType>
                                   && std::is_convertible_v<std::invoke_result_t<NewDefValFtor>, ValueType>,
                               bool>
              = true>
    constexpr auto withDefault(NewDefValFtor&& ftor) const {
        return KeyDef<id_, ValueType_, Mapper_, KVTag::Defaulted, std::decay_t<NewDefValFtor>>{
            Base::key_, std::forward<NewDefValFtor>(ftor), Base::description_};
    }

    // Make the key-value pair defaulted and set a default value (need to be constexpr literal type, or wrap generation
    // of ValueType in a lambda otherwise)
    template <typename Val_, std::enable_if_t<std::is_convertible_v<Val_, ValueType>, bool> = true>
    constexpr auto withDefault(Val_ v) const {
        return withDefault([v = std::move(v)]() { return v; });
    }
    // Sets the description of the value
    constexpr auto withDescription(std::string_view descr) const {
        return This{Base::key_, std::move(Base::defaultFunctor_), descr};
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

}  // namespace multio::datamod

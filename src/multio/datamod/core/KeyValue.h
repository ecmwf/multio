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
#include <variant>
#include "eckit/utils/Overloaded.h"
#include "multio/datamod/DataModellingException.h"
#include "multio/datamod/ReaderWriter.h"

#include "multio/datamod/core/Key.h"
#include "multio/datamod/core/KeyDef.h"
#include "multio/datamod/core/KeySet.h"
#include "multio/datamod/core/KeySetDef.h"

#include "multio/util/TypeTraits.h"


namespace multio::datamod {

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


// BaseKeyValue stores type and mapping information without `id` specialization.
// A BaseKeyValue can hold:
//  * MissingValue
//  * ValueType
//  * reference_wrapper<ValueType>
//
// Keys that are strictly required will go throug a validation step and throw if they are not present.
// Reference are implemented to efficiently pass around arrays - or to use the mechanism to just create
// a view on a data container.
//
// Directy access to the value is given with `get()` or `modify()`.
// A `modify()` call will always make sure that a `ValueType` is contained - if a ref is conatined, a copy is involved
// if necesessary.
//
// Setting values is performed via `set(...)` - it will use the `ReaderWriter` to perform all allowed and necessary
// convertions.
//
// `acquire(...)` can be called to make sure no reference is contained and values are copied if necessary.
template <typename ValueType_, typename Mapper_>
struct BaseKeyValue {
    using ValueType = ValueType_;
    using Mapper = Mapper_;
    using ReadWrite = ReaderWriter<ValueType, Mapper>;
    using This = BaseKeyValue<ValueType, Mapper>;

    using RefType = std::reference_wrapper<const ValueType>;
    using Container = std::variant<MissingValue, ValueType, RefType>;

    // Actual value
    Container value;

    // Raw accessor
    Container raw() && { return std::move(value); }
    Container& raw() & { return value; }
    const Container& raw() const& { return value; }

    bool isMissing() const { return std::holds_alternative<MissingValue>(value); }
    bool has() const { return !std::holds_alternative<MissingValue>(value); }
    bool holdsReference() const { return std::holds_alternative<RefType>(value); }

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

    // Explicitly setting value as reference
    template <typename V, std::enable_if_t<(std::is_same_v<std::decay_t<V>, ValueType>), bool> = true>
    void setRef(const V& v) {
        value = std::cref(v);
    }

    template <typename V, std::enable_if_t<(std::is_same_v<std::decay_t<V>, ValueType>), bool> = true>
    void set(V&& v) {
        value = std::forward<V>(v);
    }
    template <
        typename V,
        std::enable_if_t<(!std::is_same_v<std::decay_t<V>, MissingValue> && !std::is_same_v<std::decay_t<V>, RefType>
                          && !std::is_same_v<std::decay_t<V>, ValueType> && !std::is_same_v<std::decay_t<V>, Container>
                          && !std::is_same_v<std::decay_t<V>, This>),
                         bool>
        = true>
    void set(V&& v) {
        value = ReadWrite::read(std::forward<V>(v));
    }
    template <typename V, std::enable_if_t<(std::is_same_v<std::decay_t<V>, MissingValue>
                                            || std::is_same_v<std::decay_t<V>, RefType>
                                            || std::is_same_v<std::decay_t<V>, Container>),
                                           bool>
                          = true>
    void set(V&& v) {
        value = std::forward<V>(v);
    }
    template <typename V, std::enable_if_t<(std::is_same_v<std::decay_t<V>, This>), bool> = true>
    void set(V&& v) {
        *this = std::forward<V>(v);
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
    // TODO make this call recursive for key value sets
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
    using ReadWrite = ReaderWriter<ValueType, Mapper>;

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


template <typename T>
struct IsAnyKeyValue {
    static constexpr bool value = IsKeyValue_v<T> || IsBaseKeyValue_v<T>;
};
template <typename T>
inline constexpr bool IsAnyKeyValue_v = IsAnyKeyValue<T>::value;



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


}  // namespace multio::datamod


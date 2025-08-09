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

#include <type_traits>
#include <variant>
#include "eckit/utils/Overloaded.h"
#include "multio/datamod/core/DataModellingException.h"
#include "multio/datamod/core/TypeParserDumper.h"

#include "multio/datamod/core/Record.h"
#include "multio/util/TypeTraits.h"


namespace multio::datamod {

//-----------------------------------------------------------------------------
// Value containers
//-----------------------------------------------------------------------------

struct UnsetType {};

// Operators for missing value ... use SFINAE to let this code remain header only
// Also implement operator for other types to simplify comparison implementation for Entry
template <typename MV1, typename MV2,
          std::enable_if_t<(std::is_same_v<MV1, UnsetType> || std::is_same_v<MV2, UnsetType>), bool> = true>
bool operator==(const MV1& lhs, const MV2& rhs) noexcept {
    return std::is_same_v<MV1, UnsetType> && std::is_same_v<MV2, UnsetType>;
}

template <typename MV1, typename MV2,
          std::enable_if_t<(std::is_same_v<MV1, UnsetType> || std::is_same_v<MV2, UnsetType>), bool> = true>
bool operator!=(const MV1& lhs, const MV2& rhs) noexcept {
    return !(lhs == rhs);
}


// BaseEntry stores type and mapping information without `id` specialization.
// A BaseEntry can hold:
//  * UnsetType
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
// Setting values is performed via `set(...)` - it will use the `TypeParserDumper` to perform all allowed and necessary
// convertions.
//
// `acquire(...)` can be called to make sure no reference is contained and values are copied if necessary.
template <typename ValueType_, typename Mapper_>
struct Entry {
    using ValueType = ValueType_;
    using Mapper = Mapper_;
    using ParserDumper = TypeParserDumper<ValueType, Mapper>;
    using This = Entry<ValueType, Mapper>;

    using RefType = std::reference_wrapper<const ValueType>;
    using Container = std::variant<UnsetType, ValueType, RefType>;

    // Actual value
    Container value;

    // Raw accessor
    Container raw() && { return std::move(value); }
    Container& raw() & { return value; }
    const Container& raw() const& { return value; }

    bool isUnset() const { return std::holds_alternative<UnsetType>(value); }
    bool has() const { return !std::holds_alternative<UnsetType>(value); }
    bool holdsReference() const { return std::holds_alternative<RefType>(value); }

    // Function to get the contained value if it's not missing - due to the possibility of containing a reference,
    // only const& versions can get Optimized rvalue handling is achieved through visit
    const ValueType& get() const {
        return std::visit(eckit::Overloaded{
                              [&](const ValueType& val) -> const ValueType& { return val; },
                              [&](const RefType& val) -> const ValueType& { return val.get(); },
                              [&](const UnsetType&) -> const ValueType& {
                                  throw DataModellingException(
                                      std::string("Unchecked call to `BaseEntry::get()` (missing value). Seems like an "
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
                [&](const UnsetType&) -> ValueType& {
                    throw DataModellingException(
                        std::string("Unchecked call to `BaseEntry::modify()` (missing value). Seems like an "
                                    "unvalidated object has been accessed?"),
                        Here());
                },
            },
            value);
    }
    operator ValueType&() { return modify(); }

    void unset() noexcept { value = UnsetType{}; }

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
        std::enable_if_t<(!std::is_same_v<std::decay_t<V>, UnsetType> && !std::is_same_v<std::decay_t<V>, RefType>
                          && !std::is_same_v<std::decay_t<V>, ValueType> && !std::is_same_v<std::decay_t<V>, Container>
                          && !std::is_same_v<std::decay_t<V>, This>),
                         bool>
        = true>
    void set(V&& v) {
        value = ParserDumper::parse(std::forward<V>(v));
    }
    template <typename V,
              std::enable_if_t<(std::is_same_v<std::decay_t<V>, UnsetType> || std::is_same_v<std::decay_t<V>, RefType>
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

    template <typename V, std::enable_if_t<(util::IsOptional_v<std::decay_t<V>>), bool> = true>
    void set(V&& v) {
        if (v) {
            set(std::forward<V>(v).value());
        }
        else {
            unset();
        }
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

        // Check for nested validation
        if constexpr (IsRecord_v<ValueType>) {
            if (has()) {
                acquireRecord(modify());
            }
        }
    }

    template <typename Func,
              std::enable_if_t<ParserDumper::template CanCreateFromValue_v<decltype(std::declval<Func>()())>, bool>
              = true>
    This& ensureInit(Func&& func) {
        if (isUnset()) {
            this->set(std::forward<Func>(func)());

            if constexpr (IsRecord_v<ValueType>) {
                applyRecordDefaults(modify());
            }
        }
        return *this;
    }
    template <typename Val,
              std::enable_if_t<ParserDumper::template CanCreateFromValue_v<decltype(std::declval<Val>())>, bool> = true>
    This& ensureInit(Val&& val) {
        if (isUnset()) {
            this->set(std::forward<Val>(val)());
        }
        return *this;
    }

    This& ensureInit() {
        if (isUnset()) {
            this->set(ValueType{});

            if constexpr (IsRecord_v<ValueType>) {
                applyRecordDefaults(modify());
            }
        }
        return *this;
    }
};


template <typename ValueType, typename Mapper>
bool operator==(const Entry<ValueType, Mapper>& lhs, const Entry<ValueType, Mapper>& rhs) noexcept {
    return lhs.visit(
        [&](const auto& lhsVal) { return rhs.visit([&](const auto& rhsVal) { return lhsVal == rhsVal; }); });
}
template <typename ValueType, typename Mapper>
bool operator!=(const Entry<ValueType, Mapper>& lhs, const Entry<ValueType, Mapper>& rhs) noexcept {
    return lhs.visit(
        [&](const auto& lhsVal) { return rhs.visit([&](const auto& rhsVal) { return lhsVal != rhsVal; }); });
}


//-----------------------------------------------------------------------------


template <typename T>
struct IsEntry : std::false_type {};
template <typename Val, typename Mapper>
struct IsEntry<Entry<Val, Mapper>> : std::true_type {};

template <typename T>
inline constexpr bool IsEntry_v = IsEntry<T>::value;


}  // namespace multio::datamod


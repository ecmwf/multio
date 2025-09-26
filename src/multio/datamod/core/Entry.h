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


/// \defgroup datamod_core_entry Entry
/// \ingroup datamod_core
/// An Entry is a container that stores one value of a given type. It allows the value to be missing or be referenced.
/// A Entry can hold the low level types:
///  * UnsetType
///  * ValueType
///  * reference_wrapper<ValueType>
///
/// In addition to the `ValueType` a `Mapper` can be specified. This allows customized parsing through the
/// `TypeParserDumper` mechanism, either through the `Mapper` inself or specialization of `ParseType<ValueType>`.
///
/// This Type is ment to be described through an `EntryDef` and is ment to be used used in a "Record".
/// Multiple Entries will form a record.
///
/// Accessors:
///   * `get()`: returns a const ref to the contained value
///   * `modify()`: returns a ref to the contained value. Will always make sure that a `ValueType` is contained - if a
///   ref is conatined, a copy is involved if necesessary.
///   * `raw()`: returns the contained variant container
///   * `visit(Func&&)`: Allows visiting the value with an overload set: `UnseTtype`, `ValueType&`, `const ValueType&`,
///   `ValueType&&`.
///                      A reference_wrapper is forwarded as `const ValueType&`
///
/// Setters:
///   * `unset()`: clear the entry
///   * `set(UnsetType{})`: clear the entry
///   * `set(reference_wrapper<ValueType>)`: sets a reference
///   * `setRef(const ValueType&)`: sets a reference
///   * `set(Value)`: Set with arbitraty value - will convert or use `TypeParserDumper<ValueType, Mapper>::parse` if the
///   value can be parsed.
///
/// Query functions:
///   * `isSet()`: Check if no value is contained
///   * `holdsReference()`: Check if a reference is contained
///
/// Modifiers:
///   * `acquire(...)`: Make sure no reference is contained. Values are copied if necessary.
///   * `ensureInit()`: Will default initialize to `ValueType{}` if no value is set
///   * `ensureInit(Val)`: Will default initialize to the passed value if no value is set
///   * `ensureInit(Func&&)`: Will default initialize to the result of the function if no value is set
/// \cond
template <typename ValueType_, typename Mapper_ = DefaultMapper>
struct Entry {
    using ValueType = ValueType_;
    using Mapper = Mapper_;
    using ParserDumper = TypeParserDumper<ValueType, Mapper>;
    using This = Entry<ValueType, Mapper>;

    using RefType = std::reference_wrapper<const ValueType>;
    using Container = std::variant<UnsetType, ValueType, RefType>;

    /// Actual value
    Container value;

    /// Raw accessor
    Container raw() && { return std::move(value); }
    Container& raw() & { return value; }
    const Container& raw() const& { return value; }

    bool isSet() const { return !std::holds_alternative<UnsetType>(value); }
    bool holdsReference() const { return std::holds_alternative<RefType>(value); }

    /// Function to get the contained value if it's not missing - due to the possibility of containing a reference,
    /// only const& versions can get Optimized rvalue handling is achieved through visit
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

    /// Performs an copy-on-write
    /// From a semantics point of view. This function may look confusing.
    /// It is similar to a `get()` but allows modification on the contained value.
    /// This is controversial because all the `set()` methods do the same. However
    /// for cases where large/composed objects are accessed, this kind of access is more natural.
    /// Example:
    ///  * ValueType is another `struct` or nested record (which is used to represent configurations with nesting):
    ///    Single members or even methods on ValueType can be accessed and modified. E.g.:
    ///      - `marsRecord.time.modify().seconds = 0`
    ///      - `productConfig.modify().productCategory.timeExtent = TimeExtent::PointInTime`
    ///  * To move out objects properly
    ///      - `std::move(misc.pv.modify())`: This is the only way to tell "the `misc` object is not needed anymore, if
    ///      a reference is contained **copy** - otherwise **move**.
    ValueType& modify() {
        return std::visit(
            eckit::Overloaded{
                [&](ValueType& val) -> ValueType& { return val; },
                [&](RefType& val) -> ValueType& {
                    // If a reference is hold, the value is copied
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

    void unset() noexcept { value = UnsetType{}; }


    template <typename V>
    inline static constexpr bool CanSetValue_v
        = std::is_same_v<std::decay_t<V>, ValueType> || std::is_same_v<std::decay_t<V>, UnsetType>
       || std::is_same_v<std::decay_t<V>, RefType> || std::is_same_v<std::decay_t<V>, Container>
       || std::is_same_v<std::decay_t<V>, This> || util::IsOptional_v<std::decay_t<V>>
       || ParserDumper::template CanCreateFromValue_v<V>;  //< Check if a value can be sot for template type V

    // Explicitly setting value as reference
    template <typename V, std::enable_if_t<(std::is_same_v<std::decay_t<V>, ValueType>), bool> = true>
    void setRef(const V& v) {
        value = std::cref(v);
    }

    // Set by moving rvalue ref
    template <typename V, std::enable_if_t<(std::is_same_v<std::decay_t<V>, ValueType>), bool> = true>
    void set(V&& v) {
        value = std::forward<V>(v);
    }

    // Try to set everything that is not UnsetType, RefType, ValueType, ContainerType or an Entry (This)
    template <
        typename V,
        std::enable_if_t<(!std::is_same_v<std::decay_t<V>, UnsetType> && !std::is_same_v<std::decay_t<V>, RefType>
                          && !std::is_same_v<std::decay_t<V>, ValueType> && !std::is_same_v<std::decay_t<V>, Container>
                          && !std::is_same_v<std::decay_t<V>, This> && ParserDumper::template CanCreateFromValue_v<V>),
                         bool>
        = true>
    void set(V&& v) {
        value = ParserDumper::parse(std::forward<V>(v));
    }

    // Set everything UnsetType, RefType or ContainerType
    template <typename V,
              std::enable_if_t<(std::is_same_v<std::decay_t<V>, UnsetType> || std::is_same_v<std::decay_t<V>, RefType>
                                || std::is_same_v<std::decay_t<V>, Container>),
                               bool>
              = true>
    void set(V&& v) {
        value = std::forward<V>(v);
    }

    // Set from another Entry
    template <typename V, std::enable_if_t<(std::is_same_v<std::decay_t<V>, This>), bool> = true>
    void set(V&& v) {
        *this = std::forward<V>(v);
    }

    // Set from an optional - converting std::nullopt to UnsetType
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
            if (isSet()) {
                acquireRecord(modify());
            }
        }
    }

    template <typename Func,
              std::enable_if_t<ParserDumper::template CanCreateFromValue_v<decltype(std::declval<Func>()())>, bool>
              = true>
    This& ensureInit(Func&& func) {
        if (!isSet()) {
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
        if (!isSet()) {
            this->set(std::forward<Val>(val)());
        }
        return *this;
    }

    This& ensureInit() {
        if (!isSet()) {
            this->set(ValueType{});

            if constexpr (IsRecord_v<ValueType>) {
                applyRecordDefaults(modify());
            }
        }
        return *this;
    }
};
/// \endcond

}  // namespace multio::datamod


namespace std {

template <typename ValueType, typename Mapper>
struct equal_to<multio::datamod::Entry<ValueType, Mapper>> {
    bool operator()(const multio::datamod::Entry<ValueType, Mapper>& lhs,
                    const multio::datamod::Entry<ValueType, Mapper>& rhs) const {
        return lhs.visit(
            [&](const auto& lhsVal) { return rhs.visit([&](const auto& rhsVal) { return lhsVal == rhsVal; }); });
    }
};

template <typename ValueType, typename Mapper>
struct not_equal_to<multio::datamod::Entry<ValueType, Mapper>> {
    bool operator()(const multio::datamod::Entry<ValueType, Mapper>& lhs,
                    const multio::datamod::Entry<ValueType, Mapper>& rhs) const {
        return lhs.visit(
            [&](const auto& lhsVal) { return rhs.visit([&](const auto& rhsVal) { return lhsVal != rhsVal; }); });
    }
};


}  // namespace std

namespace multio::datamod {

template <typename ValueType, typename Mapper>
bool operator==(const Entry<ValueType, Mapper>& lhs, const Entry<ValueType, Mapper>& rhs) {
    return std::equal_to<Entry<ValueType, Mapper>>{}(lhs, rhs);
}
template <typename ValueType, typename Mapper>
bool operator!=(const Entry<ValueType, Mapper>& lhs, const Entry<ValueType, Mapper>& rhs) {
    return std::not_equal_to<Entry<ValueType, Mapper>>{}(lhs, rhs);
}


//-----------------------------------------------------------------------------


template <typename T>
struct IsEntry : std::false_type {};
template <typename Val, typename Mapper>
struct IsEntry<Entry<Val, Mapper>> : std::true_type {};

template <typename T>
inline constexpr bool IsEntry_v = IsEntry<T>::value;

/// C++20 Concept
// template <typename T>
// concept EntryType = IsEntry<std::remove_cvref_t<T>>::value;



}  // namespace multio::datamod


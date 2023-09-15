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

/// @date Sept 2023

#pragma once

#include "multio/message/MetadataException.h"
#include "multio/message/MetadataTypes.h"
#include "multio/util/VariantHelpers.h"

#include "eckit/log/JSON.h"
#include "eckit/value/Value.h"


namespace multio::message {

//----------------------------------------------------------------------------------------------------------------------


using MetadataValueVariant = util::ApplyTypeList_t<std::variant, typename MetadataTypes::AllWrapped>;

class MetadataValue : public MetadataValueVariant {
public:
    using Types = MetadataTypes;

    using This = MetadataValue;
    using Base = MetadataValueVariant;
    // ICPC has not sane conversion - constructor and assignment are explicitly redefined below
    // using MetadataValueVariant::MetadataValueVariant;
    // using Base::operator=;

    MetadataValue(const This& other);
    MetadataValue(This&&) noexcept = default;

    This& operator=(const This& other);

    This& operator=(This&&) noexcept = default;

    // Theres a special handling for unique_ptr - To support nested types it is necessary to wrap a type into a smart
    // pointer or to create a class with explicit memory handling. To avoid explicit memory handling, types wrapped in a
    // unique pointer will be handled transperently through a `get` and `visit` calls on the metadata object. Thus, the
    // fact that a unique_ptr is used is hidden from the user.
    //
    // Static cast to base class is done because gcc had a bug with visiting derived classes:
    // https://gcc.gnu.org/bugzilla/show_bug.cgi?id=90943 Not required for gcc12
    template <typename F>
    decltype(auto) visit(F&& f) const& noexcept(noexcept(util::visitUnwrapUniquePtr(std::forward<F>(f), *this))) {
        return util::visitUnwrapUniquePtr(std::forward<F>(f), *this);
    }

    template <typename F>
    decltype(auto) visit(F&& f) & noexcept(noexcept(util::visitUnwrapUniquePtr(std::forward<F>(f), *this))) {
        return util::visitUnwrapUniquePtr(std::forward<F>(f), *this);
    }

    template <typename F>
    decltype(auto) visit(F&& f) && noexcept(noexcept(util::visitUnwrapUniquePtr(std::forward<F>(f),
                                                                                std::move(*this)))) {
        return util::visitUnwrapUniquePtr(std::forward<F>(f), std::move(*this));
    }


    template <typename T>
    const T& get() const& {
        return getter<T>(*this);
    }

    template <typename T>
    T& get() & {
        return getter<T>(*this);
    }

    template <typename T>
    T&& get() && {
        return std::move(getter<T>(std::move(*this)));
    }


    std::string toString() const;
    void json(eckit::JSON& j) const;


    //----------------------------------------------------------------------------------------------------------------------


    // Implementation details


    // Special constructor to transparently create nested types that are supposed to be wrapped with unique_ptr
    template <
        typename T,
        std::enable_if_t<util::TypeListContains_v<std::unique_ptr<std::decay_t<T>>, typename Types::AllWrapped>, bool>
        = true>
    MetadataValue(T&& val) : MetadataValue(std::make_unique<std::decay_t<T>>(std::forward<T>(val))){};

   
    // Constructor that handles predefined conversions for convenience. Required for smooth icpc compilation
    template <typename T,
              std::enable_if_t<(!util::TypeListContains_v<std::unique_ptr<std::decay_t<T>>, typename Types::AllWrapped>
                                && !std::is_same_v<std::decay_t<T>, This> && !std::is_same_v<std::decay_t<T>, Base>
                                && HasMetadataValueConversionHelper<std::decay_t<T>>::value),
                               bool>
              = true>
    MetadataValue(T&& val) : Base(MetadataValueConversionHelper_t<std::decay_t<T>>(std::forward<T>(val))){};

    // Constructor that deals with all other cases exlusive to the unique_ptr handling - Sane conversion is reimplemented for icpc
    template <typename T,
              std::enable_if_t<(!util::TypeListContains_v<std::unique_ptr<std::decay_t<T>>, typename Types::AllWrapped>
                                && !std::is_same_v<std::decay_t<T>, This> && !std::is_same_v<std::decay_t<T>, Base>
                                && !HasMetadataValueConversionHelper<std::decay_t<T>>::value
                                ),
                               bool>
              = true>
    // MetadataValue(T&& val) : Base(util::FirstContvertibleTo_t<T, typename Types::AllWrapped>(std::forward<T>(val))){};
    MetadataValue(T&& val) : Base(util::SaneOverloadResolutionResult_t<T, typename Types::AllWrapped>{std::forward<T>(val)}){};


    // // The following two constructors are added because intels icpc has problems with inheriting constructors that
    // // conflict with the previous constructor
    // // // using MetadataValueVariant::MetadataValueVariant;
    // template <typename T1, typename T2, typename... TS,
    //           std::enable_if_t<std::is_constructible_v<Base, T1, T2, TS...>, bool> = true>
    // MetadataValue(T1&& v1, T2&& v2, TS&&... vs) :
    //     Base(std::forward<T1>(v1), std::forward<T2>(v2), std::forward<TS>(vs)...){};

    MetadataValue() : Base(){};



    // Overwriting operator= to support transparent unique_ptr creation and implement sane conversion for Intel icpc
    template <
        typename T,
        std::enable_if_t<util::TypeListContains_v<std::unique_ptr<std::decay_t<T>>, typename Types::AllWrapped>, bool>
        = true>
    This& operator=(T&& val) { Base::operator=(std::make_unique<std::decay_t<T>>(std::forward<T>(val))); return *this; }

    // Constructor that deals with all other cases exlusive to the unique_ptr handling
    template <typename T,
              std::enable_if_t<(!util::TypeListContains_v<std::unique_ptr<std::decay_t<T>>, typename Types::AllWrapped>
                                && !std::is_same_v<std::decay_t<T>, This> && !std::is_same_v<std::decay_t<T>, Base>
                                && HasMetadataValueConversionHelper<std::decay_t<T>>::value),
                               bool>
              = true>
    This& operator=(T&& val) { 
        using TI = MetadataValueConversionHelper_t<std::decay_t<T>>;
        if (this->index() == util::GetVariantIndex_v<TI, Base>) {
            std::get<TI>(*this) = std::forward<T>(val);
        } else {
            Base::operator=(TI{std::forward<T>(val)}); 
        }
        return *this; 
    }

    // Constructor that is required for icpc....
    template <typename T,
              std::enable_if_t<(!util::TypeListContains_v<std::unique_ptr<std::decay_t<T>>, typename Types::AllWrapped>
                                && !std::is_same_v<std::decay_t<T>, This> && !std::is_same_v<std::decay_t<T>, Base>
                                && !HasMetadataValueConversionHelper<std::decay_t<T>>::value
                                // && !std::is_same_v<util::OverloadResolutionResult_t<T, typename Types::AllWrapped>, void>),
                                ),
                               bool>
              = true>
    // MetadataValue(T&& val) : Base(util::FirstContvertibleTo_t<T, typename Types::AllWrapped>(std::forward<T>(val))){};
    This& operator=(T&& val) { 
        using TI = util::SaneOverloadResolutionResult_t<T, typename Types::AllWrapped>;
        if (this->index() == util::GetVariantIndex_v<TI, Base>) {
            std::get<TI>(*this) = std::forward<T>(val);
        } else {
            Base::operator=(TI{std::forward<T>(val)}); 
        }
        return *this; 
    }

private:
    // Implementation details

    template <
        typename T,
        std::enable_if_t<!util::TypeListContains_v<std::unique_ptr<std::decay_t<T>>, typename Types::AllWrapped>, bool>
        = true>
    static T&& wrapNestedMaybe(T&& v) noexcept {
        return std::forward<T>(v);
    }

    template <
        typename T,
        std::enable_if_t<util::TypeListContains_v<std::unique_ptr<std::decay_t<T>>, typename Types::AllWrapped>, bool>
        = true>
    static std::unique_ptr<std::decay_t<T>> wrapNestedMaybe(T&& v) {
        return std::make_unique<std::decay_t<T>>(std::forward<T>(v));
    }


    //


    template <typename T, typename This_>
    static decltype(auto) resolvedUniquePtrGetter(This_&& val) {
        static_assert(util::TypeListContains_v<std::decay_t<T>, typename Types::AllWrapped>);
        if (val.index() == util::GetVariantIndex_v<std::decay_t<T>, Base>) {
            return std::get<T>(std::forward<This_>(val));
        }
        throw MetadataWrongTypeException(util::GetVariantIndex_v<std::decay_t<T>, Base>, val.index(), Here());
    }

    template <typename T, typename This_,
              std::enable_if_t<util::TypeListContains_v<std::decay_t<T>, typename Types::AllWrapped>, bool> = true>
    static decltype(auto) uniquePtrGetter(This_&& val) {
        return resolvedUniquePtrGetter<T>(std::forward<This_>(val));
    }

    template <
        typename T, typename This_,
        std::enable_if_t<util::TypeListContains_v<std::unique_ptr<std::decay_t<T>>, typename Types::AllWrapped>, bool>
        = true>
    static decltype(auto) uniquePtrGetter(This_&& val) {
        if constexpr (!std::is_lvalue_reference_v<This_>) {
            return std::move(*(resolvedUniquePtrGetter<std::unique_ptr<T>>(std::forward<This_>(val))).get());
        }
        else {
            return *(resolvedUniquePtrGetter<std::unique_ptr<T>>(std::forward<This_>(val))).get();
        }
    }

    template <typename T, typename This_>
    static decltype(auto) getter(This_&& val) {
        return uniquePtrGetter<T>(std::forward<This_>(val));
    }
};


}  // namespace multio::message

//----------------------------------------------------------------------------------------------------------------------


namespace std {
template <>
struct variant_size<multio::message::MetadataValue> : variant_size<multio::message::MetadataValueVariant> {};

template <std::size_t I>
struct variant_alternative<I, multio::message::MetadataValue>
    : variant_alternative<I, multio::message::MetadataValueVariant> {};
}  // namespace std

namespace multio::util {

template <typename T>
struct GetVariantIndex<T, multio::message::MetadataValue> : GetVariantIndex<T, multio::message::MetadataValueVariant> {
};

template <>
struct GetVariantIndex<multio::message::Metadata, multio::message::MetadataValueVariant>
    : GetVariantIndex<std::unique_ptr<multio::message::Metadata>, multio::message::MetadataValueVariant> {};

}  // namespace multio::util


//----------------------------------------------------------------------------------------------------------------------


namespace multio::message {

//----------------------------------------------------------------------------------------------------------------------

eckit::JSON& operator<<(eckit::JSON& json, const MetadataValue& mv);

std::ostream& operator<<(std::ostream& os, const MetadataValue& metadataValue);

std::optional<MetadataValue> tryToMetadataValue(const eckit::Value& v);


//----------------------------------------------------------------------------------------------------------------------


}  // namespace multio::message


//----------------------------------------------------------------------------------------------------------------------

template <>
struct std::hash<multio::message::MetadataValue> {
    std::size_t operator()(const multio::message::MetadataValue& t) const;
};

//----------------------------------------------------------------------------------------------------------------------
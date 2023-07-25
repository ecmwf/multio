/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Domokos Sarmany
/// @author Simon Smart
/// @author Tiago Quintino

/// @date Oct 2019

#pragma once

#include "eckit/config/Configuration.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/utils/Translator.h"
#include "eckit/value/Value.h"

#include "multio/util/VariantHelpers.h"

#include <cstdint>
#include <memory>
#include <unordered_map>
#include <vector>


namespace multio::message {

//-----------------------------------------------------------------------------

class MetadataException : public eckit::Exception {
public:
    MetadataException(const std::string& reason, const eckit::CodeLocation& l = eckit::CodeLocation());
};

class MetadataKeyException : public MetadataException {
public:
    MetadataKeyException(const std::string& key, const std::string& more,
                         const eckit::CodeLocation& l = eckit::CodeLocation());
};

class MetadataMissingKeyException : public MetadataKeyException {
public:
    MetadataMissingKeyException(const std::string& missingKey, const eckit::CodeLocation& l = eckit::CodeLocation());
};

class MetadataWrongTypeException : public MetadataException {
public:
    MetadataWrongTypeException(const std::string& key, const eckit::CodeLocation& l = eckit::CodeLocation());
    MetadataWrongTypeException(std::size_t requestedIndex, std::size_t containedIndex,
                               const eckit::CodeLocation& l = eckit::CodeLocation());
    MetadataWrongTypeException(const eckit::CodeLocation& l = eckit::CodeLocation());
};


//-----------------------------------------------------------------------------


// Forward declaration
class Metadata;

using MetadataIntegerTypes = util::TypeList<bool, std::int8_t, std::int16_t, std::int32_t, std::int64_t>;
using MetadataFloatingTypes = util::TypeList<double, float>;
using MetadataStringTypes = util::TypeList<std::string>;
using MetadataScalarTypes = util::MergeTypeList_t<MetadataIntegerTypes, MetadataFloatingTypes, MetadataStringTypes>;

using MetadataIntegerVectorTypes = util::MapTypeList_t<std::vector, MetadataIntegerTypes>;
using MetadataFloatingVectorTypes = util::MapTypeList_t<std::vector, MetadataFloatingTypes>;
using MetadataStringVectorTypes = util::MapTypeList_t<std::vector, MetadataStringTypes>;
using MetadataVectorTypes
    = util::MergeTypeList_t<MetadataIntegerVectorTypes, MetadataFloatingVectorTypes, MetadataStringVectorTypes>;

using MetadataNestedTypes = util::TypeList<Metadata>;
using MetadataWrappedNestedTypes = util::MapTypeList_t<std::unique_ptr, MetadataNestedTypes>;

using MetadataTypes = util::MergeTypeList_t<MetadataScalarTypes, MetadataVectorTypes, MetadataWrappedNestedTypes>;

//-----------------------------------------------------------------------------

using MetadataValueVariant = util::ApplyTypeList_t<std::variant, MetadataTypes>;

class MetadataValue : public MetadataValueVariant {
public:
    using This = MetadataValue;
    using Base = MetadataValueVariant;
    using MetadataValueVariant::MetadataValueVariant;
    using Base::operator=;

    MetadataValue(const This&);
    MetadataValue(This&&) noexcept = default;

    This& operator=(const This&);
    This& operator=(This&&) noexcept = default;

    template <typename F>
    decltype(auto) visit(F&& f) const& noexcept(noexcept(std::visit(util::forwardUnwrappedUniquePtr(std::forward<F>(f)),
                                                                    *this))) {
        return std::visit(util::forwardUnwrappedUniquePtr(std::forward<F>(f)), *this);
    }

    template <typename F>
    decltype(auto) visit(F&& f) & noexcept(noexcept(std::visit(util::forwardUnwrappedUniquePtr(std::forward<F>(f)),
                                                               *this))) {
        return std::visit(util::forwardUnwrappedUniquePtr(std::forward<F>(f)), *this);
    }

    template <typename F>
    decltype(auto) visit(F&& f) && noexcept(noexcept(std::visit(util::forwardUnwrappedUniquePtr(std::forward<F>(f)),
                                                                std::move(*this)))) {
        return std::visit(util::forwardUnwrappedUniquePtr(std::forward<F>(f)), std::move(*this));
    }

    template <typename T>
    const T& get() const& {
        static_assert(util::TypeListContains<std::decay_t<T>, MetadataTypes>::value);
        if (this->index() == util::GetVariantIndex<std::decay_t<T>, MetadataValueVariant>::value) {
            return std::get<T>(*this);
        }
        throw MetadataWrongTypeException(util::GetVariantIndex<std::decay_t<T>, MetadataValueVariant>::value,
                                         this->index(), Here());
    }

    template <typename T>
    T& get() & {
        static_assert(util::TypeListContains<std::decay_t<T>, MetadataTypes>::value);
        if (this->index() == util::GetVariantIndex<std::decay_t<T>, MetadataValueVariant>::value) {
            return std::get<T>(*this);
        }
        throw MetadataWrongTypeException(util::GetVariantIndex<std::decay_t<T>, MetadataValueVariant>::value,
                                         this->index(), Here());
    }

    template <typename T>
    T&& get() && {
        static_assert(util::TypeListContains<std::decay_t<T>, MetadataTypes>::value);
        if (this->index() == util::GetVariantIndex<std::decay_t<T>, MetadataValueVariant>::value) {
            return std::get<T>(std::move(*this));
        }
        throw MetadataWrongTypeException(util::GetVariantIndex<std::decay_t<T>, MetadataValueVariant>::value,
                                         this->index(), Here());
    }

    template <typename T>
    T getTranslate() const {
        return visit(Overloaded{
            [&](const auto& v) -> std::enable_if_t<!eckit::IsTranslatable<std::decay_t<decltype(v)>, T>::value, T> {
                throw MetadataException(std::string("Contained type is not translateable. Index: ")
                                        + std::to_string(this->index()));
            },
            [](const auto& v) -> std::enable_if_t<eckit::IsTranslatable<std::decay_t<decltype(v)>, T>::value, T> {
                return eckit::translate<T>(v);
            }});
    }
};

// Specialized get for Metadata
template <>
const Metadata& MetadataValue::get<Metadata>() const&;

template <>
Metadata& MetadataValue::get<Metadata>() &;

template <>
Metadata&& MetadataValue::get<Metadata>() &&;


std::ostream& operator<<(std::ostream&, const MetadataValue&);

}  // namespace multio::message

namespace std {
template <>
struct variant_size<multio::message::MetadataValue> : variant_size<multio::message::MetadataValueVariant> {};

template <std::size_t I>
struct variant_alternative<I, multio::message::MetadataValue>
    : variant_alternative<I, multio::message::MetadataValueVariant> {};
}  // namespace std

namespace multio::util {

template <typename T>
struct util::GetVariantIndex<T, multio::message::MetadataValue>
    : util::GetVariantIndex<T, multio::message::MetadataValueVariant> {};

}  // namespace multio::util

//-----------------------------------------------------------------------------

namespace multio::message {

class Metadata {
    std::unordered_map<std::string, MetadataValue> values_;

public:
    Metadata();

    Metadata(const Metadata&) = default;
    Metadata(Metadata&&) noexcept = default;

    Metadata(std::initializer_list<std::pair<const std::string, MetadataValue>> li);

    // To be removed in the future
    Metadata(const eckit::Value&);
    Metadata(const eckit::Configuration&);

    Metadata& operator=(const Metadata&) = default;
    Metadata& operator=(Metadata&&) noexcept = default;

    // User-defined conversion to unique_ptr - simplify usage
    operator std::unique_ptr<Metadata>() const&;

    operator std::unique_ptr<Metadata>() &;

    operator std::unique_ptr<Metadata>() &&;


    MetadataValue&& get(const std::string& k) &&;
    MetadataValue& get(const std::string& k) &;
    const MetadataValue& get(const std::string& k) const&;

    template <typename T>
    T&& get(const std::string& k) && {
        if (auto search = values_.find(k); search != values_.end()) {
            try {
                return std::move(search->second.get<T>());
            }
            catch (const MetadataException& err) {
                std::throw_with_nested(MetadataKeyException(k, err.what(), Here()));
            }
        }
        throw MetadataMissingKeyException(k, Here());
    }

    template <typename T>
    T& get(const std::string& k) & {
        if (auto search = values_.find(k); search != values_.end()) {
            try {
                return search->second.get<T>();
            }
            catch (const MetadataException& err) {
                std::throw_with_nested(MetadataKeyException(k, err.what(), Here()));
            }
        }
        throw MetadataMissingKeyException(k, Here());
    }

    template <typename T>
    const T& get(const std::string& k) const& {
        if (auto search = values_.find(k); search != values_.end()) {
            try {
                return search->second.get<T>();
            }
            catch (const MetadataException& err) {
                std::throw_with_nested(MetadataKeyException(k, err.what(), Here()));
            }
        }
        throw MetadataMissingKeyException(k, Here());
    }

    template <typename T>
    std::optional<T> getOpt(const std::string& k) && {
        if (auto search = values_.find(k); search != values_.end()) {
            try {
                return std::move(search->second.get<T>());
            }
            catch (const MetadataException& err) {
                std::throw_with_nested(MetadataKeyException(k, err.what(), Here()));
            }
        }
        return std::nullopt;
    }

    template <typename T>
    std::optional<T> getOpt(const std::string& k) & {
        if (auto search = values_.find(k); search != values_.end()) {
            try {
                return search->second.get<T>();
            }
            catch (const MetadataException& err) {
                std::throw_with_nested(MetadataKeyException(k, err.what(), Here()));
            }
        }
        return std::nullopt;
    }

    template <typename T>
    std::optional<T> getOpt(const std::string& k) const& {
        if (auto search = values_.find(k); search != values_.end()) {
            try {
                return search->second.get<T>();
            }
            catch (const MetadataException& err) {
                std::throw_with_nested(MetadataKeyException(k, err.what(), Here()));
            }
        }
        return std::nullopt;
    }

    template <typename T>
    T&& getTranslate(const std::string& k) && {
        if (auto search = values_.find(k); search != values_.end()) {
            try {
                return std::move(search->second.getTranslate<T>());
            }
            catch (const MetadataException& err) {
                std::throw_with_nested(MetadataKeyException(k, err.what(), Here()));
            }
        }
        throw MetadataMissingKeyException(k, Here());
    }


    template <typename T>
    T getTranslate(const std::string& k) const {
        if (auto search = values_.find(k); search != values_.end()) {
            try {
                return search->second.getTranslate<T>();
            }
            catch (const MetadataException& err) {
                std::throw_with_nested(MetadataKeyException(k, err.what(), Here()));
            }
        }
        throw MetadataMissingKeyException(k, Here());
    }

    template <typename T>
    std::optional<T> getTranslateOpt(const std::string& k) const {
        if (auto search = values_.find(k); search != values_.end()) {
            try {
                return search->second.getTranslate<T>();
            }
            catch (const MetadataException& err) {
                std::throw_with_nested(MetadataKeyException(k, err.what(), Here()));
            }
        }
        return std::nullopt;
    }

    MetadataValue& operator[](const std::string&);
    MetadataValue& operator[](std::string&&);


    template <typename V>
    void set(std::string&& k, V&& v) {
        values_.insert_or_assign(std::move(k), std::forward<V>(v));
    }

    template <typename V>
    void set(const std::string& k, V&& v) {
        values_.insert_or_assign(k, std::forward<V>(v));
    }

    bool has(const std::string& k) const;

    auto find(const std::string& k) { return values_.find(k); };
    auto find(const std::string& k) const { return values_.find(k); };

    auto begin() noexcept { return values_.begin(); };

    auto begin() const noexcept { return values_.begin(); };

    auto cbegin() const noexcept { return values_.cbegin(); };

    auto end() noexcept { return values_.end(); };

    auto end() const noexcept { return values_.end(); };

    auto cend() const noexcept { return values_.cend(); };

    bool empty() const noexcept;

    std::size_t size() const noexcept;

    void clear() noexcept;
};

std::ostream& operator<<(std::ostream&, const Metadata&);

//-----------------------------------------------------------------------------

std::string toString(const Metadata& metadata);
Metadata toMetadata(const std::string& fieldId);
Metadata toMetadata(const eckit::Value& value);

// Helper for interop with eckit::LocalConfiguration
std::optional<MetadataValue> toMetadataValue(const eckit::Value& v);

//-----------------------------------------------------------------------------

}  // namespace multio::message

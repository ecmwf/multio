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
#include "eckit/parser/GenericObjectParser.h"
#include "eckit/utils/Translator.h"
#include "eckit/value/Value.h"

#include "multio/util/VariantHelpers.h"

#include <cstdint>
#include <deque>
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

struct Null {
    constexpr operator bool() { return false; }
};
constexpr bool operator<(Null, Null) {
    return false;
}

using MetadataNullTypes = util::TypeList<Null>;
using MetadataIntegerTypes = util::TypeList<bool, std::int8_t, std::int16_t, std::int32_t, std::int64_t>;
using MetadataFloatingTypes = util::TypeList<double, float>;
using MetadataStringTypes = util::TypeList<std::string>;
using MetadataNonNullScalarTypes
    = util::MergeTypeList_t<MetadataIntegerTypes, MetadataFloatingTypes, MetadataStringTypes>;
using MetadataScalarTypes = util::MergeTypeList_t<MetadataNullTypes, MetadataNonNullScalarTypes>;

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

template <>
struct util::GetVariantIndex<multio::message::Metadata, multio::message::MetadataValueVariant>
    : util::GetVariantIndex<std::unique_ptr<multio::message::Metadata>, multio::message::MetadataValueVariant> {};

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

    template <typename V>
    bool trySet(std::string&& k, V&& v) {
        return values_.try_emplace(std::move(k), std::forward<V>(v)).second;
    }

    template <typename V>
    bool trySet(const std::string& k, V&& v) {
        return values_.try_emplace(k, std::forward<V>(v)).second;
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

    friend struct MetadataValueBuilder;
};

std::ostream& operator<<(std::ostream&, const Metadata&);

//-----------------------------------------------------------------------------

std::string toString(const Metadata& metadata);
Metadata toMetadata(const std::string& fieldId);
Metadata toMetadata(const eckit::Value& value);

// Helper for interop with eckit::LocalConfiguration
std::optional<MetadataValue> toMetadataValue(const eckit::Value& v);

//-----------------------------------------------------------------------------


struct MetadataValueBuilder {
    using ObjectValueType = eckit::parser::ObjectValueType;
    template <ObjectValueType t>
    using ObjectValueTag = eckit::parser::ObjectValueTag<t>;

    static Null from(ObjectValueTag<ObjectValueType::Null>) { return Null{}; };

    static bool from(ObjectValueTag<ObjectValueType::Bool>, bool b) { return b; };

    static std::string from(ObjectValueTag<ObjectValueType::String>, const std::string& s) { return s; };
    static std::string from(ObjectValueTag<ObjectValueType::String>, std::string&& s) { return std::move(s); };
    static std::string from(ObjectValueTag<ObjectValueType::Key>, const std::string& s) { return s; };
    static std::string from(ObjectValueTag<ObjectValueType::Key>, std::string&& s) { return std::move(s); };

    template <typename IntType, std::enable_if_t<std::is_integral<IntType>::value, bool> = true>
    static MetadataValue from(ObjectValueTag<ObjectValueType::Number>, IntType n) {
        return n;
    };

    template <typename FloatType, std::enable_if_t<std::is_floating_point<FloatType>::value, bool> = true>
    static MetadataValue from(ObjectValueTag<ObjectValueType::Number>, FloatType f) {
        return f;
    };


    static Metadata from(ObjectValueTag<ObjectValueType::Map>) { return Metadata{}; };

    static void addToMapIfNotContained(Metadata& m, std::string&& k, MetadataValue&& v) {
        m.trySet(std::move(k), std::move(v));
    }
    static void addToMapIfNotContained(Metadata& m, const std::string& k, const MetadataValue& v) { m.trySet(k, v); }

    template <typename Func>
    static void forEachMap(Metadata&& m, Func&& f) {
        for (auto&& p : std::move(m)) {
            f(std::move(p.first), std::move(p.second));
        }
    }
    template <typename Func>
    static void forEachMap(const Metadata& m, Func&& f) {
        for (auto&& p : std::move(m)) {
            f(p.first, p.second);
        }
    }


    using ValueList = std::deque<MetadataValue>;

    static ValueList from(ObjectValueTag<ObjectValueType::List>) { return ValueList{}; };

    static void addToList(ValueList& l, MetadataValue&& v) { l.push_back(std::move(v)); }
    static void addToList(ValueList& l, const MetadataValue& v) { l.push_back(std::move(v)); }

    template <typename VecType, typename Func>
    static auto forEachList(VecType&& l, Func&& f) -> util::IfTypeOf<VecType, MetadataVectorTypes> {
        for (auto&& v : l) {
            f(std::move(v));
        }
    }
    template <typename VecType, typename Func>
    static auto forEachList(const VecType& l, Func&& f) -> util::IfTypeOf<VecType, MetadataVectorTypes> {
        for (const auto& v : l) {
            f(v);
        }
    }

    template <typename Func>
    static void forEachList(ValueList&& l, Func&& f) {
        for (auto&& v : l) {
            f(std::move(v));
        }
    }
    template <typename Func>
    static void forEachList(const ValueList& l, Func&& f) {
        for (const auto& v : l) {
            f(v);
        }
    }

    template <typename Func>
    static void forEachList(MetadataValue&& l, Func&& f) {
        std::move(l).visit(Overloaded{
            [&](auto&& v) -> util::IfTypeOf<decltype(v), MetadataVectorTypes> {
                for (auto&& vi : std::move(v)) {
                    f(std::move(vi));
                }
            },
            [&f](auto&& v) -> util::IfTypeNotOf<decltype(v), MetadataVectorTypes> {
                throw MetadataException(
                    "MetadataValueBuilder::forEachList(MetadataValue...) called with a value that is no vector");
            },
        });
    }

    template <typename Func>
    static void forEachList(const MetadataValue& l, Func&& f) {
        l.visit(Overloaded{
            [&f](const auto& v) -> util::IfTypeOf<decltype(v), MetadataVectorTypes> {
                for (const auto& vi : std::move(v)) {
                    f(vi);
                }
            },
            [&f](const auto& v) -> util::IfTypeNotOf<decltype(v), MetadataVectorTypes> {
                throw MetadataException(
                    "MetadataValueBuilder::forEachList(MetadataValue...) called with a value that is no vector");
            },
        });
    }

    static MetadataValue toValue(MetadataValue&& v) { return std::move(v); }

    static MetadataValue toValue(ValueList&& l) {
        if (l.empty()) {
            return Null{};
        }
        static const std::string baseErrMsg = "Metadata only supports lists of same type with non-null scalar types";
        return l.at(0).visit(Overloaded{
            [&l](const auto& v) -> util::IfTypeOf<decltype(v), MetadataNonNullScalarTypes, MetadataValue> {
                using ScalarType = std::decay_t<decltype(v)>;
                std::vector<std::decay_t<decltype(v)>> newVec;

                unsigned int i = 0;
                for (auto&& vi : std::move(l)) {
                    try {
                        newVec.push_back(std::move(vi).get<ScalarType>());
                    }
                    catch (const MetadataWrongTypeException) {
                        std::ostringstream oss;
                        oss << baseErrMsg << ". Value " << i << ": " << vi
                            << " has a different type than the previous values [";
                        bool isFirst = true;
                        for (const auto& vi2 : newVec) {
                            if (!isFirst) {
                                oss << ", ";
                            }
                            oss << (ScalarType)vi2;
                            isFirst = false;
                        }
                        oss << "]";
                        std::throw_with_nested(MetadataException(oss.str()));
                    }
                    ++i;
                }

                return newVec;
            },
            [&l](const auto& v) -> util::IfTypeNotOf<decltype(v), MetadataNonNullScalarTypes, MetadataValue> {
                std::ostringstream oss;
                oss << baseErrMsg << ". Can not convert value list: [";
                bool isFirst = true;
                for (const auto& vi : l) {
                    if (!isFirst) {
                        oss << ", ";
                    }
                    oss << vi;
                    isFirst = false;
                }
                oss << "]";
                throw MetadataException(oss.str());
            },
        });
    }

    template <typename T>
    static MetadataValue toValue(T&& t) {
        return MetadataValue(std::forward<T>(t));
    }

    template <typename MV, typename Func,
              std::enable_if_t<std::is_same_v<std::decay_t<MV>, MetadataValue>, bool> = true>
    static decltype(auto) visit(const MV& v, Func&& f) {
        using RetValue
            = decltype(std::forward<Func>(f)(ObjectValueTag<ObjectValueType::Null>{}, std::declval<const Null&>()));
        return v.visit(Overloaded{[&f](const Metadata& m) -> RetValue {
                                      return std::forward<Func>(f)(ObjectValueTag<ObjectValueType::Map>{}, m);
                                  },
                                  [&f](const Null& n) -> RetValue {
                                      return std::forward<Func>(f)(ObjectValueTag<ObjectValueType::Null>{}, n);
                                  },
                                  [&f](const bool& b) -> RetValue {
                                      return std::forward<Func>(f)(ObjectValueTag<ObjectValueType::Bool>{}, b);
                                  },
                                  [&f](const auto& v) -> util::IfTypeOf<decltype(v), MetadataIntegerTypes, RetValue> {
                                      return std::forward<Func>(f)(ObjectValueTag<ObjectValueType::Number>{}, v);
                                  },
                                  [&f](const auto& v) -> util::IfTypeOf<decltype(v), MetadataFloatingTypes, RetValue> {
                                      return std::forward<Func>(f)(ObjectValueTag<ObjectValueType::Number>{}, v);
                                  },
                                  [&f](const auto& v) -> util::IfTypeOf<decltype(v), MetadataStringTypes, RetValue> {
                                      return std::forward<Func>(f)(ObjectValueTag<ObjectValueType::String>{}, v);
                                  },
                                  [&f](const auto& v) -> util::IfTypeOf<decltype(v), MetadataVectorTypes, RetValue> {
                                      // Not directely a ValueList as specified as list type, propagated value must just
                                      // fit with forEachList
                                      return std::forward<Func>(f)(ObjectValueTag<ObjectValueType::List>{}, v);
                                  }});
    }

    // Using SFINAE and std::is_same avoids implicit conversion of scalar types to metadata value and produces more
    // detailed results
    template <typename MV, typename Func,
              std::enable_if_t<std::is_same_v<std::decay_t<MV>, MetadataValue> && std::is_rvalue_reference_v<MV>, bool>
              = true>
    static decltype(auto) visit(MV&& v, Func&& f) {
        using RetValue
            = decltype(std::forward<Func>(f)(ObjectValueTag<ObjectValueType::Null>{}, std::declval<Null&&>()));
        return std::move(v).visit(
            Overloaded{[&f](Metadata&& m) -> RetValue {
                           return std::forward<Func>(f)(ObjectValueTag<ObjectValueType::Map>{}, std::move(m));
                       },
                       [&f](Null&& n) -> RetValue {
                           return std::forward<Func>(f)(ObjectValueTag<ObjectValueType::Null>{}, std::move(n));
                       },
                       [&f](bool&& b) -> RetValue {
                           return std::forward<Func>(f)(ObjectValueTag<ObjectValueType::Bool>{}, std::move(b));
                       },
                       [&f](auto&& v) -> util::IfTypeOf<decltype(v), MetadataIntegerTypes, RetValue> {
                           return std::forward<Func>(f)(ObjectValueTag<ObjectValueType::Number>{}, std::move(v));
                       },
                       [&f](auto&& v) -> util::IfTypeOf<decltype(v), MetadataFloatingTypes, RetValue> {
                           return std::forward<Func>(f)(ObjectValueTag<ObjectValueType::Number>{}, std::move(v));
                       },
                       [&f](auto&& v) -> util::IfTypeOf<decltype(v), MetadataStringTypes, RetValue> {
                           return std::forward<Func>(f)(ObjectValueTag<ObjectValueType::String>{}, std::move(v));
                       },
                       [&f](auto&& v) -> util::IfTypeOf<decltype(v), MetadataVectorTypes, RetValue> {
                           // Not directely a ValueList as specified as list type, propagated value must just fit with
                           // forEachList
                           return std::forward<Func>(f)(ObjectValueTag<ObjectValueType::List>{}, std::move(v));
                       }});
    }


    // Visit for scalar values - required as iterating a list type is not forwarding a MetadataValue but a direct
    // scalartype
    template <typename V, typename Func,
              std::enable_if_t<util::TypeListContains<std::decay_t<V>, MetadataIntegerTypes>::value, bool> = true>
    static decltype(auto) visit(V&& v, Func&& f) {
        return std::forward<Func>(f)(ObjectValueTag<ObjectValueType::Number>{}, std::forward<V>(v));
    }
    template <typename V, typename Func,
              std::enable_if_t<util::TypeListContains<std::decay_t<V>, MetadataFloatingTypes>::value, bool> = true>
    static decltype(auto) visit(V&& v, Func&& f) {
        return std::forward<Func>(f)(ObjectValueTag<ObjectValueType::Number>{}, std::forward<V>(v));
    }
    template <typename V, typename Func,
              std::enable_if_t<util::TypeListContains<std::decay_t<V>, MetadataStringTypes>::value, bool> = true>
    static decltype(auto) visit(V&& v, Func&& f) {
        return std::forward<Func>(f)(ObjectValueTag<ObjectValueType::String>{}, std::forward<V>(v));
    }

    // Bool needs special handling because it has a reference to special vector type....
    template <typename Func>
    static decltype(auto) visit(bool b, Func&& f) {
        return std::forward<Func>(f)(ObjectValueTag<ObjectValueType::Bool>{}, b);
    }
};


//-----------------------------------------------------------------------------

}  // namespace multio::message

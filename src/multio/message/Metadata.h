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

//-----------------------------------------------------------------------------

using MetadataValueVariant = util::ApplyTypeList_t<std::variant, typename MetadataTypes::AllWrapped>;

class MetadataValue : public MetadataValueVariant {
public:
    using Types = MetadataTypes;

    using This = MetadataValue;
    using Base = MetadataValueVariant;
    using MetadataValueVariant::MetadataValueVariant;
    using Base::operator=;

    MetadataValue(const This& other);
    MetadataValue(This&&) noexcept = default;

    This& operator=(const This& other);

    This& operator=(This&&) noexcept = default;

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


    //-----------------------------------------------------------------------------

    // Implementation details


    // Special constructor to transparently create nested types that are supposed to be wrapped with unique_ptr
    template <typename T,
              std::enable_if_t<
                  util::TypeListContains<std::unique_ptr<std::decay_t<T>>, typename Types::AllWrapped>::value, bool>
              = true>
    MetadataValue(T&& val) : MetadataValue(std::make_unique<std::decay_t<T>>(std::forward<T>(val))){};

    // Constructor that deals with all other cases exlusive to the unique_ptr handling
    template <
        typename T,
        std::enable_if_t<(!util::TypeListContains<std::unique_ptr<std::decay_t<T>>, typename Types::AllWrapped>::value
                          && !std::is_same<std::decay_t<T>, This>::value && !std::is_same<std::decay_t<T>, Base>::value
                          && std::is_constructible<Base, T>::value),
                         bool>
        = true>
    MetadataValue(T&& val) : Base(std::forward<T>(val)){};

private:
    // Implementation details

    template <typename T,
              std::enable_if_t<
                  !util::TypeListContains<std::unique_ptr<std::decay_t<T>>, typename Types::AllWrapped>::value, bool>
              = true>
    static T&& wrapNestedMaybe(T&& v) noexcept {
        return std::forward<T>(v);
    }
    template <typename T,
              std::enable_if_t<
                  util::TypeListContains<std::unique_ptr<std::decay_t<T>>, typename Types::AllWrapped>::value, bool>
              = true>
    static std::unique_ptr<std::decay_t<T>> wrapNestedMaybe(T&& v) {
        return std::make_unique<std::decay_t<T>>(std::forward<T>(v));
    }


    template <typename T, typename This_>
    static decltype(auto) resolvedUniquePtrGetter(This_&& val) {
        static_assert(util::TypeListContains<std::decay_t<T>, typename Types::AllWrapped>::value);
        if (val.index() == util::GetVariantIndex<std::decay_t<T>, Base>::value) {
            return std::get<T>(std::forward<This_>(val));
        }
        throw MetadataWrongTypeException(util::GetVariantIndex<std::decay_t<T>, Base>::value, val.index(), Here());
    }

    template <typename T, typename This_,
              std::enable_if_t<util::TypeListContains<std::decay_t<T>, typename Types::AllWrapped>::value, bool> = true>
    static decltype(auto) uniquePtrGetter(This_&& val) {
        return resolvedUniquePtrGetter<T>(std::forward<This_>(val));
    }

    template <typename T, typename This_,
              std::enable_if_t<
                  util::TypeListContains<std::unique_ptr<std::decay_t<T>>, typename Types::AllWrapped>::value, bool>
              = true>
    static decltype(auto) uniquePtrGetter(This_&& val) {
        if constexpr (std::is_rvalue_reference<This_>::value) {
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

    //-----------------------------------------------------------------------------
};


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
public:
    using This = Metadata;
    using KeyType = typename MetadataTypes::KeyType;
    using MapType = typename MetadataTypes::MapType<MetadataValue>;

public:
    Metadata(const Metadata&) = default;
    Metadata(Metadata&&) noexcept = default;

    Metadata();
    Metadata(std::initializer_list<std::pair<const KeyType, MetadataValue>> li);

protected:
    // Used from update()
    Metadata(MapType&& values);

public:
    This& operator=(const This&) = default;
    This& operator=(This&&) noexcept = default;

    MetadataValue&& get(const KeyType& k) &&;
    MetadataValue& get(const KeyType& k) &;
    const MetadataValue& get(const KeyType& k) const&;


    std::optional<MetadataValue> getOpt(const KeyType& k) && noexcept { return optionalGetter(std::move(*this), k); }
    std::optional<MetadataValue> getOpt(const KeyType& k) & noexcept { return optionalGetter(*this, k); }
    std::optional<MetadataValue> getOpt(const KeyType& k) const& noexcept { return optionalGetter(*this, k); }

    template <typename T>
    T&& get(const KeyType& k) && {
        return std::move(referenceGetter<T>(*this, k).get());
    }

    template <typename T>
    T& get(const KeyType& k) & {
        return referenceGetter<T>(*this, k).get();
    }

    template <typename T>
    const T& get(const KeyType& k) const& {
        return referenceGetter<T>(*this, k).get();
    }

    template <typename T>
    std::optional<T> getOpt(const KeyType& k) && noexcept {
        return optionalGetter<T>(std::move(*this), k);
    }

    template <typename T>
    std::optional<T> getOpt(const KeyType& k) const& noexcept {
        return optionalGetter<T>(*this, k);
    }

    MetadataValue& operator[](const KeyType& key);
    MetadataValue& operator[](KeyType&& key);

    template <typename V>
    void set(KeyType&& k, V&& v) {
        values_.insert_or_assign(std::move(k), std::forward<V>(v));
    }

    template <typename V>
    void set(const KeyType& k, V&& v) {
        values_.insert_or_assign(k, std::forward<V>(v));
    }

    // Adds a value if not already contained
    template <typename V>
    auto trySet(KeyType&& k, V&& v) {
        return values_.try_emplace(std::move(k), std::forward<V>(v));
    }

    // Adds a value if not already contained
    template <typename V>
    auto trySet(const KeyType& k, V&& v) {
        return values_.try_emplace(k, std::forward<V>(v));
    }


    auto find(const KeyType& k) { return values_.find(k); };

    auto find(const KeyType& k) const { return values_.find(k); };

    auto begin() noexcept { return values_.begin(); };

    auto begin() const noexcept { return values_.begin(); };

    auto cbegin() const noexcept { return values_.cbegin(); };

    auto end() noexcept { return values_.end(); };

    auto end() const noexcept { return values_.end(); };

    auto cend() const noexcept { return values_.cend(); };


    bool empty() const noexcept;

    std::size_t size() const noexcept;

    void clear() noexcept;

    /**
     * Extracts all values from other metadata whose keys are not contained yet in this metadata.
     * Explicitly always modifies both metadata.
     */
    void merge(This& other);
    void merge(This&& other);

    /**
     * Adds all Metadata contained in other and returns a Metadata object with key/values that have been overwritten.
     * Existing iterators to this container are invalidated after an update.
     */
    This update(const This& other);
    This update(This&& other);


    std::string toString() const;
    void json(eckit::JSON& j) const;

private:
    MapType values_;

    //-----------------------------------------------------------------------------
    // Implementation details

    template <typename T, typename This_>
    static decltype(auto) referenceGetter(This_&& val, const KeyType& k) {
        if (auto search = val.values_.find(k); search != val.values_.end()) {
            try {
                return std::ref(search->second.template get<T>());
            }
            catch (const MetadataException& err) {
                std::throw_with_nested(MetadataKeyException(k, err.what(), Here()));
            }
        }
        throw MetadataMissingKeyException(k, Here());
    }

    template <typename This_>
    static decltype(auto) referenceGetter(This_&& val, const KeyType& k) {
        if (auto search = val.values_.find(k); search != val.values_.end()) {
            return std::ref(search->second);
        }
        throw MetadataMissingKeyException(k, Here());
    }

    template <typename T, typename This_>
    static std::optional<T> optionalGetter(This_&& val, const KeyType& k) {
        if (auto search = val.values_.find(k); search != val.values_.end()) {
            try {
                if constexpr (std::is_rvalue_reference<This_>::value) {
                    return std::move(search->second.template get<T>());
                }
                else {
                    return search->second.template get<T>();
                }
            }
            catch (const MetadataException& err) {
                std::throw_with_nested(MetadataKeyException(k, err.what(), Here()));
            }
        }
        return std::nullopt;
    }

    template <typename This_>
    static std::optional<MetadataValue> optionalGetter(This_&& val, const KeyType& k) noexcept {
        if (auto search = val.values_.find(k); search != val.values_.end()) {
            if constexpr (std::is_rvalue_reference<This_>::value) {
                return std::optional<MetadataValue>{std::move(search->second)};
            }
            else {
                return std::optional<MetadataValue>{search->second};
            }
        }
        return std::nullopt;
    }

    //-----------------------------------------------------------------------------
};


//-----------------------------------------------------------------------------


eckit::JSON& operator<<(eckit::JSON& json, const MetadataValue& mv);

eckit::JSON& operator<<(eckit::JSON& json, const Metadata& metadata);

//-----------------------------------------------------------------------------

std::ostream& operator<<(std::ostream& os, const MetadataValue& metadataValue);

std::ostream& operator<<(std::ostream& os, const Metadata& metadata);


//-----------------------------------------------------------------------------

Metadata metadataFromYAML(const std::string& fieldId);


//-----------------------------------------------------------------------------

std::optional<MetadataValue> toMetadataValue(const eckit::Value& v);

std::optional<Metadata> toMetadataMaybe(const eckit::Value& v);

Metadata toMetadata(const eckit::Value& value);

//-----------------------------------------------------------------------------


}  // namespace multio::message

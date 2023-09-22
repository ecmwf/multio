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
#include "multio/message/details/MetadataTypes.h"
#include "multio/util/VariantHelpers.h"

#include "eckit/log/JSON.h"
#include "eckit/parser/YAMLParser.h"

#include <sstream>


namespace multio::message::details {

//-----------------------------------------------------------------------------

template <typename Traits>
using MetadataValueVariant = util::ApplyTypeList_t<std::variant, typename MetadataTypes<Traits>::AllWrapped>;


template <typename Traits_>
class MetadataValue : public MetadataValueVariant<Traits_> {
public:
    using Traits = Traits_;
    using Types = MetadataTypes<Traits>;

    using This = MetadataValue<Traits>;
    using Base = MetadataValueVariant<Traits>;
    using MetadataValueVariant<Traits_>::MetadataValueVariant;
    using Base::operator=;

    MetadataValue(const This& other) :
        Base{other.visit([](auto&& v) { return Base{wrapNestedMaybe(std::forward<decltype(v)>(v))}; })} {}
    MetadataValue(This&&) noexcept = default;

    This& operator=(const This& other) {
        Base::operator=(other.visit([](auto&& v) { return Base{wrapNestedMaybe(std::forward<decltype(v)>(v))}; }));
        return *this;
    }

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


    std::string toString() const {
        std::stringstream ss;
        eckit::JSON json(ss);
        json << *this;
        return ss.str();
    }

    void json(eckit::JSON& j) const { j << *this; }


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


}  // namespace multio::message::details

namespace std {
template <typename Traits>
struct variant_size<multio::message::details::MetadataValue<Traits>>
    : variant_size<multio::message::details::MetadataValueVariant<Traits>> {};

template <std::size_t I, typename Traits>
struct variant_alternative<I, multio::message::details::MetadataValue<Traits>>
    : variant_alternative<I, multio::message::details::MetadataValueVariant<Traits>> {};
}  // namespace std

namespace multio::util {

template <typename T, typename Traits>
struct util::GetVariantIndex<T, multio::message::details::MetadataValue<Traits>>
    : util::GetVariantIndex<T, multio::message::details::MetadataValueVariant<Traits>> {};

template <typename Traits>
struct util::GetVariantIndex<multio::message::details::Metadata<Traits>,
                             multio::message::details::MetadataValueVariant<Traits>>
    : util::GetVariantIndex<std::unique_ptr<multio::message::details::Metadata<Traits>>,
                            multio::message::details::MetadataValueVariant<Traits>> {};

}  // namespace multio::util

//-----------------------------------------------------------------------------

namespace multio::message::details {

template <typename Traits>
class Metadata {
public:
    using This = Metadata<Traits>;
    using KeyType = typename Traits::KeyType;
    using MapType = typename Traits::template MapType<MetadataValue<Traits>>;

public:
    Metadata(const Metadata&) = default;
    Metadata(Metadata&&) noexcept = default;

    Metadata() : values_{Traits::template initMap<MetadataValue<Traits>>()} {}
    Metadata(std::initializer_list<std::pair<const KeyType, MetadataValue<Traits>>> li) :
        values_{Traits::template initMap<MetadataValue<Traits>>(std::move(li))} {}

protected:
    // Used from update()
    Metadata(MapType&& values) : values_{std::move(values)} {}

public:
    This& operator=(const This&) = default;
    This& operator=(This&&) noexcept = default;

    MetadataValue<Traits>&& get(const KeyType& k) && { return std::move(referenceGetter(*this, k).get()); }

    MetadataValue<Traits>& get(const KeyType& k) & { return referenceGetter(*this, k).get(); }

    const MetadataValue<Traits>& get(const KeyType& k) const& { return referenceGetter(*this, k).get(); }


    std::optional<MetadataValue<Traits>> getOpt(const KeyType& k) && noexcept {
        return optionalGetter(std::move(*this), k);
    }
    std::optional<MetadataValue<Traits>> getOpt(const KeyType& k) & noexcept { return optionalGetter(*this, k); }
    std::optional<MetadataValue<Traits>> getOpt(const KeyType& k) const& noexcept { return optionalGetter(*this, k); }

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

    MetadataValue<Traits>& operator[](const KeyType& key) { return values_[key]; }
    MetadataValue<Traits>& operator[](KeyType&& key) { return values_[std::move(key)]; }

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

    bool empty() const noexcept { return values_.empty(); }

    std::size_t size() const noexcept { return values_.size(); }

    void clear() noexcept { values_.clear(); }

    /**
     * Extracts all values from other metadata whose keys are not contained yet in this metadata.
     * Explicitly always modifies both metadata.
     */
    void merge(This& other) { values_.merge(other.values_); }
    void merge(This&& other) { values_.merge(std::move(other.values_)); }

    /**
     * Adds all Metadata contained in other and returns a Metadata object with key/values that have been overwritten.
     * Existing iterators to this container are invalidated after an update.
     */
    This update(const This& other) {
        auto tmp = std::move(values_);
        values_ = other.values_;
        values_.merge(tmp);
        return tmp;
    }


    This update(This&& other) {
        auto tmp = std::move(values_);
        values_ = std::move(other.values_);
        values_.merge(tmp);
        return tmp;
    }


    std::string toString() const {
        std::stringstream ss;
        eckit::JSON json(ss);
        json << *this;
        return ss.str();
    }

    void json(eckit::JSON& j) const { j << *this; }

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
    static std::optional<MetadataValue<Traits>> optionalGetter(This_&& val, const KeyType& k) noexcept {
        if (auto search = val.values_.find(k); search != val.values_.end()) {
            if constexpr (std::is_rvalue_reference<This_>::value) {
                return std::optional<MetadataValue<Traits>>{std::move(search->second)};
            }
            else {
                return std::optional<MetadataValue<Traits>>{search->second};
            }
        }
        return std::nullopt;
    }

    //-----------------------------------------------------------------------------
};


//-----------------------------------------------------------------------------


template <typename Traits>
eckit::JSON& operator<<(eckit::JSON& json, const MetadataValue<Traits>& mv) {
    mv.visit([&json](const auto& v) { json << v; });
    return json;
}

template <typename Traits>
eckit::JSON& operator<<(eckit::JSON& json, const Metadata<Traits>& metadata) {
    json.startObject();
    for (const auto& kv : metadata) {
        json << kv.first;
        json << kv.second;
    }
    json.endObject();
    return json;
}

//-----------------------------------------------------------------------------

template <typename Traits>
std::ostream& operator<<(std::ostream& os, const MetadataValue<Traits>& metadataValue) {
    eckit::JSON json(os);
    json << metadataValue;
    return os;
}


template <typename Traits>
std::ostream& operator<<(std::ostream& os, const Metadata<Traits>& metadata) {
    eckit::JSON json(os);
    json << metadata;
    return os;
}


//-----------------------------------------------------------------------------


}  // namespace multio::message::details

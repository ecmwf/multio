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


private:
    struct details {
        template <typename T, typename This_>
        static decltype(auto) getter(This_&& val) {
            static_assert(util::TypeListContains<std::decay_t<T>, typename Types::AllWrapped>::value);
            if (val.index() == util::GetVariantIndex<std::decay_t<T>, Base>::value) {
                return std::get<T>(std::forward<This_>(val));
            }
            throw MetadataWrongTypeException(util::GetVariantIndex<std::decay_t<T>, Base>::value, val.index(), Here());
        }
    };

public:
    MetadataValue(const This& other) : Base{other.visit([](auto&& v) { return Base{std::forward<decltype(v)>(v)}; })} {}

    MetadataValue(This&&) noexcept = default;

    This& operator=(const This& other) {
        Base::operator=(other.visit([](auto&& v) { return Base{std::forward<decltype(v)>(v)}; }));
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


    // Getter with TypeTag to allow specialization via overloading
    template <typename T,
              std::enable_if_t<util::TypeListContains<std::decay_t<T>, typename Types::AllWrapped>::value, bool> = true>
    const T& get() const& {
        return details::template getter<T>(*this);
    }

    template <typename T,
              std::enable_if_t<util::TypeListContains<std::decay_t<T>, typename Types::AllWrapped>::value, bool> = true>
    T& get() & {
        return details::template getter<T>(*this);
    }

    template <typename T,
              std::enable_if_t<util::TypeListContains<std::decay_t<T>, typename Types::AllWrapped>::value, bool> = true>
    T&& get() && {
        return details::template getter<T>(std::move(*this));
    }

    // Specialized get for unique_ptr & nested types
    template <typename T,
              std::enable_if_t<
                  util::TypeListContains<std::unique_ptr<std::decay_t<T>>, typename Types::AllWrapped>::value, bool>
              = true>
    const T& get() const& {
        return *get<std::unique_ptr<T>>().get();
    }

    template <typename T,
              std::enable_if_t<
                  util::TypeListContains<std::unique_ptr<std::decay_t<T>>, typename Types::AllWrapped>::value, bool>
              = true>
    T& get() & {
        return *get<std::unique_ptr<T>>().get();
    }

    template <typename T,
              std::enable_if_t<
                  util::TypeListContains<std::unique_ptr<std::decay_t<T>>, typename Types::AllWrapped>::value, bool>
              = true>
    T&& get() && {
        return std::move(*(get<std::unique_ptr<T>>()).get());
    }
};


template <typename Traits>
std::ostream& operator<<(std::ostream& os, const MetadataValue<Traits>& metadataValue) {
    eckit::JSON json(os);
    toJSON(metadataValue, json);
    return os;
}

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

private:
    struct details {
        template <typename T, typename This_>
        static decltype(auto) refGetter(This_&& val, const KeyType& k) {
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

        template <typename This>
        static decltype(auto) refGetter(This&& val, const KeyType& k) {
            if (auto search = val.values_.find(k); search != val.values_.end()) {
                return std::ref(search->second);
            }
            throw MetadataMissingKeyException(k, Here());
        }

        template <typename T, typename This>
        static std::optional<T> optGetter(This&& val, const KeyType& k) {
            if (auto search = val.values_.find(k); search != val.values_.end()) {
                try {
                    if constexpr (std::is_rvalue_reference<This>::value) {
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

        template <typename This>
        static std::optional<MetadataValue<Traits>> optGetter(This&& val, const KeyType& k) noexcept {
            if (auto search = val.values_.find(k); search != val.values_.end()) {
                if constexpr (std::is_rvalue_reference<This>::value) {
                    return std::optional<MetadataValue<Traits>>{std::move(search->second)};
                }
                else {
                    return std::optional<MetadataValue<Traits>>{search->second};
                }
            }
            return std::nullopt;
        }
    };

    MapType values_;

protected:
    Metadata(const MapType& values) : values_{values} {};
    Metadata(MapType&& values) : values_{std::move(values)} {};

public:
    Metadata(const Metadata&) = default;
    Metadata(Metadata&&) noexcept = default;

    Metadata() : values_{Traits::template initMap<MetadataValue<Traits>>()} {}
    Metadata(std::initializer_list<std::pair<const KeyType, MetadataValue<Traits>>> li) :
        values_{Traits::template initMap<MetadataValue<Traits>>(std::move(li))} {}

    // // To be removed in the future
    // Metadata(const eckit::Value& v) : Metadata(toMetadata(v)) {}
    // Metadata(const eckit::Configuration& c) : Metadata(c.get()) {}


    This& operator=(const This&) = default;
    This& operator=(This&&) noexcept = default;

    // User-defined conversion to unique_ptr - simply usage with assign through implict conversion
    operator std::unique_ptr<This>() const& { return std::make_unique<This>(*this); }

    operator std::unique_ptr<This>() & { return std::make_unique<This>(*this); }

    operator std::unique_ptr<This>() && { return std::make_unique<This>(std::move(*this)); }


    MetadataValue<Traits>&& get(const KeyType& k) && { return std::move(details::refGetter(*this, k).get()); }

    MetadataValue<Traits>& get(const KeyType& k) & { return details::refGetter(*this, k).get(); }

    const MetadataValue<Traits>& get(const KeyType& k) const& { return details::refGetter(*this, k).get(); }


    std::optional<MetadataValue<Traits>> getOpt(const KeyType& k) && noexcept {
        return details::optGetter(std::move(*this), k);
    }
    std::optional<MetadataValue<Traits>> getOpt(const KeyType& k) & noexcept { return details::optGetter(*this, k); }
    std::optional<MetadataValue<Traits>> getOpt(const KeyType& k) const& noexcept {
        return details::optGetter(*this, k);
    }

    template <typename T>
    T&& get(const KeyType& k) && {
        return std::move(details::template refGetter<T>(*this, k).get());
    }

    template <typename T>
    T& get(const KeyType& k) & {
        return details::template refGetter<T>(*this, k).get();
    }

    template <typename T>
    const T& get(const KeyType& k) const& {
        return details::template refGetter<T>(*this, k).get();
    }

    template <typename T>
    std::optional<T> getOpt(const KeyType& k) && noexcept {
        return details::template optGetter<T>(std::move(*this), k);
    }

    template <typename T>
    std::optional<T> getOpt(const KeyType& k) const& noexcept {
        return details::template optGetter<T>(*this, k);
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
     * Extracts all values from other metadata whose keysare not contained yet in this metadata.
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
};


//-----------------------------------------------------------------------------

template <typename T>
void toJSON(const T& v, eckit::JSON& json) {
    json << v;
}

void toJSON(const Null&, eckit::JSON& json);
;

template <typename Traits>
void toJSON(const Metadata<Traits>& metadata, eckit::JSON& json);

template <typename Traits>
void toJSON(const MetadataValue<Traits>& mv, eckit::JSON& json) {
    mv.visit([&json](const auto& v) { toJSON(v, json); });
}

template <typename T>
void toJSON(const std::vector<T>& v, eckit::JSON& json) {
    json.startList();
    for (const auto& vi : v) {
        toJSON(vi, json);
    }
    json.endList();
}

template <typename Traits>
void toJSON(const Metadata<Traits>& metadata, eckit::JSON& json) {
    json.startObject();
    for (const auto& kv : metadata) {
        json << kv.first;
        toJSON(kv.second, json);
    }
    json.endObject();
}

//-----------------------------------------------------------------------------

template <typename Traits>
std::ostream& operator<<(std::ostream& os, const Metadata<Traits>& metadata) {
    eckit::JSON json(os);
    toJSON(metadata, json);
    return os;
}


//-----------------------------------------------------------------------------

template <typename Traits>
std::string toString(const Metadata<Traits>& metadata) {
    std::stringstream ss;
    eckit::JSON json(ss);
    toJSON(metadata, json);
    return ss.str();
}

//-----------------------------------------------------------------------------

}  // namespace multio::message::details

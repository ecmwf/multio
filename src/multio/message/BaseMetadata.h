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

#include "multio/message/MetadataValue.h"


//----------------------------------------------------------------------------------------------------------------------


namespace multio::message {

class BaseMetadata {
public:
    using This = BaseMetadata;
    using KeyType = typename MetadataTypes::KeyType;
    using MapType = typename MetadataTypes::MapType<MetadataValue>;

    BaseMetadata(const BaseMetadata&) = default;
    BaseMetadata(BaseMetadata&&) noexcept = default;

    BaseMetadata();
    BaseMetadata(std::initializer_list<std::pair<const KeyType, MetadataValue>> li);

    virtual ~BaseMetadata() {};

    // Used from update()
    BaseMetadata(MapType&& values);
    BaseMetadata(const MapType& values);

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

    using Iterator = typename MapType::iterator;
    using ConstIterator = typename MapType::const_iterator;


    // Find is virtual to allow modifying behaviour for Parametrization
    virtual Iterator find(const KeyType& k) { return values_.find(k); };
    virtual ConstIterator find(const KeyType& k) const { return values_.find(k); };
    Iterator begin() noexcept { return values_.begin(); };
    ConstIterator begin() const noexcept { return values_.begin(); };
    ConstIterator cbegin() const noexcept { return values_.cbegin(); };
    Iterator end() noexcept { return values_.end(); };
    ConstIterator end() const noexcept { return values_.end(); };
    ConstIterator cend() const noexcept { return values_.cend(); };

    std::size_t erase(const KeyType& k) { return values_.erase(k); };
    Iterator erase(Iterator it) { return values_.erase(it); };
    Iterator erase(ConstIterator it) { return values_.erase(it); };
    Iterator erase(ConstIterator first, ConstIterator last) { return values_.erase(first, last); };


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
    void updateOverwrite(const This& other);
    void updateOverwrite(This&& other);

    /**
     * Adds all Metadata contained in other and returns a Metadata object with key/values that have been overwritten.
     * Similar to merge unless that the right-hand-side map is not actively modified.
     * Existing iterators to this container are invalidated after an update.
     */
    void updateNoOverwrite(const This& other);
    void updateNoOverwrite(This&& other);


    std::string toString() const;
    void json(eckit::JSON& j) const;

    // Allow accessing the map for derived type to simplify implementing Parametrization lookups
protected:
    MapType values_;

    //----------------------------------------------------------------------------------------------------------------------

    // Implementation details

    template <typename T, typename This_>
    static decltype(auto) referenceGetter(This_&& val, const KeyType& k) {
        if (auto search = val.find(k); search != val.end()) {
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
        if (auto search = val.find(k); search != val.end()) {
            return std::ref(search->second);
        }
        throw MetadataMissingKeyException(k, Here());
    }

    template <typename T, typename This_>
    static std::optional<T> optionalGetter(This_&& val, const KeyType& k) {
        if (auto search = val.find(k); search != val.end()) {
            try {
                if constexpr (!std::is_lvalue_reference_v<This_>) {
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
        if (auto search = val.find(k); search != val.end()) {
            if constexpr (!std::is_lvalue_reference_v<This_>) {
                return std::optional<MetadataValue>{std::move(search->second)};
            }
            else {
                return std::optional<MetadataValue>{search->second};
            }
        }
        return std::nullopt;
    }

    //----------------------------------------------------------------------------------------------------------------------
};


//----------------------------------------------------------------------------------------------------------------------

eckit::JSON& operator<<(eckit::JSON& json, const BaseMetadata& metadata);

std::ostream& operator<<(std::ostream& os, const BaseMetadata& metadata);


//-----------------------------------------------------------------------------


}  // namespace multio::message

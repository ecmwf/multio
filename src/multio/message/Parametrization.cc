/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "Parametrization.h"
#include "Metadata.h"

#include <algorithm>
#include <unordered_map>
#include <unordered_set>

#include "eckit/log/Log.h"
#include "multio/LibMultio.h"

#include "multio/util/TypeTraits.h"


namespace multio::message {


namespace parametrization {

using multio::util::TypeTag;

// TODO - may move the dispatch functions to header

template <typename T>
struct MapTypeRepres {
    using type = T;  // Default - identity
};

template <typename T>
using MapTypeRepres_t = typename MapTypeRepres<T>::type;


// Specialization - int32_t is not supported in metadata, hence store as int64_t
template <>
struct MapTypeRepres<std::int32_t> {
    using type = std::int64_t;
};


template <typename Func>
decltype(auto) dispatchElementType(std::string_view typeStr, Func&& f) {
    // may throw
    return dispatchElementType(decodeElementType(typeStr), std::forward<Func>(f));
}

// Disptach function might not return a value...
// Decltype(auto) requires at least C++14... moreover the returntype must be the same for all dispatching results. If
// not std::variant may be used...
template <typename Func>
decltype(auto) dispatchElementType(ElementType t, Func&& f) {
    switch (t) {
        case ElementType::Byte: {
            return std::forward<Func>(f)(TypeTag<GetElementType_t<ElementType::Byte>>{});
        }
        case ElementType::Int32: {
            return std::forward<Func>(f)(TypeTag<GetElementType_t<ElementType::Int32>>{});
        }
        case ElementType::Int64: {
            return std::forward<Func>(f)(TypeTag<GetElementType_t<ElementType::Int64>>{});
        }
        case ElementType::Real32: {
            return std::forward<Func>(f)(TypeTag<GetElementType_t<ElementType::Real32>>{});
        }
        case ElementType::Real64: {
            return std::forward<Func>(f)(TypeTag<GetElementType_t<ElementType::Real64>>{});
        }
        default:
            std::ostringstream oss;
            oss << "Error in dispatchElementType: Unkown tag " << ((unsigned)t) << std::endl;
            throw eckit::SeriousBug(oss.str(), Here());
    }
}


ElementType decodeElementType(std::string_view typeKey) {
    static const std::unordered_map<std::string_view, ElementType> map{
        {"byte", ElementType::Byte},     {"int64", ElementType::Int64},   {"int32", ElementType::Int32},
        {"real64", ElementType::Real64}, {"real32", ElementType::Real32},
    };

    if (auto search = map.find(typeKey); search != map.end()) {
        return search->second;
    }

    std::ostringstream oss;
    oss << "Parametrization error: unknown element type \"" << typeKey << "\"";
    throw MetadataException(oss.str(), Here());
}


std::string toString(ElementType et) {
    switch (et) {
        case ElementType::Byte:
            return "byte";
        case ElementType::Int32:
            return "int32";
        case ElementType::Int64:
            return "int64";
        case ElementType::Real32:
            return "real32";
        case ElementType::Real64:
            return "real64";
        default:
            NOTIMP;
    }
}

}  // namespace parametrization


Parametrization& Parametrization::instance() {
    static Parametrization singleton;
    return singleton;
}

BaseMetadata& Parametrization::get() {
    return data_;
}

void Parametrization::clear() {
    data_ = BaseMetadata{};
}


void Parametrization::update(const BaseMetadata& other) {
    const static std::unordered_set<std::string> ignoreKeys{
        {PARAMETRIZATION_PAYLOAD_KEY, PARAMETRIZATION_PAYLOAD_ELEMENT_TYPE}};
    std::lock_guard<std::mutex> lock{mutex_};


    for (const auto& kv : other) {
        if (ignoreKeys.find(kv.first.value()) == ignoreKeys.end()) {
            update(kv.first.value(), kv.second);
        }
    }
}


void Parametrization::update(std::string_view key, parametrization::ElementType et, const void* data,
                             std::size_t size) {
    std::lock_guard<std::mutex> lock{mutex_};

    parametrization::dispatchElementType(et, [&](auto tt) {
        using Type = typename std::decay_t<decltype(tt)>::type;
        using parametrization::MapTypeRepres_t;

        // Copy data into a vector - may convert type if `MapTypeRepres_t` is specialized
        update(key, MetadataValue{std::vector<MapTypeRepres_t<Type>>(
                        static_cast<const Type*>(data), static_cast<const Type*>(data) + (size / sizeof(Type)))});
    });
}

void Parametrization::update(std::string_view key, std::string_view kt, const void* data, std::size_t size) {
    update(key, parametrization::decodeElementType(kt), data, size);
}


void Parametrization::update(const Message& msg) {
    const auto& md = msg.metadata();
    auto searchKey = md.find(PARAMETRIZATION_PAYLOAD_KEY);

    if (searchKey != md.end()) {
        auto& payload = msg.payload();
        if (msg.payload().size() == 0) {
            std::ostringstream oss;
            oss << "Parametrization error. Key " << PARAMETRIZATION_PAYLOAD_KEY << " given with value \""
                << searchKey->second << "\" is specified but payload is empty.";
            throw MetadataException(oss.str(), Here());
        }
        auto searchType = md.find(PARAMETRIZATION_PAYLOAD_ELEMENT_TYPE);
        if (searchType == md.end()) {
            std::ostringstream oss;
            oss << "Parametrization error. Key " << PARAMETRIZATION_PAYLOAD_KEY << " given with value \""
                << searchKey->second << "\" but no key type \"" << PARAMETRIZATION_PAYLOAD_ELEMENT_TYPE
                << "\" is given.";
            throw MetadataException(oss.str(), Here());
        }

        update(searchKey->second.get<std::string>(), searchType->second.get<std::string>(), payload.data(),
               payload.size());
    }

    update(md);
}


void Parametrization::update(std::string_view key, const MetadataValue& val) {
    auto it = data_.find(key);
    if (it == data_.end()) {
        eckit::Log::debug<LibMultio>() << "Parametrization :: " << key << ": " << val << std::endl;
        data_.set(key, val);
    }
    else {
        if (it->second != val) {
            std::ostringstream oss;
            oss << "Parametrization error. Key " << key << " already contains a different value: " << it->second
                << " != " << val;
            throw MetadataException(oss.str(), Here());
        }
    }
}


void Parametrization::print(std::ostream& out) const {
    out << "Parametrization :: " << data_;
}


}  // namespace multio::message

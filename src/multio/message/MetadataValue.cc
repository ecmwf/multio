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

#include "multio/message/MetadataValue.h"

#include "multio/message/Metadata.h"

#include "eckit/parser/YAMLParser.h"

#include <sstream>


namespace multio::message {

//----------------------------------------------------------------------------------------------------------------------


MetadataValue::MetadataValue(const This& other) :
    Base{other.visit([](auto&& v) { return Base{wrapNestedMaybe(std::forward<decltype(v)>(v))}; })} {}


MetadataValue& MetadataValue::operator=(const This& other) {
    Base::operator=(other.visit([](auto&& v) { return Base{wrapNestedMaybe(std::forward<decltype(v)>(v))}; }));
    return *this;
}

std::string MetadataValue::toString() const {
    std::stringstream ss;
    eckit::JSON json(ss);
    json << *this;
    return ss.str();
}

void MetadataValue::json(eckit::JSON& j) const {
    j << *this;
}


//----------------------------------------------------------------------------------------------------------------------

eckit::JSON& operator<<(eckit::JSON& json, const MetadataValue& mv) {
    mv.visit([&json](const auto& v) { json << v; });
    return json;
}


std::ostream& operator<<(std::ostream& os, const MetadataValue& metadataValue) {
    eckit::JSON json(os);
    json << metadataValue;
    return os;
}


//----------------------------------------------------------------------------------------------------------------------

std::optional<MetadataValue> tryToMetadataValue(const eckit::Value& v) {
    if (v.isList()) {
        if (v.size() == 0) {
            return std::nullopt;
        }
        auto fillVec = [&v](auto vec) {
            auto size = v.size();
            vec.reserve(size);
            for (unsigned int i = 0; i < size; ++i) {
                vec.push_back(v[i]);
            }
            return MetadataValue{std::move(vec)};
        };


        if (v[0].isNumber()) {
            return fillVec(std::vector<std::int64_t>{});
        }
        if (v[0].isDouble()) {
            return fillVec(std::vector<double>{});
        }
        if (v[0].isBool()) {
            return fillVec(std::vector<bool>{});
        }
        if (v[0].isString()) {
            return fillVec(std::vector<std::string>{});
        }
        return std::nullopt;
    }
    if (v.isMap()) {
        return toMetadata(v);
    }
    if (v.isNumber()) {
        return MetadataValue{(std::int64_t)v};
    }
    if (v.isDouble()) {
        return MetadataValue{(double)v};
    }
    if (v.isBool()) {
        return MetadataValue{(bool)v};
    }
    if (v.isString()) {
        return MetadataValue{(std::string)v};
    }
    return std::nullopt;
}


//----------------------------------------------------------------------------------------------------------------------


}  // namespace multio::message


//----------------------------------------------------------------------------------------------------------------------


std::size_t std::hash<multio::message::MetadataValue>::operator()(const multio::message::MetadataValue& t) const {
    return t.visit([&](const auto& v) -> std::size_t {
        using T = std::decay_t<decltype(v)>;
        if constexpr (std::is_same_v<T, multio::message::Metadata>) {
            throw multio::message::MetadataException("Hashing of Metadata is not supported", Here());
        }
        else if constexpr (multio::util::IsVector_v<T>) {
            throw multio::message::MetadataException("Hashing of vector is not supported", Here());
        }
        else {
            return std::hash<T>{}(v);
        }
    });
}

//----------------------------------------------------------------------------------------------------------------------

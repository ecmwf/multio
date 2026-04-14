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

#include <optional>
#include <string>
#include <string_view>
#include <type_traits>
#include <vector>

#include "eckit/config/LocalConfiguration.h"
#include "eckit/exception/Exceptions.h"

#include "multio/message/Metadata.h"
#include "multio/util/record/Entry.h"

#include "multio/datamod/types/LevType.h"
#include "multio/datamod/types/Param.h"
#include "multio/datamod/types/StatType.h"


namespace multio::datamod::detail {

// Re-export shared types
using multio::util::record::Entry;
using multio::util::record::HasFieldsMember_v;

//----------------------------------------------------------------------------------------------------------------------
// Type-specific parseEntry overloads for message::Metadata
// Each returns true if the key was found and parsed, false if absent.
//----------------------------------------------------------------------------------------------------------------------

bool parseEntry(std::string& value, std::string_view key, const message::Metadata& md);
bool parseEntry(std::int64_t& value, std::string_view key, const message::Metadata& md);
bool parseEntry(double& value, std::string_view key, const message::Metadata& md);
bool parseEntry(bool& value, std::string_view key, const message::Metadata& md);

// Domain types
bool parseEntry(Param& value, std::string_view key, const message::Metadata& md);
bool parseEntry(LevType& value, std::string_view key, const message::Metadata& md);
bool parseEntry(StatType& value, std::string_view key, const message::Metadata& md);

// Vector types
bool parseEntry(std::vector<std::int64_t>& value, std::string_view key, const message::Metadata& md);
bool parseEntry(std::vector<double>& value, std::string_view key, const message::Metadata& md);


// Explicit overload for optional<StatType> to avoid default-constructing StatType (which has no default ctor)
bool parseEntry(std::optional<StatType>& value, std::string_view key, const message::Metadata& md);

// std::optional<T> -- parse inner type, wrap in optional
template <typename T>
bool parseEntry(std::optional<T>& value, std::string_view key, const message::Metadata& md) {
    T inner;
    if (parseEntry(inner, key, md)) {
        value = std::move(inner);
        return true;
    }
    return false;
}


//----------------------------------------------------------------------------------------------------------------------
// Type-specific dumpEntry overloads for message::Metadata
//----------------------------------------------------------------------------------------------------------------------

void dumpEntry(const std::string& value, std::string_view key, message::Metadata& md);
void dumpEntry(std::int64_t value, std::string_view key, message::Metadata& md);
void dumpEntry(double value, std::string_view key, message::Metadata& md);
void dumpEntry(bool value, std::string_view key, message::Metadata& md);

// Domain types
void dumpEntry(const Param& value, std::string_view key, message::Metadata& md);
void dumpEntry(LevType value, std::string_view key, message::Metadata& md);
void dumpEntry(const StatType& value, std::string_view key, message::Metadata& md);

// Vector types
void dumpEntry(const std::vector<std::int64_t>& value, std::string_view key, message::Metadata& md);
void dumpEntry(const std::vector<double>& value, std::string_view key, message::Metadata& md);


// std::optional<T> -- only dump if value is set
template <typename T>
void dumpEntry(const std::optional<T>& value, std::string_view key, message::Metadata& md) {
    if (value.has_value()) {
        dumpEntry(*value, key, md);
    }
}


//----------------------------------------------------------------------------------------------------------------------
// Type-specific dumpConfigEntry overloads for eckit::LocalConfiguration
//----------------------------------------------------------------------------------------------------------------------

void dumpConfigEntry(const std::string& value, const std::string& key, eckit::LocalConfiguration& conf);
void dumpConfigEntry(std::int64_t value, const std::string& key, eckit::LocalConfiguration& conf);
void dumpConfigEntry(double value, const std::string& key, eckit::LocalConfiguration& conf);
void dumpConfigEntry(bool value, const std::string& key, eckit::LocalConfiguration& conf);

// Domain types
void dumpConfigEntry(const Param& value, const std::string& key, eckit::LocalConfiguration& conf);
void dumpConfigEntry(LevType value, const std::string& key, eckit::LocalConfiguration& conf);
void dumpConfigEntry(const StatType& value, const std::string& key, eckit::LocalConfiguration& conf);

// Vector types
void dumpConfigEntry(const std::vector<std::int64_t>& value, const std::string& key, eckit::LocalConfiguration& conf);
void dumpConfigEntry(const std::vector<double>& value, const std::string& key, eckit::LocalConfiguration& conf);


// std::optional<T> -- only dump if value is set
template <typename T>
void dumpConfigEntry(const std::optional<T>& value, const std::string& key, eckit::LocalConfiguration& conf) {
    if (value.has_value()) {
        dumpConfigEntry(*value, key, conf);
    }
}


//----------------------------------------------------------------------------------------------------------------------
// Field-level dispatch (required vs optional)
//----------------------------------------------------------------------------------------------------------------------

template <typename TStruct, typename TValue>
void parseField(const Entry<TStruct, TValue>& entry, TStruct& result, const message::Metadata& md) {
    if (entry.required) {
        if (!parseEntry(entry.get(result), entry.key, md)) {
            throw eckit::UserError{"Required metadata entry '" + std::string(entry.key) + "' is missing", Here()};
        }
    }
    else {
        parseEntry(entry.get(result), entry.key, md);
    }
}

template <typename TStruct, typename TValue>
void dumpField(const Entry<TStruct, TValue>& entry, const TStruct& source, message::Metadata& md) {
    dumpEntry(entry.get(source), entry.key, md);
}

template <typename TStruct, typename TValue>
void dumpConfigField(const Entry<TStruct, TValue>& entry, const TStruct& source, eckit::LocalConfiguration& conf,
                     std::string_view stripPrefix) {
    std::string key{entry.key};
    if (!stripPrefix.empty() && key.substr(0, stripPrefix.size()) == stripPrefix) {
        key = key.substr(stripPrefix.size());
    }
    dumpConfigEntry(entry.get(source), key, conf);
}


//----------------------------------------------------------------------------------------------------------------------
// Record-level operations
//----------------------------------------------------------------------------------------------------------------------

template <typename TStruct>
TStruct readMetadata(const message::Metadata& md) {
    static_assert(HasFieldsMember_v<TStruct>, "TStruct must have a static fields_ member");
    TStruct result;
    std::apply([&](const auto&... field) { (parseField(field, result, md), ...); }, TStruct::fields_);
    return result;
}

template <typename TStruct>
void writeMetadata(const TStruct& source, message::Metadata& md) {
    static_assert(HasFieldsMember_v<TStruct>, "TStruct must have a static fields_ member");
    std::apply([&](const auto&... field) { (dumpField(field, source, md), ...); }, TStruct::fields_);
}

template <typename TStruct>
message::Metadata writeMetadata(const TStruct& source) {
    static_assert(HasFieldsMember_v<TStruct>, "TStruct must have a static fields_ member");
    message::Metadata md;
    std::apply([&](const auto&... field) { (dumpField(field, source, md), ...); }, TStruct::fields_);
    return md;
}

template <typename TStruct>
void writeConfig(const TStruct& source, eckit::LocalConfiguration& conf, std::string_view stripPrefix = "") {
    static_assert(HasFieldsMember_v<TStruct>, "TStruct must have a static fields_ member");
    std::apply([&](const auto&... field) { (dumpConfigField(field, source, conf, stripPrefix), ...); },
               TStruct::fields_);
}

template <typename TStruct>
eckit::LocalConfiguration writeConfig(const TStruct& source, std::string_view stripPrefix = "") {
    static_assert(HasFieldsMember_v<TStruct>, "TStruct must have a static fields_ member");
    eckit::LocalConfiguration conf;
    std::apply([&](const auto&... field) { (dumpConfigField(field, source, conf, stripPrefix), ...); },
               TStruct::fields_);
    return conf;
}


}  // namespace multio::datamod::detail

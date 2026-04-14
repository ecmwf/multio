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

#include "multio/config/ComponentConfiguration.h"
#include "multio/util/record/Entry.h"


namespace multio::util::config::detail {

// Re-export shared types from util::record under the config::detail namespace for backward compatibility
using multio::util::record::containsKey;
using multio::util::record::Entry;
using multio::util::record::HasFieldsMember;
using multio::util::record::HasFieldsMember_v;

//----------------------------------------------------------------------------------------------------------------------

template <typename TEnum, std::enable_if_t<std::is_enum_v<TEnum>, bool> = true>
struct EnumTrait;

//----------------------------------------------------------------------------------------------------------------------
// parseEntry overloads — basic types
//----------------------------------------------------------------------------------------------------------------------

bool parseEntry(std::string& value, const std::string& key, const eckit::LocalConfiguration& localConfig);
bool parseEntry(std::int64_t& value, const std::string& key, const eckit::LocalConfiguration& localConfig);
bool parseEntry(double& value, const std::string& key, const eckit::LocalConfiguration& localConfig);
bool parseEntry(bool& value, const std::string& key, const eckit::LocalConfiguration& localConfig);

template <typename T, std::enable_if_t<std::is_enum_v<T>, bool> = true>
bool parseEntry(T& value, const std::string& key, const eckit::LocalConfiguration& localConfig) {
    if (!localConfig.has(key)) {
        return false;
    }
    if (!localConfig.isString(key)) {
        throw eckit::UserError{"Could not convert value of key '" + key + "' to enum value : value must be a string",
                               Here()};
    }
    const auto configValue = localConfig.getString(key);
    for (const auto& pair : EnumTrait<T>::values) {
        if (pair.second == configValue) {
            value = pair.first;
            return true;
        }
    }

    size_t i = 0;
    std::ostringstream oss;
    for (const auto& pair : EnumTrait<T>::values) {
        if (i++ > 0) {
            oss << ", ";
        }
        oss << "'" << pair.second << "'";
    }
    throw eckit::UserError{
        "Could not convert '" + configValue + "' to enum value : allowed values are: [" + oss.str() + "]", Here()};
}

template <typename TConfig, std::enable_if_t<HasFieldsMember_v<TConfig>, bool> = true>
bool parseEntry(std::vector<TConfig>& vector, const std::string& key, const eckit::LocalConfiguration& localConfig);

//----------------------------------------------------------------------------------------------------------------------
// parseEntry — optional / required / Entry wrappers
//----------------------------------------------------------------------------------------------------------------------

template <typename T>
bool parseEntry(std::optional<T>& value, const std::string& key, const eckit::LocalConfiguration& localConfig) {
    T result;
    if (parseEntry(result, key, localConfig)) {
        value = result;
        return true;
    }
    return false;
}

template <typename T>
void parseRequiredEntry(T& value, const std::string& key, const eckit::LocalConfiguration& localConfig) {
    if (!parseEntry(value, key, localConfig)) {
        throw eckit::UserError{"Required entry '" + key + "' is missing from the configuration", Here()};
    }
}

template <typename T>
void parseOptionalEntry(T& value, const std::string& key, const eckit::LocalConfiguration& localConfig) {
    parseEntry(value, key, localConfig);
}

template <typename TConfig, typename TValue>
void parseEntry(const Entry<TConfig, TValue>& entry, TConfig& config, const eckit::LocalConfiguration& localConfig) {
    if (entry.required) {
        parseRequiredEntry(entry.get(config), std::string{entry.key}, localConfig);
    }
    else {
        parseOptionalEntry(entry.get(config), std::string{entry.key}, localConfig);
    }
}

//----------------------------------------------------------------------------------------------------------------------
// parseConfig / parseActionConfig — placed AFTER all parseEntry declarations so that
// ordinary lookup at template definition time finds the Entry-based parseEntry overload.
// (Entry is now defined in util::record, so ADL alone would not find config::detail::parseEntry.)
//----------------------------------------------------------------------------------------------------------------------

template <typename TConfig>
std::string configKeysString() {
    return std::apply([&](const auto&... field) { return ((std::string(field.key) + ", ") + ... + ""); },
                      TConfig::fields_);
}

template <typename TConfig>
TConfig parseConfig(const eckit::LocalConfiguration& localConfig) {
    for (const auto& key : localConfig.keys()) {
        if (!containsKey<TConfig>(key)) {
            size_t i = 0;
            std::ostringstream oss;
            for (const auto& key : localConfig.keys()) {
                if (i++ > 0) {
                    oss << ", ";
                }
                oss << "'" << key << "'";
            }
            throw eckit::UserError{"Found unknown key '" + key + "' in the configuration, allowed keys are: ["
                                       + configKeysString<TConfig>() + "]. Contained keys: [" + oss.str() + "]",
                                   Here()};
        }
    }

    TConfig config;
    std::apply([&](const auto&... field) { (parseEntry(field, config, localConfig), ...); }, config.fields_);
    return config;
}

template <typename TConfig>
TConfig parseActionConfig(const multio::config::ComponentConfiguration& componentConfig) {
    auto localConfig = componentConfig.parsedConfig();
    localConfig.remove("type");
    localConfig.remove("next");
    return parseConfig<TConfig>(localConfig);
}

//----------------------------------------------------------------------------------------------------------------------
// parseEntry for vector of sub-configs — needs parseConfig which is now defined above
//----------------------------------------------------------------------------------------------------------------------

template <typename TConfig, std::enable_if_t<HasFieldsMember_v<TConfig>, bool>>
bool parseEntry(std::vector<TConfig>& vector, const std::string& key, const eckit::LocalConfiguration& localConfig) {
    if (!localConfig.has(key)) {
        return false;
    }
    if (localConfig.isSubConfigurationList(key)) {
        vector.clear();
        for (const auto& c : localConfig.getSubConfigurations(key)) {
            vector.emplace_back(parseConfig<TConfig>(c));
        }
        return true;
    }
    throw eckit::UserError{"Could not convert value of key '" + key + "' to vector : no conversion method defined",
                           Here()};
}


}  // namespace multio::util::config::detail

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

#include <string_view>

#include "eckit/config/LocalConfiguration.h"
#include "multio/config/ComponentConfiguration.h"

#include "detail/Parser.h"


namespace multio::util::config {


/**
 * Register a required field in the configuration
 * @param key String value of this field
 * @param value Location where parsed value will be stored
 */
template <typename TConfig, typename TValue>
constexpr detail::Entry<TConfig, TValue> requiredEntry(const std::string_view& key, TValue TConfig::* value) {
    return {key, value, true};
}

/**
 * Register an optional (or defaulted) field in the configuration
 * @param key String value of this field
 * @param value Location where parsed value will be stored
 */
template <typename TConfig, typename TValue>
constexpr detail::Entry<TConfig, TValue> optionalEntry(const std::string_view& key, TValue TConfig::* value) {
    return {key, value, false};
}

/**
 * Create a parsed configuration object from the eckit::LocalConfiguration
 * @param localConfig An unparsed eckit::LocalConfiguration
 * @returns A parsed configuration of the desired type
 * @throws If an unknown key is present in the localConfig, or if a value cannot be parsed
 */
template <typename TConfig>
TConfig parseConfig(const eckit::LocalConfiguration& localConfig) {
    return detail::parseConfig<TConfig>(localConfig);
}

/**
 * Create a parsed configuration object from the multio::config::ComponentConfiguration
 * This method ignores the keys "type" and "next" that are present in action configurations
 * @param componentConfig An unparsed multio::config::ComponentConfiguration
 * @returns A parsed configuration of the desired type
 * @throws If an unknown key is present in the localConfig, or if a value cannot be parsed
 */
template <typename TConfig>
TConfig parseActionConfig(const multio::config::ComponentConfiguration& componentConfig) {
    return detail::parseActionConfig<TConfig>(componentConfig);
}


}  // namespace multio::util::config

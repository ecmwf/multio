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
#include "multio/util/record/Entry.h"


namespace multio::util::config {

// Re-export shared record entry factories for backward compatibility
using multio::util::record::optionalEntry;
using multio::util::record::requiredEntry;

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

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

#include <string>

#include "eckit/config/LocalConfiguration.h"
#include "eckit/exception/Exceptions.h"

#include "Entry.h"


namespace multio::util::config::detail {


template <typename TConfig>
TConfig parseConfig(const eckit::LocalConfiguration& localConfig) {
    TConfig config;
    std::set<std::string> allowedKeys;

    std::apply(
        [&](const auto&... field) {
            (parseEntry(field, config, localConfig), ...);
            (allowedKeys.emplace(field.key), ...);
        },
        config.fields_);

    for (const auto& key : localConfig.keys()) {
        if (allowedKeys.find(key) == allowedKeys.end()) {
            size_t i = 0;
            std::ostringstream oss;
            for (const auto& key : localConfig.keys()) {
                if (i++ > 0) {
                    oss << ", ";
                }
                oss << "'" << key << "'";
            }
            throw eckit::UserError{"Found unknown key '" + key + "' in the configuration, allowed keys are: ["
                                   + oss.str() + "]"};
        }
    }

    return config;
}


}  // namespace multio::util::config::detail

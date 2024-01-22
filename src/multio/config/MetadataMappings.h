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

/// @date Aug 2022

#pragma once

#include "eckit/config/LocalConfiguration.h"

#include "multio/message/MetadataMapping.h"

#include <functional>
#include <memory>
#include <unordered_map>


namespace multio::config {

class MultioConfiguration;  // Forward declaration
struct ConfigFile;          // Forward declaration

class MetadataMappings {
public:
    const std::vector<message::MetadataMapping>& getMappings(const MultioConfiguration& multioConf,
                                                             const std::string& mapping) const;

private:
    // const YAMLFile& configFile_;
    mutable std::unordered_map<std::string, std::unique_ptr<std::vector<message::MetadataMapping>>> mappings_;
};

}  // namespace multio::config

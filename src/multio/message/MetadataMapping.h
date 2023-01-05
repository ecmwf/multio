
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

/// @date Sep 2022

#pragma once

#include <unordered_map>

#include "eckit/config/LocalConfiguration.h"
#include "eckit/utils/Optional.h"

#include "multio/message/Metadata.h"

namespace multio {
namespace message {

struct MetadataMappingOptions {
    bool enforceMatch;
    bool overwriteExisting;
};


class MetadataMapping {
public:
    MetadataMapping(const std::string& sourceKey, const eckit::LocalConfiguration& mappings,
                    const eckit::LocalConfiguration& optionalMappings,
                    const std::vector<eckit::LocalConfiguration>& sourceList, const std::string& targetKey,
                    const eckit::Optional<std::string>& targetPath = eckit::Optional<std::string>{});
    MetadataMapping(const std::string& sourceKey, const eckit::LocalConfiguration& mappings,
                    const eckit::LocalConfiguration& optionalMappings,
                    const std::unordered_map<std::string, eckit::LocalConfiguration>& source,
                    const eckit::Optional<std::string>& targetPath = eckit::Optional<std::string>{});
    MetadataMapping(const std::string& sourceKey, const eckit::LocalConfiguration& mappings,
                    const eckit::LocalConfiguration& optionalMappings,
                    std::unordered_map<std::string, eckit::LocalConfiguration>&& source,
                    const eckit::Optional<std::string>& targetPath = eckit::Optional<std::string>{});

    void applyInplace(Metadata&, MetadataMappingOptions options = MetadataMappingOptions{}) const;

    Metadata apply(Metadata&&, MetadataMappingOptions options = MetadataMappingOptions{}) const;
    Metadata apply(const Metadata&, MetadataMappingOptions options = MetadataMappingOptions{}) const;

private:
    std::string sourceKey_;
    eckit::LocalConfiguration mapping_;
    eckit::LocalConfiguration optionalMapping_;
    std::unordered_map<std::string, eckit::LocalConfiguration> source_;
    eckit::Optional<std::string> targetPath_;
};


}  // namespace message
}  // namespace multio

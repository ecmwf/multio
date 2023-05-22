
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
#include "eckit/exception/Exceptions.h"
#include "eckit/utils/Optional.h"

#include "multio/message/Metadata.h"

namespace multio {
namespace message {

//=====================================================================================================================

struct MetadataMappingOptions {
    bool enforceMatch;
    bool overwriteExisting;
};


//=====================================================================================================================


/**
 * A metadata mapping is performing changes on metadata by matching a key in the metadata with a key in a custom (user)
 * provided map. As an example you may have a look in `tests/multio/config/metadata-mapping/nemo-to-grib.yaml`:
 * ```
 *  mappings:
 *   - match:
 *       nemoParam: nemo-id
 *     map:
 *       param: param-id
 *       gridSubtype: grid-type
 *       domain: grid-type
 *       typeOfLevel: level-type
 *
 *   data:
 *     # Co-ordinates
 *     - nemo-id: lat_T
 *       param-id: 250003
 *       grid-type : "T grid"
 *       level-type : "oceanSurface"
 *
 *     - nemo-id: lon_T
 *       param-id: 250004
 *       grid-type : "T grid"
 *       level-type : "oceanSurface"
 * ```
 *
 * Moveover this mapping mechanism is also used to customised encoding for specific params.
 * This is achieved by conditionally performing mappings (for example on "param") and storing them at a sub dictionary
 * in the metadata (with target path "encoder-overwrites").
 */
class MetadataMapping {
public:
    MetadataMapping(const std::string& metadataKey, const eckit::LocalConfiguration& mappings,
                    const eckit::LocalConfiguration& optionalMappings,
                    const std::vector<eckit::LocalConfiguration>& mapDataList, const std::string& matchKey,
                    const eckit::Optional<std::string>& targetPath = eckit::Optional<std::string>{});

    MetadataMapping(const std::string& metadataKey, const eckit::LocalConfiguration& mappings,
                    const eckit::LocalConfiguration& optionalMappings,
                    const std::unordered_map<std::string, eckit::LocalConfiguration>& source,
                    const eckit::Optional<std::string>& targetPath = eckit::Optional<std::string>{});

    MetadataMapping(const std::string& metadataKey, const eckit::LocalConfiguration& mappings,
                    const eckit::LocalConfiguration& optionalMappings,
                    std::unordered_map<std::string, eckit::LocalConfiguration>&& source,
                    const eckit::Optional<std::string>& targetPath = eckit::Optional<std::string>{});


    void applyInplace(Metadata&, MetadataMappingOptions options = MetadataMappingOptions{}) const;


    Metadata apply(Metadata&&, MetadataMappingOptions options = MetadataMappingOptions{}) const;

    Metadata apply(const Metadata&, MetadataMappingOptions options = MetadataMappingOptions{}) const;


private:
    std::string metadataKey_;                    // Describes the key to be looked for in the metadata
    eckit::LocalConfiguration mapping_;          // Description of a mapping.
    eckit::LocalConfiguration optionalMapping_;  // Description of a optional mapping.
    std::unordered_map<std::string, eckit::LocalConfiguration>
        mapData_;  // Input data on which the mapping is performed
    eckit::Optional<std::string>
        targetPath_;  // Optional key for a nested dictionary in the metadata at which the mapped data will be written
};


//=====================================================================================================================

class MetadataMappingException : public eckit::Exception {
public:
    MetadataMappingException(const std::string& reason, const eckit::CodeLocation& location = eckit::CodeLocation());
};

//=====================================================================================================================

}  // namespace message
}  // namespace multio

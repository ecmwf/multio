
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

#include "multio/message/Metadata.h"

#include <optional>

namespace multio::message {

//---------------------------------------------------------------------------------------------------------------------

struct MetadataMappingOptions {
    bool enforceMatch;
    bool overwriteExisting;
};


//---------------------------------------------------------------------------------------------------------------------


/**
 * A metadata mapping is performing changes on metadata by matching a key in the metadata with a key in a custom (user)
 * provided map. As an example you may have a look in `tests/multio/config/metadata-mapping/nemo-to-grib.yaml`:
 * ```
 *  mappings:
 *   - match:
 *       nemoParam: nemo-id
 *     map:
 *       param: param-id
 *       gridType: grid-type
 *       unstructuredGridSubtype: unstructured-grid-subtype
 *       domain: domain
 *       typeOfLevel: level-type
 *
 *   data:
 *     # Co-ordinates
 *     - nemo-id: lat_T
 *       param-id: 250003
 *       grid-type: "unstructured_grid"
 *       domain : "T grid"
 *       unstructured-grid-subtype : "T"
 *       level-type : "oceanSurface"
 *
 *     - nemo-id: lon_T
 *       param-id: 250004
 *       grid-type: "unstructured_grid"
 *       domain : "T grid"
 *       unstructured-grid-subtype : "T"
 *       level-type : "oceanSurface"
 * ```
 *
 * Moveover this mapping mechanism is also used to customised encoding for specific params.
 * This is achieved by conditionally performing mappings (for example on "param") and storing them at a sub dictionary
 * in the metadata (with target path "encoder-overwrites").
 */
class MetadataMapping {
public:
    using KeyType = typename MetadataTypes::KeyType;
    using MatchKeyType = KeyType;  // In the future we may want to support multiple keys for matching

    using KeyMapping = std::vector<std::pair<KeyType, KeyType>>;
    using DataMapping = std::unordered_map<MetadataValue, message::Metadata>;

    MetadataMapping(const MatchKeyType& metadataKey, const KeyMapping& mappings, const KeyMapping& optionalMappings,
                    const std::vector<eckit::LocalConfiguration>& mapDataList, const KeyType& matchKey,
                    const std::optional<KeyType>& targetPath = std::optional<KeyType>{});

    MetadataMapping(const MatchKeyType& metadataKey, const KeyMapping& mappings, const KeyMapping& optionalMappings,
                    const std::unordered_map<MetadataValue, eckit::LocalConfiguration>& source,
                    const std::optional<KeyType>& targetPath = std::optional<KeyType>{});

    MetadataMapping(const MatchKeyType& metadataKey, const DataMapping& mapping,
                    const std::optional<KeyType>& targetPath = std::optional<KeyType>{});
    MetadataMapping(const MatchKeyType& metadataKey, DataMapping&& mapping,
                    const std::optional<KeyType>& targetPath = std::optional<KeyType>{});


    void applyInplace(Metadata&, MetadataMappingOptions options = MetadataMappingOptions{}) const;


    Metadata apply(Metadata&&, MetadataMappingOptions options = MetadataMappingOptions{}) const;

    Metadata apply(const Metadata&, MetadataMappingOptions options = MetadataMappingOptions{}) const;


private:
    MatchKeyType metadataKey_;  // Describes the key to be looked for in the metadata
    DataMapping mapData_;       // Input data on which the mapping is performed
    std::optional<KeyType>
        targetPath_;  // Optional key for a nested dictionary in the metadata at which the mapped data will be written
};


//---------------------------------------------------------------------------------------------------------------------

class MetadataMappingException : public MetadataException {
public:
    MetadataMappingException(const std::string& reason, const eckit::CodeLocation& location = eckit::CodeLocation());
};

//---------------------------------------------------------------------------------------------------------------------

}  // namespace multio::message

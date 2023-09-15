/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/// @author Philipp Geier

/// @date August 2023

#pragma once

#include "multio/action/encode-grib2/GridInfo.h"
#include "multio/config/ComponentConfiguration.h"
#include "multio/grib2/cpp/MultioGrib2.h"
#include "multio/util/Hash.h"
#include "multio/util/MioGribHandle.h"
#include "multio/util/Result.h"


#include <optional>

namespace multio::action {

using multio::util::MioGribHandle;

namespace encodeGrib2 {


static const std::string PDT_KEY{"productDefinitionTemplateNumber"};
static const std::string LOCAL_DF_KEY{"localDefinitionNumber"};


static const std::string CONF_BASE_SAMPLE{"base-sample"};
static const std::string CONF_DOMAIN_OPTIONS{"domain-options"};

static const std::string CONF_ADD_METADATA{"additional-metadata"};

using DomainId = std::string;


struct DomainOptions : GridInfoCreationOptions {
    std::unique_ptr<MioGribHandle> baseSample;
    std::optional<message::Metadata> additionalMetadata;
};

struct SampleConfiguration {
    std::unique_ptr<MioGribHandle> baseSample;
    std::optional<message::Metadata> additionalMetadata;
    std::unordered_map<DomainId, DomainOptions> domainOptions;
};


struct WithMetadataOverwrites {
    message::Metadata metadataWithOverwrites;
};

struct PreparedSampleArguments : WithMetadataOverwrites {
    std::reference_wrapper<const MioGribHandle> sample;
    const GridInfoCreationOptions* gridInfoCreationOptions;
};


void sampleConfigurationFromConfiguration(SampleConfiguration&, const eckit::Configuration& conf);
SampleConfiguration sampleConfigurationFromConfiguration(const eckit::Configuration& conf);

void domainOptionsFromConfiguration(DomainOptions&, const eckit::Configuration& conf);
DomainOptions domainOptionsFromConfiguration(const eckit::Configuration& conf);


struct SampleKey {
    std::optional<DomainId> domain;
    std::int64_t productDefinitionTemplateNumber;
};

bool operator==(const SampleKey& lhs, const SampleKey& rhs) noexcept;

}  // namespace encodeGrib2

}  // namespace multio::action


template <>
struct std::hash<multio::action::encodeGrib2::SampleKey> {
    std::size_t operator()(const multio::action::encodeGrib2::SampleKey& t) const
        noexcept(noexcept(multio::util::hash_combine(t.domain, t.productDefinitionTemplateNumber))) {
        return multio::util::hash_combine(t.domain, t.productDefinitionTemplateNumber);
    }
};

namespace multio::action::encodeGrib2 {


class SampleManager {
public:
    util::Result<SampleKey> tryGetSampleKeyFromMetadata(const message::Metadata&) const noexcept;
    SampleKey sampleKeyFromMetadata(const message::Metadata&) const;


    using HandleToEncode = std::unique_ptr<MioGribHandle>;
    using HandlesToEncode = std::vector<HandleToEncode>;

    struct GridSample {
        GridInfo gridInfo;
        std::unique_ptr<MioGribHandle> sample;
    };

    struct InitDomainResult {
        // Created gridSample
        std::reference_wrapper<const GridSample> gridSample;

        // Possible additional handles (e.g. to encode coordinate information) that may be produced if no domain
        // information is given or a domain just has been initialized.
        std::optional<HandlesToEncode> encodeAdditionalHandles;
    };

    struct InitDomainResultWithMetadata : InitDomainResult, WithMetadataOverwrites {};

    /**
     * Domain initialization is allowed to send encoded coordinates
     *
     * Throws an EncodeGrib2Exception if information for sample preparation is incomplete.
     *
     * @param domainId    Name of the domain to initialize a grib sample for.
     * @param md          Metadata with grid related information for grib sample preparation.
     *                    Either atlas specific information (`atlasNamedGrid`) or eccodes specific information
     * (`gridType` + additional required grid dependent keys).
     * @return            Possible handles to encode coordinate information. May be used for unstructured grids if the
     * option `CONF_EXTRACT_LAT_LON` is set.
     */
    InitDomainResultWithMetadata initDomain(const std::string& domainId, const message::Metadata& md);

    struct HandleFieldResult : WithMetadataOverwrites {
        // Handle with all product information set - only data values are missing
        HandleToEncode encodeFieldHandle;

        // Possible additional handles (e.g. to encode coordinate information) that may be produced if no domain
        // information is given or a domain just has been initialized.
        std::optional<HandlesToEncode> encodeAdditionalHandles;
    };

    /**
     * Prepares or creates a grib sample according to the following scheme:
     *  - `domain` and `productDefinitionTemplate` given:
     *     Lookus for a prepared sample for both keys and use if given.
     *     Otherwise look for a sample with the domain.
     *      - If domain sample is given: prepare a product specific template and put it in the cache for further use
     *      - If no domain sample is given, try to create a new domain specific sample and a domain + product specific
     * sample and put it in the cache.
     *  - only `productDefinitionTemplate` given:
     *     Try to infer grid information from metadata with the same approach of `initDomain` and create a sample
     *
     * Once a sample has been created or loaded from cache,
     * the metadata is checked for a set of product specific keys that may be set on that given template.
     *
     * Throws an EncodeGrib2Exception if information for sample preparation is incomplete.
     *
     * @param sampleKey  Sampyle key with optional domain and inferred productDefinitionTemplateNumber.
     * @param md         Metadata with product and optional grid information
     * @return           Returns at least one handle with prepared product information and possible additional handles.
     */
    HandleFieldResult handleField(const SampleKey&, const message::Metadata&);

    /**
     * Retrieves a list of possible product-related keys that can be set on a grib2 product template.
     * The keys are iterated, checked in the metadata and possibly set on the grib handle.
     * The possible types a value for a specific key can take are checked as well.
     * Moreover first eccodes metakeys are checked and the low-level grib2 keys are just checked if a meta key is not
     *existing.
     *
     * @param sampleKey  Key with evaluated product and possibly domain categorization
     *                   that has been retrieved through `sampleKeyFromMetadata(const message::Metadata&)`.
     * @param metadata   Metadata with possible overwrites from `handleField` or `initDomain`.
     * @param gribHandle Handle on which keys to set on.
     **/
    void transferProductKeys(const SampleKey& sampleKey, const message::Metadata& metadata,
                             MioGribHandle& gribHandle) const;


    SampleManager(const config::ComponentConfiguration&);

protected:
    InitDomainResult initDomain(const std::string& domainId, const PreparedSampleArguments& prepArgs);

    GridSample createGridSample(const PreparedSampleArguments& prepArgs);

    // Extract lon lat information, create handles and free memory such that lonlats can only be extracted once
    std::optional<HandlesToEncode> extractLonLat(GridSample&);

    PreparedSampleArguments prepareSampleArguments(const std::optional<DomainId>&, const message::Metadata&);

private:
    SampleConfiguration sampleConfiguration_;
    multio::grib2::Grib2ProductHandler<std::string> grib2ProductHandler_;
    std::unordered_map<DomainId, GridSample> gridSamples_;
    std::unordered_map<SampleKey, std::unique_ptr<MioGribHandle>> samples_;
};


}  // namespace multio::action::encodeGrib2

/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "SampleManager.h"
#include "Exception.h"

#include "eckit/io/StdFile.h"
#include "eckit/util/Overloaded.h"

#include "eccodes.h"

#include <sstream>


namespace multio::action::encodeGrib2 {

bool operator==(const SampleKey& lhs, const SampleKey& rhs) noexcept {
    return (lhs.productDefinitionTemplateNumber == rhs.productDefinitionTemplateNumber) && (lhs.domain == rhs.domain);
}

std::unique_ptr<MioGribHandle> loadGribSample(const std::string& path,
                                              const config::MultioConfiguration& multioConfig) {
    // TODO provide utility to distinguish between relative and absolute paths
    eckit::AutoStdFile fin{multioConfig.replaceCurly(path)};
    int err;
    return std::make_unique<MioGribHandle>(codes_handle_new_from_file(nullptr, fin, PRODUCT_GRIB, &err));
}

void domainOptionsFromConfiguration(DomainOptions& domainOptions, const eckit::Configuration& conf,
                                    const config::MultioConfiguration& multioConfig) {
    gridInfoCreationOptionsFromConfiguration(domainOptions, conf);

    if (conf.has(CONF_BASE_SAMPLE)) {
        domainOptions.baseSample = loadGribSample(conf.getString(CONF_BASE_SAMPLE), multioConfig);
    }

    if (conf.has(CONF_ADD_METADATA)) {
        domainOptions.additionalMetadata = message::toMetadata(conf.getSubConfiguration(CONF_ADD_METADATA).get());
    }
}

DomainOptions domainOptionsFromConfiguration(const eckit::Configuration& conf,
                                             const config::MultioConfiguration& multioConfig) {
    DomainOptions domainOptions;
    domainOptionsFromConfiguration(domainOptions, conf, multioConfig);
    return domainOptions;
}

void sampleConfigurationFromConfiguration(SampleConfiguration& sampleConf, const eckit::Configuration& conf,
                                          const config::MultioConfiguration& multioConfig) {
    if (conf.has(CONF_BASE_SAMPLE)) {
        sampleConf.baseSample = loadGribSample(conf.getString(CONF_BASE_SAMPLE), multioConfig);
    }
    else {
        std::ostringstream oss;
        oss << "The encoder configuration is expected to have a field \"" << CONF_BASE_SAMPLE
            << "\" with a path to grib sample.";
        throw EncodeGrib2Exception(oss.str(), Here());
    }

    if (conf.has(CONF_ADD_METADATA)) {
        sampleConf.additionalMetadata = message::toMetadata(conf.getSubConfiguration(CONF_ADD_METADATA).get());
    }

    if (conf.has(CONF_DOMAIN_OPTIONS)) {
        auto domMap = conf.getSubConfiguration(CONF_DOMAIN_OPTIONS);
        for (auto&& k : domMap.keys()) {
            sampleConf.domainOptions.emplace(
                k, domainOptionsFromConfiguration(domMap.getSubConfiguration(k), multioConfig));
        }
    }
}

SampleConfiguration sampleConfigurationFromConfiguration(const eckit::Configuration& conf,
                                                         const config::MultioConfiguration& multioConfig) {
    SampleConfiguration sampleConf;
    sampleConfigurationFromConfiguration(sampleConf, conf, multioConfig);
    return sampleConf;
}

SampleManager::SampleManager(const config::ComponentConfiguration& conf) :
    sampleConfiguration_{sampleConfigurationFromConfiguration(conf.parsedConfig(), conf.multioConfig())} {};


util::Result<SampleKey> SampleManager::tryGetSampleKeyFromMetadata(const message::Metadata& md) const noexcept {
    return visit(eckit::Overloaded{
                     [&md](std::int64_t pdt) -> util::Result<SampleKey> {
                         return SampleKey{md.getOpt<std::string>(DOMAIN_KEY), pdt};
                     },
                     [](multio::grib2::Error&& err) -> util::Result<SampleKey> { return util::ErrorMessage{err.msg}; },
                 },
                 grib2ProductHandler_.inferProductDefinitionTemplateNumber(
                     [&md](auto&& strKey) { return md.getOpt<std::string>(std::forward<decltype(strKey)>(strKey)); }));
}

SampleKey SampleManager::sampleKeyFromMetadata(const message::Metadata& md) const {
    return tryGetSampleKeyFromMetadata(md).valueOrHandleErr([](util::ErrorMessage&& err) -> SampleKey {
        std::ostringstream oss;
        oss << "sampleKeyFromMetadata - Error while inferring a product definition template: " << std::move(err.msg)
            << std::endl;
        throw EncodeGrib2Exception(oss.str(), Here());
    });
};


SampleManager::GridSample SampleManager::createGridSample(const PreparedSampleArguments& prepArgs) {
    GridInfo gridInfo = gridInfoFromMetadata(
        prepArgs.gridInfoCreationOptions ? *prepArgs.gridInfoCreationOptions : GridInfoCreationOptions{},
        prepArgs.metadataWithOverwrites);
    auto baseSample = prepArgs.sample.get().duplicate();
    codesKeySetter(gridInfo, [&baseSample](auto&& strVal, auto&& val) {
        baseSample->setValue(std::forward<decltype(strVal)>(strVal), std::forward<decltype(val)>(val));
    });
    return GridSample{std::move(gridInfo), std::move(baseSample)};
}


std::optional<SampleManager::HandlesToEncode> SampleManager::extractLonLat(GridSample& gridSample) {
    return std::visit(
        eckit::Overloaded{[&gridSample](UnstructuredGridInfo& gridInfo) -> std::optional<SampleManager::HandlesToEncode> {
                       if (!gridInfo.lonLat) {
                           return std::optional<SampleManager::HandlesToEncode>{};
                       }
                       auto sampleLon = gridSample.sample->duplicate();
                       sampleLon->setValue("paramId", gridInfo.lonLat->paramIdLon);
                       sampleLon->setDataValues(gridInfo.lonLat->lon);

                       auto sampleLat = gridSample.sample->duplicate();
                       sampleLat->setValue("paramId", gridInfo.lonLat->paramIdLat);
                       sampleLat->setDataValues(gridInfo.lonLat->lat);

                       // Extraction happens only once - free memory
                       gridInfo.lonLat = std::nullopt;

                       SampleManager::HandlesToEncode res;
                       res.emplace_back(std::move(sampleLon));
                       res.emplace_back(std::move(sampleLat));
                       return res;
                   },
                   [](auto& gridInfo) { return std::optional<SampleManager::HandlesToEncode>{}; }},
        gridSample.gridInfo);
}


PreparedSampleArguments SampleManager::prepareSampleArguments(const std::optional<DomainId>& domainId,
                                                              const message::Metadata& md) {
    auto searchDomainOptions
        = domainId ? sampleConfiguration_.domainOptions.find(*domainId) : sampleConfiguration_.domainOptions.end();
    bool hasDomainOptions = searchDomainOptions != sampleConfiguration_.domainOptions.end();

    const auto& sample = hasDomainOptions && searchDomainOptions->second.baseSample
                           ? *searchDomainOptions->second.baseSample.get()
                           : *sampleConfiguration_.baseSample.get();

    message::Metadata metadataWithOverwrites{md};
    if (sampleConfiguration_.additionalMetadata) {
        metadataWithOverwrites.update(*sampleConfiguration_.additionalMetadata);
    }
    if (hasDomainOptions && searchDomainOptions->second.additionalMetadata) {
        metadataWithOverwrites.update(*(searchDomainOptions->second.additionalMetadata));
    }


    return PreparedSampleArguments{{std::move(metadataWithOverwrites)},
                                   std::cref(sample),
                                   hasDomainOptions ? &(searchDomainOptions->second) : nullptr};
};

SampleManager::InitDomainResult SampleManager::initDomain(const std::string& domainId,
                                                          const PreparedSampleArguments& prepArgs) {
    auto options = prepArgs.gridInfoCreationOptions ? *prepArgs.gridInfoCreationOptions : GridInfoCreationOptions{};
    auto [it, hasBeenInserted] = gridSamples_.try_emplace(domainId, createGridSample(prepArgs));

    if (!hasBeenInserted) {
        throw EncodeGrib2Exception(std::string("SampleManager::initDomain: A domain named \"") + domainId
                                       + std::string("\" has already been initialized"),
                                   Here());
    }

    if (!options.extractLonLatFromUnstructuredGrid) {
        return SampleManager::InitDomainResult{std::cref(it->second), {}};
    }

    return SampleManager::InitDomainResult{std::cref(it->second), extractLonLat(it->second)};
}

SampleManager::InitDomainResultWithMetadata SampleManager::initDomain(const std::string& domainId,
                                                                      const message::Metadata& md) {
    PreparedSampleArguments prepArgs = prepareSampleArguments(domainId, md);
    return SampleManager::InitDomainResultWithMetadata{initDomain(domainId, prepArgs),
                                                       {std::move(prepArgs.metadataWithOverwrites)}};
}

namespace {


template <typename T, typename OnError>
T getResult(multio::grib2::Result<T>&& result, OnError&& onError) {
    return std::visit(eckit::Overloaded{[&](T&& resVal) -> T { return resVal; },
                                 [&](multio::grib2::Error&& error) -> T {
                                     return std::invoke(std::forward<OnError>(onError), std::move(error));
                                 }},
                      std::move(result));
}

template <typename Key>
const auto& getPdtKeyList(const multio::grib2::Grib2ProductHandler<Key>& grib2ProductHandler,
                          const SampleKey& sampleKey) {
    return getResult(grib2ProductHandler.keysForPdt(sampleKey.productDefinitionTemplateNumber),
                     [&](multio::grib2::Error&& error)
                         -> std::variant_alternative_t<0, std::decay_t<decltype(grib2ProductHandler.keysForPdt(
                                                              sampleKey.productDefinitionTemplateNumber))>> {
                         std::ostringstream oss;
                         oss << "SampleManager::handleField - Error while retrieving keys for "
                                "productDefinitionTemplateNumber "
                             << sampleKey.productDefinitionTemplateNumber << ": " << error.msg;
                         throw EncodeGrib2Exception(oss.str(), Here());
                     })
        .get();
}

template <typename Key>
const auto& getKeyInfo(const multio::grib2::Grib2ProductHandler<Key>& grib2ProductHandler, const Key& key) {
    return getResult(
               grib2ProductHandler.keyInfoForKey(key),
               [&](multio::grib2::Error&& error)
                   -> std::variant_alternative_t<0, std::decay_t<decltype(grib2ProductHandler.keyInfoForKey(key))>> {
                   std::ostringstream oss;
                   oss << "SampleManager::handleField - Error while retrieving key info for key " << key << ": "
                       << error.msg;
                   throw EncodeGrib2Exception(oss.str(), Here());
               })
        .get();
}


template <typename Key>
bool lookupAndSetKey(const Key& key, const std::set<multio::grib2::KeyTypes>& allowedKeyTypes,
                     const message::Metadata& md, MioGribHandle& h) {
    auto searchKey = md.find(key);
    if (searchKey == md.end()) {
        return false;
    }

    auto throwNotOfType = [&](const std::string& keyTypeString, const auto& v) {
        std::ostringstream oss;
        oss << "SampleManager lookupAndSetKey - Key \"" << key << "\" is not expected to be of type \"" << keyTypeString
            << "\". Value: " << v;
        throw EncodeGrib2Exception(oss.str(), Here());
    };

    searchKey->second.visit(eckit::Overloaded{
        [&](const auto& v) -> util::IfTypeOf<decltype(v), message::MetadataIntegerTypes> {
            if (allowedKeyTypes.find(multio::grib2::KeyTypes::IntType) == allowedKeyTypes.end()) {
                throwNotOfType("IntType", v);
            };
            h.setValue(key, v);
        },
        [&](const auto& v) -> util::IfTypeOf<decltype(v), message::MetadataFloatingTypes> {
            if (allowedKeyTypes.find(multio::grib2::KeyTypes::FloatType) == allowedKeyTypes.end()) {
                throwNotOfType("FloatType", v);
            };
            h.setValue(key, v);
        },
        [&](const auto& v) -> util::IfTypeOf<decltype(v), message::MetadataStringTypes> {
            if (allowedKeyTypes.find(multio::grib2::KeyTypes::StringType) == allowedKeyTypes.end()) {
                throwNotOfType("StringType", v);
            };
            h.setValue(key, v);
        },
        [&](const auto& v) -> util::IfTypeOf<decltype(v), message::MetadataIntegerVectorTypes> {
            if (allowedKeyTypes.find(multio::grib2::KeyTypes::IntArrayType) == allowedKeyTypes.end()) {
                throwNotOfType("IntArrayType", v);
            };
            h.setValue(key, v);
        },
        [&](const auto& v) -> util::IfTypeOf<decltype(v), message::MetadataFloatingVectorTypes> {
            if (allowedKeyTypes.find(multio::grib2::KeyTypes::FloatArrayType) == allowedKeyTypes.end()) {
                throwNotOfType("FloatArrayType", v);
            };
            h.setValue(key, v);
        },
        [&](const auto& v) -> util::IfTypeOf<decltype(v), message::MetadataStringVectorTypes> {
            if (allowedKeyTypes.find(multio::grib2::KeyTypes::StringArrayType) == allowedKeyTypes.end()) {
                throwNotOfType("StringArrayType", v);
            };
            h.setValue(key, v);
        },
        [&](const auto& v) -> util::IfTypeOf<decltype(v), message::MetadataNullTypes> { throwNotOfType("<Null>", v); },
        [&](const auto& v) -> util::IfTypeOf<decltype(v), message::MetadataNestedTypes> {
            throwNotOfType("<Nested Metadata>", v);
        }});

    return true;
}

template <typename Key, typename KeyList>
void lookupAndSetKeys(const multio::grib2::Grib2ProductHandler<Key>& grib2ProductHandler, const KeyList& keyList,
                      const message::Metadata& md, MioGribHandle& h, std::set<Key>& alreadyCheckedKeys) {
    for (const auto& k : keyList) {
        if (!alreadyCheckedKeys.insert(k).second) {
            std::ostringstream oss;
            oss << "SampleManager lookupAndSetKey - Grib2 key \"" << k
                << "\" has already been checked - Maybe there are cyclic dependencies in the keylists? Please report "
                   "to an maintainer.";
            throw EncodeGrib2Exception(oss.str(), Here());
        }
        const auto& keyInfo = getKeyInfo(grib2ProductHandler, k);
        if (!lookupAndSetKey(k, keyInfo.types, md, h) && keyInfo.alternativeKeys) {
            lookupAndSetKeys(grib2ProductHandler, *(keyInfo.alternativeKeys), md, h, alreadyCheckedKeys);
        }
    }
}

template <typename Key, typename KeyList>
void lookupAndSetKeys(const multio::grib2::Grib2ProductHandler<Key>& grib2ProductHandler, const KeyList& keyList,
                      const message::Metadata& md, MioGribHandle& h) {
    std::set<Key> alreadyCheckedKeys;
    lookupAndSetKeys(grib2ProductHandler, keyList, md, h, alreadyCheckedKeys);
}

}  // namespace

SampleManager::HandleFieldResult SampleManager::handleField(const SampleKey& sampleKey, const message::Metadata& md) {
    if (sampleKey.domain) {
        PreparedSampleArguments prepArgs = prepareSampleArguments(*sampleKey.domain, md);

        // Search for existing sample
        auto searchSample = samples_.find(sampleKey);
        if (searchSample == samples_.end()) {
            std::optional<SampleManager::HandlesToEncode> encodeAdditionalHandles;
            // Prepare a new sample
            const GridSample& gridSample = std::invoke([&]() -> const GridSample& {
                // Check if a grid sample already exists or lazy initialize
                auto searchGridSample = gridSamples_.find(*sampleKey.domain);
                if (searchGridSample == gridSamples_.end()) {
                    InitDomainResult res = initDomain(*sampleKey.domain, prepArgs);
                    encodeAdditionalHandles = std::move(res.encodeAdditionalHandles);
                    return res.gridSample.get();
                }
                else {
                    return searchGridSample->second;
                }
            });

            auto sample = gridSample.sample->duplicate();
            sample->setValue(PDT_KEY, sampleKey.productDefinitionTemplateNumber);

            // Insert with hint
            auto it = samples_.try_emplace(searchSample, sampleKey, std::move(sample));
            return HandleFieldResult{{std::move(prepArgs.metadataWithOverwrites)},
                                     it->second->duplicate(),
                                     std::move(encodeAdditionalHandles)};
        }
        else {
            // Use already prepared sample
            return HandleFieldResult{
                {std::move(prepArgs.metadataWithOverwrites)}, searchSample->second->duplicate(), {}};
        }
    }
    else {
        // No prepared sample to lookup - create and
        PreparedSampleArguments prepArgs = prepareSampleArguments({}, md);

        GridSample gridSample = createGridSample(prepArgs);

        auto newSample = gridSample.sample->duplicate();
        newSample->setValue(PDT_KEY, sampleKey.productDefinitionTemplateNumber);
        return HandleFieldResult{
            {std::move(prepArgs.metadataWithOverwrites)}, std::move(newSample), extractLonLat(gridSample)};
    }
}

void SampleManager::transferProductKeys(const SampleKey& sampleKey, const message::Metadata& md,
                                        MioGribHandle& handle) const {
    lookupAndSetKeys(grib2ProductHandler_, getPdtKeyList(grib2ProductHandler_, sampleKey), md, handle);
}


};  // namespace multio::action::encodeGrib2

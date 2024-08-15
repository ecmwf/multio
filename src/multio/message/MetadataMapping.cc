
#include "MetadataMapping.h"


#include "eckit/exception/Exceptions.h"

#include <sstream>
#include <typeinfo>

namespace multio::message {

namespace {

void applyMapping(Metadata& data, const MetadataMapping::MatchKeyType& metadataKey,
                  const std::pair<MetadataValue, eckit::LocalConfiguration>& dataPair,
                  const MetadataMapping::KeyMapping& mapping, bool isOptional) {
    for (const auto& keyPair : mapping) {
        if (dataPair.second.has(keyPair.second)) {
            if (auto mv = tryToMetadataValue(dataPair.second, keyPair.second); mv) {
                data.set(keyPair.first, std::move(*mv));
            }
            else {
                std::ostringstream oss;
                oss << "Metadata mapping failure: Source key \"" << metadataKey << "\" in metadata is resolving to \""
                    << dataPair.first << "\". The value for key \"" << keyPair.second
                    << "\" can not be parsed to a valid metadata value: "
                    << dataPair.second.getSubConfiguration(keyPair.second) << std::endl;
                throw MetadataMappingException(oss.str(), Here());
            }
        }
        else {
            if (!isOptional) {
                std::ostringstream oss;
                oss << "Metadata mapping failure: Source key \"" << metadataKey << "\" in metadata is resolving to \""
                    << dataPair.first << "\" which mapping is not providing a mapping for key \"" << keyPair.second
                    << "\"." << std::endl;
                throw MetadataMappingException(oss.str(), Here());
            }
        }
    }
}

MetadataMapping::DataMapping constructDataMapping(
    const MetadataMapping::MatchKeyType& metadataKey, const MetadataMapping::KeyMapping& mappings,
    const MetadataMapping::KeyMapping& optionalMappings,
    const std::unordered_map<MetadataValue, eckit::LocalConfiguration>& source) {
    MetadataMapping::DataMapping ret{};

    for (const auto& dataPair : source) {
        Metadata data;

        applyMapping(data, metadataKey, dataPair, mappings, false);
        applyMapping(data, metadataKey, dataPair, optionalMappings, true);

        ret.emplace(dataPair.first, std::move(data));
    }

    return ret;
}


std::unordered_map<MetadataValue, eckit::LocalConfiguration> constructSourceMap(
    const std::vector<eckit::LocalConfiguration>& mapDataList, const MetadataMapping::MatchKeyType& key) {
    std::unordered_map<MetadataValue, eckit::LocalConfiguration> map;
    for (auto const& cfg : mapDataList) {
        if (auto k = tryToMetadataValue(cfg, key); k) {
            map.insert({*k, cfg});
        }
        else {
            std::ostringstream oss;
            oss << "Metadata mapping failure: The data value " << cfg.getString(key) << " for source key \"" << key
                << " can not be parsed to a valid metadata value" << std::endl;
            throw MetadataMappingException(oss.str(), Here());
        }
    }
    return map;
}

std::string metadataMappingExceptionReason(const std::string& r) {
    std::string s{"Mapping failure: "};
    s.append(r);
    return s;
}
}  // namespace

MetadataMappingException::MetadataMappingException(const std::string& r, const eckit::CodeLocation& location) :
    MetadataException(metadataMappingExceptionReason(r), location) {}


MetadataMapping::MetadataMapping(const MatchKeyType& metadataKey, const DataMapping& mapping,
                                 const std::optional<MetadataMapping::KeyType>& targetPath) :
    metadataKey_(metadataKey), mapData_{mapping}, targetPath_{targetPath} {}

MetadataMapping::MetadataMapping(const MatchKeyType& metadataKey, DataMapping&& mapping,
                                 const std::optional<MetadataMapping::KeyType>& targetPath) :
    metadataKey_(metadataKey), mapData_{std::move(mapping)}, targetPath_{targetPath} {}


MetadataMapping::MetadataMapping(const MatchKeyType& metadataKey, const KeyMapping& mappings,
                                 const KeyMapping& optionalMappings,
                                 const std::vector<eckit::LocalConfiguration>& mapDataList,
                                 const MetadataMapping::KeyType& matchKey,
                                 const std::optional<MetadataMapping::KeyType>& targetPath) :
    MetadataMapping(
        metadataKey,
        constructDataMapping(metadataKey, mappings, optionalMappings, constructSourceMap(mapDataList, matchKey)),
        targetPath) {}

MetadataMapping::MetadataMapping(const MatchKeyType& metadataKey, const KeyMapping& mappings,
                                 const KeyMapping& optionalMappings,
                                 const std::unordered_map<MetadataValue, eckit::LocalConfiguration>& source,
                                 const std::optional<MetadataMapping::KeyType>& targetPath) :
    MetadataMapping(metadataKey, constructDataMapping(metadataKey, mappings, optionalMappings, source), targetPath) {}


void MetadataMapping::applyInplace(Metadata& m, MetadataMappingOptions options) const {
    auto searchLookUpKey = m.find(metadataKey_);
    if (searchLookUpKey == m.end()) {
        if (options.enforceMatch) {
            std::ostringstream oss;
            oss << "Metadata has no source key \"" << metadataKey_ << "\"";
            throw MetadataMappingException(oss.str(), Here());
        }
        return;
    }
    auto searchMappingData = mapData_.find(searchLookUpKey->second);
    if (searchMappingData == mapData_.end()) {
        if (options.enforceMatch) {
            std::ostringstream oss;
            oss << "Metadata mapping failure: Source key \"" << metadataKey_ << "\" in metadata is resolving to \""
                << searchLookUpKey->second << "\" for which no mapping has be provided in the mapping file."
                << std::endl;
            throw MetadataMappingException(oss.str(), Here());
        }
        return;
    }

    // TODO handle internals without LocalConfiguration
    std::optional<Metadata> targetConfMaybe{};
    Metadata& ms = targetPath_ ? (targetConfMaybe = m.get<BaseMetadata>(*targetPath_), *targetConfMaybe) : m;

    if (options.overwriteExisting) {
        ms.updateOverwrite(searchMappingData->second);
    }
    else {
        ms.updateNoOverwrite(searchMappingData->second);
    }

    if (targetPath_) {
        m.set(*targetPath_, ms);
    }
}


Metadata MetadataMapping::apply(Metadata&& m, MetadataMappingOptions options) const {
    Metadata mc(std::move(m));
    applyInplace(mc, options);
    return mc;
};

Metadata MetadataMapping::apply(const Metadata& m, MetadataMappingOptions options) const {
    Metadata mc(m);
    applyInplace(mc, options);
    return mc;
};

}  // namespace multio::message

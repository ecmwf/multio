
#include "MetadataMapping.h"
#include <sstream>
#include "eckit/exception/Exceptions.h"

namespace multio {
namespace message {


namespace {
std::unordered_map<std::string, eckit::LocalConfiguration> constructSourceMap(
    const std::vector<eckit::LocalConfiguration>& mapDataList, const std::string& key) {
    std::unordered_map<std::string, eckit::LocalConfiguration> map;
    for (auto const& cfg : mapDataList) {
        map.insert({cfg.getString(key), cfg});
    }
    return map;
}

std::string metadataMappingExceptionReason(const std::string& r) {
    std::string s{"Metadata mapping failure: "};
    s.append(r);
    return s;
}
}  // namespace

MetadataMappingException::MetadataMappingException(const std::string& r, const eckit::CodeLocation& location):
    eckit::Exception(metadataMappingExceptionReason(r), location) {
}


MetadataMapping::MetadataMapping(const std::string& metadataKey, const eckit::LocalConfiguration& mappings,
                                 const eckit::LocalConfiguration& optionalMappings,
                                 const std::vector<eckit::LocalConfiguration>& mapDataList, const std::string& matchKey,
                                 const eckit::Optional<std::string>& targetPath) :
    metadataKey_(metadataKey),
    mapping_{mappings},
    optionalMapping_{optionalMappings},
    mapData_{constructSourceMap(mapDataList, matchKey)},
    targetPath_{targetPath} {}

MetadataMapping::MetadataMapping(const std::string& metadataKey, const eckit::LocalConfiguration& mappings,
                                 const eckit::LocalConfiguration& optionalMappings,
                                 const std::unordered_map<std::string, eckit::LocalConfiguration>& source,
                                 const eckit::Optional<std::string>& targetPath) :
    metadataKey_(metadataKey),
    mapping_{mappings},
    optionalMapping_{optionalMappings},
    mapData_{source},
    targetPath_{targetPath} {}

MetadataMapping::MetadataMapping(const std::string& metadataKey, const eckit::LocalConfiguration& mappings,
                                 const eckit::LocalConfiguration& optionalMappings,
                                 std::unordered_map<std::string, eckit::LocalConfiguration>&& source,
                                 const eckit::Optional<std::string>& targetPath) :
    metadataKey_(metadataKey),
    mapping_{mappings},
    optionalMapping_{optionalMappings},
    mapData_{std::move(source)},
    targetPath_{targetPath} {}


void MetadataMapping::applyInplace(Metadata& m, MetadataMappingOptions options) const {
    if (!m.has(metadataKey_)) {
        if (options.enforceMatch) {
            std::ostringstream oss;
            oss << "Metadata has no source key \"" << metadataKey_ << "\"";
            throw MetadataMappingException(oss.str(), Here());
        }
        return;
    }
    std::string lookUpKey = m.getString(metadataKey_);
    auto from = mapData_.find(lookUpKey);
    if (from == mapData_.end()) {
        std::ostringstream oss;
        oss << "Metadata mapping failure: Source key \"" << metadataKey_ << "\" in metadata is resolving to \""
            << lookUpKey << "\" for which no mapping has be provided in the mapping file." << std::endl;
        throw MetadataMappingException(oss.str(), Here());
    }

    // TODO handle internals without LocalConfiguration
    eckit::Optional<eckit::LocalConfiguration> targetConfMaybe{};
    eckit::LocalConfiguration& ms
        = targetPath_ ? (targetConfMaybe = m.getSubConfiguration(*targetPath_), *targetConfMaybe) : m;

    // Please don't blame about this [&] capture. It is really just convenient to use the lambda here and won't cause
    // any harm. Adding all variables and members to the list is just cumbersome
    const auto applyMapping = [&](const eckit::LocalConfiguration& mapping, bool isOptional) {
        for (const auto& key : mapping.keys()) {
            if (!options.overwriteExisting && m.has(key)) {
                continue;
            }
            std::string lookUpMapKey = mapping.getString(key);
            if (from->second.has(lookUpMapKey)) {
                ms.set(key, from->second.getString(lookUpMapKey));
            }
            else {
                if (!isOptional) {
                    std::ostringstream oss;
                    oss << "Metadata mapping failure: Source key \"" << metadataKey_
                        << "\" in metadata is resolving to \"" << lookUpKey
                        << "\" which mapping is not providing a mapping for key \"" << lookUpMapKey << "\"."
                        << std::endl;
                    throw MetadataMappingException(oss.str(), Here());
                }
            }
        }
    };
    applyMapping(mapping_, false);
    applyMapping(optionalMapping_, true);

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

}  // namespace message
}  // namespace multio

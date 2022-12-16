
#include "MetadataMapping.h"
#include <sstream>
#include "eckit/exception/Exceptions.h"

namespace multio {
namespace message {

namespace {
std::unordered_map<std::string, eckit::LocalConfiguration> constructSourceMap(
    const std::vector<eckit::LocalConfiguration>& sourceList, const std::string& key) {
    std::unordered_map<std::string, eckit::LocalConfiguration> map;
    for (auto const& cfg : sourceList) {
        map.insert({cfg.getString(key), cfg});
    }
    return map;
}
}  // namespace

MetadataMapping::MetadataMapping(const std::string& sourceKey, const eckit::LocalConfiguration& mappings,
                                   const eckit::LocalConfiguration& optionalMappings,
                                   const std::vector<eckit::LocalConfiguration>& sourceList, const std::string& key,
                                   const eckit::Optional<std::string>& targetPath) :
    sourceKey_(sourceKey),
    mapping_{mappings},
    optionalMapping_{optionalMappings},
    source_{constructSourceMap(sourceList, key)},
    targetPath_{targetPath} {}
MetadataMapping::MetadataMapping(const std::string& sourceKey, const eckit::LocalConfiguration& mappings,
                                   const eckit::LocalConfiguration& optionalMappings,
                                   const std::unordered_map<std::string, eckit::LocalConfiguration>& source,
                                   const eckit::Optional<std::string>& targetPath) :
    sourceKey_(sourceKey),
    mapping_{mappings},
    optionalMapping_{optionalMappings},
    source_{source},
    targetPath_{targetPath} {}
MetadataMapping::MetadataMapping(const std::string& sourceKey, const eckit::LocalConfiguration& mappings,
                                   const eckit::LocalConfiguration& optionalMappings,
                                   std::unordered_map<std::string, eckit::LocalConfiguration>&& source,
                                   const eckit::Optional<std::string>& targetPath) :
    sourceKey_(sourceKey),
    mapping_{mappings},
    optionalMapping_{optionalMappings},
    source_{std::move(source)},
    targetPath_{targetPath} {}


void MetadataMapping::applyInplace(Metadata& m, MetadataMappingOptions options) const {
    if (!m.has(sourceKey_)) {
        if (options.enforceMatch) {
            std::ostringstream oss;
            oss << "MetadataMapping failure: Metadata has no source key \"" << sourceKey_ << "\"";
            throw eckit::Exception(oss.str());
        }
        return;
    }
    std::string lookUpKey = m.getString(sourceKey_);
    auto from = source_.find(lookUpKey);
    if (from == source_.end()) {
        std::ostringstream oss;
        oss << "Metadata mapping failure: Source key \"" << sourceKey_ << "\" in metadata is resolving to \""
            << lookUpKey << "\" for which no mapping has be provided in the mapping file." << std::endl;
        throw eckit::Exception(oss.str());
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
                    oss << "Metadata mapping failure: Source key \"" << sourceKey_
                        << "\" in metadata is resolving to \"" << lookUpKey
                        << "\" which mapping is not providing a mapping for key \"" << lookUpMapKey << "\"."
                        << std::endl;
                    throw eckit::Exception(oss.str());
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

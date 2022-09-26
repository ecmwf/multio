
#include "ParameterMapping.h"
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

ParameterMapping::ParameterMapping(const std::string& sourceKey,
                                   const eckit::LocalConfiguration& mappings,
                                   const std::vector<eckit::LocalConfiguration>& sourceList,
                                   const std::string& key) :
    sourceKey_(sourceKey), mapping_{mappings}, source_{constructSourceMap(sourceList, key)} {}
ParameterMapping::ParameterMapping(
    const std::string& sourceKey, const eckit::LocalConfiguration& mappings,
    const std::unordered_map<std::string, eckit::LocalConfiguration>& source) :
    sourceKey_(sourceKey), mapping_{mappings}, source_{source} {}
ParameterMapping::ParameterMapping(
    const std::string& sourceKey, const eckit::LocalConfiguration& mappings,
    std::unordered_map<std::string, eckit::LocalConfiguration>&& source) :
    sourceKey_(sourceKey), mapping_{mappings}, source_{std::move(source)} {}

void ParameterMapping::applyInplace(Metadata& m, ParameterMappingOptions options) const {
    if (!m.has(sourceKey_)) {
        if (options.enforceMatch) {
            std::ostringstream oss;
            oss << "ParameterMapping failure: Metadata has no source key \"" << sourceKey_ << "\"";
            throw eckit::Exception(oss.str());
        }
        return;
    }
    std::string lookUpKey = m.getString(sourceKey_);
    auto from = source_.find(lookUpKey);
    if (from == source_.end()) {
        std::ostringstream oss;
        oss << "Parameter mapping failure: Source key \"" << sourceKey_
            << "\" in metadata is resolving to \"" << lookUpKey
            << "\" for which no mapping has be provided in the mapping file." << std::endl;
        throw eckit::Exception(oss.str());
    }
    for (const auto& key : mapping_.keys()) {
        if(!options.overwriteExisting && m.has(key)) {
            continue;
        }
        std::string lookUpMapKey = mapping_.getString(key);
        if (!from->second.has(lookUpMapKey)) {
            std::ostringstream oss;
            oss << "Parameter mapping failure: Source key \"" << sourceKey_
                << "\" in metadata is resolving to \"" << lookUpKey
                << "\" which mapping is not providing a mapping for key \"" << lookUpMapKey << "\"."
                << std::endl;
            throw eckit::Exception(oss.str());
        }
        m.set(key, from->second.getString(lookUpMapKey));
    }
};

Metadata ParameterMapping::apply(Metadata&& m, ParameterMappingOptions options) const {
    Metadata mc(std::move(m));
    applyInplace(mc, options);
    return mc;
};
Metadata ParameterMapping::apply(const Metadata& m, ParameterMappingOptions options) const {
    Metadata mc(m);
    applyInplace(mc, options);
    return mc;
};

}  // namespace message
}  // namespace multio

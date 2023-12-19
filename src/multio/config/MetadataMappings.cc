#include "MetadataMappings.h"
#include "MultioConfiguration.h"

#include "eckit/exception/Exceptions.h"
#include "eckit/value/Value.h"

namespace multio::config {


message::MetadataMapping::KeyMapping toKeyMapping(const eckit::LocalConfiguration& map) {
    message::MetadataMapping::KeyMapping ret;

    for (const auto& key : map.keys()) {
        ret.emplace_back(key, map.getString(key));
    }

    return ret;
}

const std::vector<message::MetadataMapping>& MetadataMappings::getMappings(const MultioConfiguration& multioConf,
                                                                           const std::string& mapping) const {
    const auto& configFile = multioConf.getConfigFile(mapping);

    auto search = mappings_.find(configFile.source);
    if (search != mappings_.end()) {
        return *(search->second.get());
    }
    else {
        if (!configFile.content.has("data")) {
            std::ostringstream oss;
            oss << "MetadataMapping " << configFile.source << " does not have a top-level key \"data\"" << std::endl;
            throw message::MetadataMappingException(oss.str(), Here());
        }

        std::vector<eckit::LocalConfiguration> sourceList = configFile.content.getSubConfigurations("data");

        for (auto& s : sourceList) {
            for (auto& key : s.keys()) {
                // Replace the value if it is string
                if (s.getSubConfiguration(key).get().isString()) {
                    s.set(key, multioConf.replaceCurly(s.getString(key)));
                }
            }
        }


        // Evaluate mappings block
        if (!configFile.content.has("mappings")) {
            std::ostringstream oss;
            oss << "Metadata mapping " << configFile.source << " does not list a \"mappings\" block" << std::endl;
            throw message::MetadataMappingException(oss.str(), Here());
        }
        auto mappingsVector = configFile.content.getSubConfigurations("mappings");
        std::unique_ptr<std::vector<message::MetadataMapping>> v
            = std::make_unique<std::vector<message::MetadataMapping>>();
        v->reserve(mappingsVector.size());

        int mcind = 1;
        for (const auto& mc : mappingsVector) {
            if (!mc.has("match")) {
                std::ostringstream oss;
                oss << "Mapping #" << mcind << " of parameter mapping \"" << mapping
                    << "\" does not list a \"match\" block" << std::endl;
                throw message::MetadataMappingException(oss.str(), Here());
            }
            if (!mc.has("map") && !mc.has("optional-map")) {
                std::ostringstream oss;
                oss << "Mapping #" << mcind << " of parameter mapping \"" << mapping
                    << "\" does not list a \"map\" or \"optional-map\" block" << std::endl;
                throw message::MetadataMappingException(oss.str(), Here());
            }
            auto matchBlock = mc.getSubConfiguration("match");
            auto matchKeys = matchBlock.keys();
            if (matchKeys.size() != 1) {
                std::ostringstream oss;
                oss << "Match block of mapping #" << mcind << " of parameter mapping \"" << mapping
                    << "\" should list exactly one key mapping. Found " << matchKeys.size() << " mappings."
                    << std::endl;
                throw message::MetadataMappingException(oss.str(), Here());
            }
            std::string sourceKey = matchKeys[0];
            std::string targetKey = matchBlock.getString(sourceKey);

            auto mappings = toKeyMapping(mc.getSubConfiguration("map"));
            auto optionalMappings = toKeyMapping(mc.getSubConfiguration("optional-map"));

            auto targetPath = mc.has("target-path") ? std::optional<std::string>{mc.getString("target-path")}
                                                    : std::optional<std::string>{};

            v->emplace(v->end(), std::move(sourceKey), std::move(mappings), std::move(optionalMappings), sourceList,
                       std::move(targetKey), std::move(targetPath));
            ++mcind;
        }

        return *mappings_.emplace(mapping, std::move(v)).first->second.get();
    }
}


}  // namespace multio::config

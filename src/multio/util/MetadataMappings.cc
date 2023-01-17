#include "ConfigurationContext.h" // Include header now
#include "MetadataMappings.h"
#include "eckit/exception/Exceptions.h"

namespace multio {
namespace util {

// namespace {
// const YAMLFile& getMetadataMappingConfiguration(const GlobalConfCtx& globalConfCtx) {
//     if (globalConfCtx.globalConfig().has("parameter-mappings")) {
//         return globalConfCtx.getYAMLFile(([&](){
//             try {
//                 return globalConfCtx.globalConfig().getString("parameter-mappings");
//             }
//             catch (...) {
//                 std::throw_with_nested(eckit::Exception("The global key \"parameter-mapping\" is supposed to map to a string. Default: \"parameter-mappings.yaml\"."));
//             }
//         })());
//     }
//     else {
//         return globalConfCtx.getYAMLFile("parameter-mappings.yaml");
//     }
// }
// }  // namespace

MetadataMappings::MetadataMappings(const GlobalConfCtx& globalConfCtx) :
    globalConfCtx_(globalConfCtx),
    // configFile_{getMetadataMappingConfiguration(globalConfCtx)},
    mappings_{} {}


const std::vector<message::MetadataMapping>& MetadataMappings::getMappings(const std::string& mapping) const {
    const auto& yamlFile = globalConfCtx_.getYAMLFile(mapping);

    auto search = mappings_.find(yamlFile.path);
    if (search != mappings_.end()) {
        return search->second;
    } else {
        if (!yamlFile.content.has("data")) {
            std::ostringstream oss;
            oss << "MetadataMapping " << yamlFile.path << " does not have a top-level key \"data\"" << std::endl;
            throw eckit::Exception(oss.str());
        }

        std::vector<eckit::LocalConfiguration> sourceList = yamlFile.content.getSubConfigurations("data");

        // Evaluate mappings block
        if (!yamlFile.content.has("mappings")) {
            std::ostringstream oss;
            oss << "Metadata mapping " << yamlFile.path << " does not list a \"mappings\" block" << std::endl;
            throw eckit::Exception(oss.str());
        }
        auto mappingsVector = yamlFile.content.getSubConfigurations("mappings");
        std::vector<message::MetadataMapping> v;
        v.reserve(mappingsVector.size());

        int mcind = 1;
        for (const auto& mc: mappingsVector) {
            if (!mc.has("match")) {
                std::ostringstream oss;
                oss << "Mapping #" << mcind << " of parameter mapping \"" << mapping << "\" does not list a \"match\" block" << std::endl;
                throw eckit::Exception(oss.str());
            }
            if (!mc.has("map") && !mc.has("optionalMap")) {
                std::ostringstream oss;
                oss << "Mapping #" << mcind << " of parameter mapping \"" << mapping << "\" does not list a \"map\" or \"optionalMap\" block" << std::endl;
                throw eckit::Exception(oss.str());
            }
            auto matchBlock = mc.getSubConfiguration("match");
            auto matchKeys = matchBlock.keys();
            if (matchKeys.size() != 1) {
                std::ostringstream oss;
                oss << "Match block of mapping #" << mcind << " of parameter mapping \"" << mapping << "\" should list exactly one key mapping. Found " << matchKeys.size() << " mappings." << std::endl;
                throw eckit::Exception(oss.str());
            }
            std::string sourceKey = matchKeys[0];
            std::string targetKey = matchBlock.getString(sourceKey);

            auto mappings = mc.getSubConfiguration("map");
            auto optionalMappings = mc.getSubConfiguration("optionalMap");

            auto targetPath = mc.has("targetPath") ? eckit::Optional<std::string>{mc.getString("targetPath")} : eckit::Optional<std::string>{};

            v.emplace(v.end(), std::move(sourceKey), std::move(mappings), std::move(optionalMappings), sourceList, std::move(targetKey), std::move(targetPath));
            ++mcind;
        }
        mappings_.emplace(mapping, std::move(v));
        return mappings_.at(mapping);
    }
}


}  // namespace util
}  // namespace multio

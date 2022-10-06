#include "ConfigurationContext.h" // Include header now
#include "ParameterMappings.h"
#include "eckit/exception/Exceptions.h"

namespace multio {
namespace util {

namespace {
eckit::LocalConfiguration getParameterMappingConfiguration(const GlobalConfCtx& globalConfCtx) {
    if (globalConfCtx.globalConfig().has("parameter-mappings")) {
        auto fileNameMaybe = ([&](){
            try {
                return eckit::Optional<std::string>{globalConfCtx.globalConfig().getString("parameter-mappings")};
            }
            catch (const eckit::Exception&) {
                return eckit::Optional<std::string>{};
            }
        })();
        if (fileNameMaybe) {
            return globalConfCtx.getYAMLFile(*fileNameMaybe);
        } else {
            return globalConfCtx.globalConfig().getSubConfiguration("parameter-mappings");
        }
    }
    else {
        return globalConfCtx.getYAMLFile("parameter-mappings.yaml");
    }
}
}  // namespace

ParameterMappings::ParameterMappings(std::reference_wrapper<const GlobalConfCtx> globalConfCtx) :
    globalConfCtx_(std::move(globalConfCtx)),
    configs_{getParameterMappingConfiguration(globalConfCtx_.get())},
    mappings_{} {}    
    

std::pair<std::string, std::shared_ptr<std::vector<message::ParameterMapping>>> ParameterMappings::getMappings(const std::string& mapping) const {
    auto search = mappings_.find(mapping);
    if (search != mappings_.end()) {
        return *search;
    }
    if (configs_.has(mapping)) {
        auto mappingConfig =  configs_.getSubConfiguration(mapping);
        
        // Evaluate source block
        if (!mappingConfig.has("source")) {
            std::ostringstream oss;
            oss << "Parameter mapping \"" << mapping << "\" does not list a \"source\" block" << std::endl;
            throw eckit::Exception(oss.str());
        }
        
        auto sourceConfigBlock =  mappingConfig.getSubConfiguration("source");
        if (!sourceConfigBlock.has("file")) {
            std::ostringstream oss;
            oss << "Source block of parameter mapping \"" << mapping << "\" does not list a \"file\" key, mapping to a YAML file." << std::endl;
            throw eckit::Exception(oss.str());
        }
        auto sourceFname = sourceConfigBlock.getString("file");
        if (!sourceConfigBlock.has("path")) {
            std::ostringstream oss;
            oss << "Source block of parameter mapping \"" << mapping << "\" does not list a \"path\" key, specifing the top level key or path in the file \"" << sourceFname << "\"" << std::endl;
            throw eckit::Exception(oss.str());
        }
        auto sourcePath = sourceConfigBlock.getString("path");
        
        auto sourceConfig = globalConfCtx_.get().getYAMLFile(sourceFname);
        if (!sourceConfig.has(sourcePath)) {
            std::ostringstream oss;
            oss << "YAML file \"" << sourceFname << "\" for parameter mapping \"" << mapping << "\" does have a top-level path \"" << sourcePath << "\"" << std::endl;
            throw eckit::Exception(oss.str());
        }
        
        std::vector<eckit::LocalConfiguration> sourceList = sourceConfig.getSubConfigurations(sourcePath);
        
        // Evaluate mappings block
        if (!mappingConfig.has("mappings")) {
            std::ostringstream oss;
            oss << "Parameter mapping \"" << mapping << "\" does not list a \"mappings\" block" << std::endl;
            throw eckit::Exception(oss.str());
        }
        auto mappingsVector = mappingConfig.getSubConfigurations("mappings");
        std::vector<message::ParameterMapping> v;
        v.reserve(mappingsVector.size());
        
        int mcind = 1;
        for (const auto& mc: mappingsVector) {
            if (!mc.has("match")) {
                std::ostringstream oss;
                oss << "Mapping #" << mcind << " of parameter mapping \"" << mapping << "\" does not list a \"match\" block" << std::endl;
                throw eckit::Exception(oss.str());
            }
            if (!mc.has("map")) {
                std::ostringstream oss;
                oss << "Mapping #" << mcind << " of parameter mapping \"" << mapping << "\" does not list a \"map\" block" << std::endl;
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
            auto mappingsKeys = mappings.keys();
            if (mappingsKeys.size() <= 0) {
                std::ostringstream oss;
                oss << "Map block of mapping #" << mcind << " of parameter mapping \"" << mapping << "\" should list at least one mapping." << std::endl;
                throw eckit::Exception(oss.str());
            }
            
            v.emplace(v.end(), std::move(sourceKey), std::move(mappings), sourceList, std::move(targetKey));
            ++mcind;
        }
        mappings_.emplace(mapping, std::make_shared<std::vector<message::ParameterMapping>>(std::move(v)));
        return *(mappings_.find(mapping));
    }
    
    std::ostringstream oss;
    oss << "No parameter mapping \"" << mapping << "\" found. Available mappings: " << std::endl;
    for(const auto& k: configs_.keys()) {
        oss << "  - " << k << std::endl;
    }
    throw eckit::Exception(oss.str());
}


}  // namespace util
}  // namespace multio

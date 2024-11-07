#include "RemapParamID.h"

namespace multio::action {
 
RemapParamID::RemapParamID( const config::ComponentConfiguration& compConf ):
hasMapping_{false},paramIDMap_{} {
    const auto mappings
        = compConf.parsedConfig().has("mapping-param") ? compConf.parsedConfig().getSubConfigurations("mapping-param") : std::vector<eckit::LocalConfiguration>{};

    if (!mappings.empty()) {
        hasMapping_ = true;
        for (const auto& mapping : mappings) {
            auto matcher = mapping.getSubConfiguration("where");
            std::string param           = matcher.getString("param-is");
            std::string operation       = matcher.getString("operation-is");
            std::string outputFrequency = matcher.getString("output-frequency-is");
            std::string newParam        = mapping.getString("map-to-param");
            std::ostringstream key;
            // TODO: outptuFrequqncy should always be computed in seconds
            key << param << "_" << operation << "_" << outputFrequency << std::endl;
            paramIDMap_[key.str()] = newParam;
        }
    }

}

void RemapParamID::ApplyRemap( message::Metadata& md, const std::string& opname, const std::string& outputFrequency ) {
    if ( hasMapping_ ) {
        std::ostringstream key;
        std::string param{"xxx"};
        if ( md.has("param") ) {
            param = md.getString("param");
        }
        else if ( md.has("paramId") ) {
            param = md.getString("paramId");
        }
        key << param << "_" << opname << "_" << outputFrequency << std::endl;
        auto it = paramIDMap_.find(key.str());
        if ( it != paramIDMap_.end() ) {
            md.set("param", it->second);
        }
    }
}

}

#include "RemapParamID.h"

#include <cstdlib>
#include <string>

#include "multio/LibMultio.h"
#include "multio/message/Glossary.h"
#include "multio/util/Substitution.h"


namespace multio::action {

using message::glossary;

RemapParamID::RemapParamID(const config::ComponentConfiguration& compConf) : hasMapping_{false}, paramIDMap_{} {
    const auto mappings = compConf.parsedConfig().has("mapping-param")
                            ? compConf.parsedConfig().getSubConfigurations("mapping-param")
                            : std::vector<eckit::LocalConfiguration>{};

    if (!mappings.empty()) {
        hasMapping_ = true;
        for (const auto& mapping : mappings) {
            auto matcher = mapping.getSubConfiguration("where");
            std::string param = matcher.getString("param-is");
            std::string operation = matcher.getString("operation-is");
            std::string outputFrequency = matcher.getString("output-frequency-is");
            std::string newParam = mapping.getString("map-to-param");
            std::ostringstream key;
            // TODO: outptuFrequqncy should always be computed in seconds
            key << param << "_" << operation << "_" << outputFrequency << std::endl;
            paramIDMap_[key.str()] = newParam;
        }
    }
}

void RemapParamID::ApplyRemap(message::Metadata& md, const std::string& opname, const std::string& outputFrequency) {
    if (hasMapping_) {
        std::ostringstream key;
        std::string cparam{"xxx"};
        if (auto param = md.getOpt<std::string>(glossary().param); param) {
            cparam = *param;
        }
        if (auto paramId = md.getOpt<std::int64_t>(glossary().paramId); paramId) {
            cparam = std::to_string(*paramId);
        }
        else {
            throw eckit::SeriousBug{"param/paramId metadata not present", Here()};
        }
        key << cparam << "_" << opname << "_" << outputFrequency << std::endl;
        auto it = paramIDMap_.find(key.str());
        if (it != paramIDMap_.end()) {
            md.set(glossary().paramId, std::int64_t(::atol(it->second.c_str())));
            md.set(glossary().param, it->second.c_str());
        }
    }
}

}  // namespace multio::action

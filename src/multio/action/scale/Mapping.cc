#include "Mapping.h"

#include <cstdlib>
#include <string>

#include "MetadataUtils.h"
#include "multio/LibMultio.h"
#include "multio/message/Glossary.h"
#include "multio/util/Substitution.h"


namespace multio::action {

using message::glossary;

ScaleMapping::ScaleMapping(const config::ComponentConfiguration& compConf) : hasMapping_(false), scaleMap_{} {

    const auto mappings = compConf.parsedConfig().has("mapping-definition")
                            ? compConf.parsedConfig().getSubConfigurations("mapping-definition")
                            : std::vector<eckit::LocalConfiguration>{};

    if (!mappings.empty()) {
        hasMapping_ = true;
        for (const auto& mapping : mappings) {
            auto matcher = mapping.getSubConfiguration("case");
            scaleMap_[matcher.getString("param-is")] = matcher.getString("map-to-param");
        }
    }
}

void ScaleMapping::applyMapping(message::Metadata& md) const {
    if (hasMapping_) {

        std::string cparam = extractParam(md);

        auto it = scaleMap_.find(cparam);
        if (it != scaleMap_.end()) {
            md.set(glossary().paramId, std::stoll(it->second));
            md.set(glossary().param, it->second.c_str());
        }
    }
}

}  // namespace multio::action
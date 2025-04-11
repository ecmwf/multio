#include "Scaling.h"

#include <cstdlib>
#include <string>

#include "multio/LibMultio.h"
#include "multio/util/Substitution.h"


namespace multio::action::scale {


ScaleScaling::ScaleScaling(const config::ComponentConfiguration& compConf) : hasScaling_(false), scaleFactor_{} {

    const auto mappings = compConf.parsedConfig().has("mapping-definition")
                            ? compConf.parsedConfig().getSubConfigurations("mapping-definition")
                            : std::vector<eckit::LocalConfiguration>{};

    if (!mappings.empty()) {
        hasScaling_ = true;
        for (const auto& mapping : mappings) {
            auto matcher = mapping.getSubConfiguration("case");
            scaleFactor_[matcher.getString("param-is")] = matcher.getDouble("scaling-factor");
        }
    }
}
double ScaleScaling::getScalingFactor(const std::string paramID) const {
    auto it = scaleFactor_.find(paramID);
    if (it != scaleFactor_.end()) {
        return it->second;
    }
    else {
        throw eckit::SeriousBug{"Scaling Factor not found paramID" + paramID, Here()};
    }
}


}  // namespace multio::action::scale
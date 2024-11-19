#include "Scaling.h"

#include <string>
#include <cstdlib>

#include "multio/LibMultio.h"
#include "multio/message/Glossary.h"
#include "multio/util/Substitution.h"


namespace multio::action {

using message::glossary;

ScaleScaling::ScaleScaling( const config::ComponentConfiguration& compConf ):
hasScaling_(false),scaleFactor_{} {

    const auto mappings
        = compConf.parsedConfig().has("mapping-definition") ? compConf.parsedConfig().getSubConfigurations("mapping-definition") : std::vector<eckit::LocalConfiguration>{};

    if (!mappings.empty()) {
        hasScaling_ = true;
        for (const auto& mapping : mappings) {
            auto matcher = mapping.getSubConfiguration("case");
            scaleFactor_[matcher.getString("param-is")] = matcher.getDouble("scaling-factor");
        }
    }

}
double ScaleScaling::getScalingFactor( const std::string paramID) const {
    auto it = scaleFactor_.find(paramID);
    if (it != scaleFactor_.end()) {
        return it->second;
    } else {
        throw eckit::SeriousBug{"Scaling Factor not found paramID" +paramID, Here()};
    }
}
template <typename Precision>
void ScaleScaling::applyScaling( message::Message & msg) const {
    if (hasScaling_) {
        std::ostringstream key;
        std::string cparam{"xxx"};
        
        // Extract the scaling parameter
        if (auto param = msg.metadata().getOpt<std::string>(glossary().param); param) {
            cparam = *param;
        } else if (auto paramId = msg.metadata().getOpt<std::int64_t>(glossary().paramId); paramId) {
            cparam = std::to_string(*paramId);
        } else {
            throw eckit::SeriousBug{"param/paramId metadata not present", Here()};
        }

        // Find the scaling factor
        double scaleFactor = getScalingFactor(cparam);

        // Ensure the payload size is compatible with Precision type
        size_t size = msg.payload().size() / sizeof(Precision);
        if (size == 0) {
            throw eckit::SeriousBug{" Payload is empty: Scaling Action: " +msg.metadata().toString(), Here()};
        }

        // Access the payload data
        auto data = static_cast<Precision*>(msg.payload().modifyData());
        if (!data) {
            throw eckit::SeriousBug{" Payload data could not be modified: Scaling Action: "+msg.metadata().toString(), Here()};
            std::cout << "Error: Payload data could not be modified." << std::endl;
            return;
        }
        // Apply the scaling factor using std::transform
        std::transform(data, data + size, data, [scaleFactor](Precision value) {
            return static_cast<Precision>(value * scaleFactor);
        });

        //TODO do we need to consider missing values and skip?
    } else {
        return;
    }
}



// Explicit instantiations for specific types
template void ScaleScaling::applyScaling<float>(message::Message & msg) const;
template void ScaleScaling::applyScaling<double>(message::Message & msg) const;

}
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

template <typename Precision>
void ScaleScaling::applyScaling( message::Message & msg) const {
    if ( hasScaling_ ) {
        std::ostringstream key;
        std::string cparam{"xxx"};
        if (auto param = msg.metadata().getOpt<std::string>(glossary().param); param) {
            cparam = *param;
        }
        if (auto paramId = msg.metadata().getOpt<std::int64_t>(glossary().paramId); paramId) {
            cparam = std::to_string( *paramId );
        }
        else {
            throw eckit::SeriousBug{"param/paramId metadata not present", Here()};
        }

        double scaleFactor = 0.0;
        auto it = scaleFactor_.find(cparam);
        if (it != scaleFactor_.end()) {
            scaleFactor = it->second;
        } else {
            return;
        }

        size_t size = msg.payload().size()/sizeof(Precision);
        auto data = static_cast<Precision*>(msg.payload().modifyData());
        
        std::transform(data, data + size, data, [scaleFactor](Precision value) {
            return static_cast<Precision>(value * scaleFactor);
        });
    }
}


// Explicit instantiations for specific types
template void ScaleScaling::applyScaling<float>(message::Message & msg) const;
template void ScaleScaling::applyScaling<double>(message::Message & msg) const;

}
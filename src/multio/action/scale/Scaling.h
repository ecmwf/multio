#pragma once

#include <map>
#include <string>

#include "eckit/config/LocalConfiguration.h"
#include "multio/config/ComponentConfiguration.h"
#include "multio/message/Message.h"
#include "multio/message/Glossary.h"

namespace multio::action {

using message::glossary;

class ScaleScaling {
private:
  bool hasScaling_;
  std::map<std::string,double> scaleFactor_;
public:
    explicit ScaleScaling( const config::ComponentConfiguration& compConf );
    double getScalingFactor( const std::string paramID) const;
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
    };

}
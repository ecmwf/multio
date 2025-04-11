#pragma once

#include <algorithm>
#include <map>
#include <string>
#include "eckit/config/LocalConfiguration.h"
#include "multio/action/scale/MetadataUtils.h"
#include "multio/config/ComponentConfiguration.h"
#include "multio/message/Glossary.h"
#include "multio/message/Message.h"

namespace multio::action::scale {

using message::glossary;

class ScaleScaling {
private:
    bool hasScaling_;
    std::map<std::string, double> scaleFactor_;

public:
    explicit ScaleScaling(const config::ComponentConfiguration& compConf);
    double getScalingFactor(const std::string paramID) const;

    template <typename Precision>
    void applyScaling(message::Message& msg) const {
        if (hasScaling_) {
            std::string cparam = extractParam(msg.metadata());
            // Find the scaling factor
            double scaleFactor = getScalingFactor(cparam);

            // Ensure the payload size is compatible with Precision type
            size_t size = msg.payload().size() / sizeof(Precision);
            if (size == 0) {
                throw eckit::SeriousBug{" Payload is empty: Scaling Action: " + msg.metadata().toString(), Here()};
            }

            // Access the payload data
            auto data = static_cast<Precision*>(msg.payload().modifyData());
            if (!data) {
                throw eckit::SeriousBug{
                    " Payload data could not be modified: Scaling Action: " + msg.metadata().toString(), Here()};
            }
            // Apply the scaling factor using std::transform
            std::transform(data, data + size, data,
                           [scaleFactor](Precision value) { return static_cast<Precision>(value * scaleFactor); });
        }
        else {
            return;
        }
    }
};

}  // namespace multio::action::scale

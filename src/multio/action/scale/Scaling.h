#pragma once

#include <map>
#include <string>

#include "eckit/config/LocalConfiguration.h"
#include "multio/config/ComponentConfiguration.h"
#include "multio/message/Message.h"

namespace multio::action {

class ScaleScaling {
private:
  bool hasScaling_;
  std::map<std::string,double> scaleFactor_;
public:
    explicit ScaleScaling( const config::ComponentConfiguration& compConf );
    double getScalingFactor( const std::string paramID) const;
    template <typename Precision>
    void applyScaling( message::Message & msg) const ;
};

}
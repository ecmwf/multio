#pragma once

#include <map>
#include <string>

#include "eckit/config/LocalConfiguration.h"
#include "multio/config/ComponentConfiguration.h"
#include "multio/message/Message.h"

namespace multio::action {

class RemapParamID {
private:
  bool hasMapping_;
  std::map<std::string,std::string> paramIDMap_;
public:
    RemapParamID( const config::ComponentConfiguration& compConf );
    void ApplyRemap( message::Metadata& md, const std::string& opname, const std::string& outputFrequency );
};

}

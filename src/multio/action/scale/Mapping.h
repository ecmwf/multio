#pragma once

#include <map>
#include <string>

#include "eckit/config/LocalConfiguration.h"
#include "multio/config/ComponentConfiguration.h"
#include "multio/message/Message.h"

namespace multio::action {

class ScaleMapping {
private:
    bool hasMapping_;
    std::map<std::string, std::string> scaleMap_;

public:
    explicit ScaleMapping(const config::ComponentConfiguration& compConf);
    void applyMapping(message::Metadata& md) const;
};

}  // namespace multio::action
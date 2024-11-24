#pragma once

#include <string>
#include "multio/LibMultio.h"
#include "multio/action/scale/MetadataUtils.h"
#include "multio/message/Metadata.h"

namespace multio::action {
    std::string extractParam(const multio::message::Metadata& md);

}
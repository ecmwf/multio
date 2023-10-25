
#pragma once

#include <vector>

#include "eckit/config/LocalConfiguration.h"

#include "multio/config/MultioConfiguration.h"


namespace multio::config {

std::vector<eckit::LocalConfiguration> configurePlans(const eckit::LocalConfiguration& componentConfig,
                                                      const MultioConfiguration& multioConf);
}

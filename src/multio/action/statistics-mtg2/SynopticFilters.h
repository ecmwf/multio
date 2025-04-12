#pragma once

#include <cstdint>
#include <map>
#include <memory>
#include <string>
#include <vector>


#include "multio/action/ChainedAction.h"
#include "multio/action/statistics-mtg2/cfg/StatisticsConfiguration.h"
#include "multio/action/statistics-mtg2/synoptic-filters/Filter.h"
#include "multio/config/ComponentConfiguration.h"
#include "multio/message/Message.h"


namespace multio::action::statistics_mtg2 {

// Filters without specific yaml configuration
std::unique_ptr<SynopticFilter> make_filter(const std::string& filterKind, const StatisticsConfiguration& cfg);

// Filters with specific yaml configuration
std::unique_ptr<SynopticFilter> make_filter(const std::string& filterKind, const eckit::LocalConfiguration& compConf,
                                            const StatisticsConfiguration& cfg);


}  // namespace multio::action::statistics_mtg2
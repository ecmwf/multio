#pragma once

#include <cstdint>


#include "eckit/config/LocalConfiguration.h"
#include "multio/action/statistics-mtg2/cfg/StatisticsConfiguration.h"
#include "multio/config/ComponentConfiguration.h"
#include "multio/message/Message.h"


namespace multio::action::statistics_mtg2 {

class SynopticFilter {
public:
    SynopticFilter() {};

    virtual ~SynopticFilter() {};

    virtual void fillMetadata(size_t idx, message::Metadata& metadata) const = 0;

    virtual std::string name() const = 0;

    virtual size_t size() const = 0;

    virtual bool match(const message::Message& msg, const StatisticsConfiguration& cfg, size_t& key) const = 0;
};

}  // namespace multio::action::statistics_mtg2

#pragma once

#include "multio/action/statistics/synoptic-filters/Filter.h"


namespace multio::action {

class AllTimesFilter final : public SynopticFilter {
public:
    AllTimesFilter() {};

    ~AllTimesFilter() {};

    size_t size() const { return static_cast<size_t>(1); };


    std::string name() const { return std::string{"AllTimesFilter"}; };

    void fillMetadata(size_t idx, message::Metadata& metadata) const {
        // long startStep = metadata.getLong( "startStep" );
        // metadata.setLong( startStep );
    };


    bool match(const message::Message& msg, const StatisticsConfiguration& cfg, size_t& key) const {
        key = 0;
        return true;
    };
};

}  // namespace multio::action

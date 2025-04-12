#pragma once

#include "multio/action/statistics-mtg2/synoptic-filters/Filter.h"


namespace multio::action::statistics_mtg2 {

class DailyHoursFilter final : public SynopticFilter {
public:
    DailyHoursFilter() {};
    ~DailyHoursFilter() {};

    size_t size() const { return static_cast<size_t>(24); };

    std::string name() const { return std::string{"DailyHoursFilter"}; };

    void fillMetadata(size_t idx, message::Metadata& metadata) const {
        // long startStep = metadata.getLong( "startStep" );
        // metadata.setLong( startStep+idx );
        return;
    };


    bool match(const message::Message& msg, const StatisticsConfiguration& cfg, size_t& key) const {
        eckit::DateTime now = currentDateTime(msg, cfg);
        key = (now.time().hhmmss() / 10000);
        return true;
    };
};

}  // namespace multio::action::statistics_mtg2

#include "SynopticFilters.h"

#include <iostream>
#include <ostream>

#include "eckit/exception/Exceptions.h"
#include "eckit/types/DateTime.h"

#include "multio/action/statistics-mtg2/TimeUtils.h"
#include "multio/config/ComponentConfiguration.h"

#include "multio/action/statistics-mtg2/synoptic-filters/AllTimesFilter.h"
#include "multio/action/statistics-mtg2/synoptic-filters/DailyCustomFilter.h"
#include "multio/action/statistics-mtg2/synoptic-filters/DailyHoursFilter.h"

namespace multio::action::statistics_mtg2 {


// Filters without specific yaml configuration
std::unique_ptr<SynopticFilter> make_filter(const std::string& filterKind, const StatisticsConfiguration& cfg) {

    if (filterKind == "NoFilter") {
        return std::make_unique<AllTimesFilter>();
    }

    if (filterKind == "DailyHours") {
        return std::make_unique<DailyHoursFilter>();
    }

    std::ostringstream os;
    os << "Invalid filter name in statistics :: " << filterKind << std::endl;
    throw eckit::UserError(os.str(), Here());
};


// Filters with specific yaml configuration
std::unique_ptr<SynopticFilter> make_filter(const std::string& filterKind, const eckit::LocalConfiguration& compConf,
                                            const StatisticsConfiguration& cfg) {

    if (filterKind == "DailyCustom") {
        return std::make_unique<DailyCustomFilter>(compConf, cfg);
    }

    std::ostringstream os;
    os << "Invalid filter name in statistics :: " << filterKind << std::endl;
    throw eckit::UserError(os.str(), Here());
};

}  // namespace multio::action::statistics_mtg2

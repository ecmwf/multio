#pragma once

#include <algorithm>
#include <ostream>

#include "eckit/exception/Exceptions.h"
#include "multio/LibMultio.h"

#include "multio/action/statistics/synoptic-filters/Filter.h"
#include "multio/config/ComponentConfiguration.h"


namespace multio::action {


namespace {


std::vector<long> initDailyCustomFilterReadArray(const eckit::LocalConfiguration& compConf,
                                                 const StatisticsConfiguration& cfg) {
    std::vector<long> tmp;
    LOG_DEBUG_LIB(LibMultio) << cfg.logPrefix() << " + Initialize new DailyCustomFilter with explicit configuration"
                             << std::endl;
    LOG_DEBUG_LIB(LibMultio) << cfg.logPrefix() << " + Config    :: " << compConf << std::endl;
    tmp = compConf.getLongVector("hours-set");
    std::sort(tmp.begin(), tmp.end());
    long old = -1;
    for (auto& i : tmp) {
        if (i < 0 || i > 23) {
            throw eckit::SeriousBug{"Index out of range", Here()};
        }
        if ((old - i) == 0) {
            throw eckit::SeriousBug{"Duplicte are no allowed", Here()};
        }
        old = i;
    }
    LOG_DEBUG_LIB(LibMultio) << cfg.logPrefix() << " + Hours set :: " << tmp << std::endl;
    return tmp;
};


std::vector<long> initDailyCustomFilterReadVars(const eckit::LocalConfiguration& compConf,
                                                const StatisticsConfiguration& cfg) {
    std::vector<long> tmp;
    LOG_DEBUG_LIB(LibMultio) << cfg.logPrefix() << " + Initialize new DailyCustomFilter with from/to/by configuration"
                             << std::endl;
    LOG_DEBUG_LIB(LibMultio) << cfg.logPrefix() << " + Config    :: " << compConf << std::endl;
    long from;
    long to;
    long by;
    from = compConf.getLong("from");
    to = compConf.getLong("to");
    by = compConf.getLong("by", 1);
    LOG_DEBUG_LIB(LibMultio) << cfg.logPrefix() << " + From    :: " << from << std::endl;
    LOG_DEBUG_LIB(LibMultio) << cfg.logPrefix() << " + To      :: " << to << std::endl;
    LOG_DEBUG_LIB(LibMultio) << cfg.logPrefix() << " + By      :: " << by << std::endl;
    if (by < 1 || by > 23) {
        throw eckit::SeriousBug{"\"by\" out of range", Here()};
    }
    if (from < 0 || from > 23) {
        throw eckit::SeriousBug{"\"to\" out of range", Here()};
    }
    if (to < 0 || to > 23) {
        throw eckit::SeriousBug{"\"from\" out of range", Here()};
    }
    for (long i = from; i <= to; i = i + by) {
        tmp.push_back(i);
    }
    LOG_DEBUG_LIB(LibMultio) << cfg.logPrefix() << " + Hours set :: " << tmp << std::endl;
    return tmp;
};


std::vector<long> initDailyCustomFilter(const eckit::LocalConfiguration& compConf, const StatisticsConfiguration& cfg) {
    if (compConf.has("hours-set")) {
        return initDailyCustomFilterReadArray(compConf, cfg);
    }

    if (compConf.has("from") && compConf.has("to")) {
        return initDailyCustomFilterReadVars(compConf, cfg);
    }

    std::ostringstream os;
    os << "Invalid configuration for the filter :: DailyCustomFilter" << std::endl;
    throw eckit::UserError(os.str(), Here());
};


std::array<long, 24> initReqHouorsInv(const std::vector<long>& reqHours) {
    std::array<long, 24> tmp;
    tmp.fill(static_cast<long>(9999999));
    for (long i = 0; i < reqHours.size(); ++i) {
        tmp[reqHours[i]] = i;
    }
    return tmp;
};


}  // namespace


class DailyCustomFilter final : public SynopticFilter {

    const std::vector<long> reqHours_;
    const std::array<long, 24> reqHoursInv_;

public:
    DailyCustomFilter(const eckit::LocalConfiguration& compConf, const StatisticsConfiguration& cfg) :
        reqHours_{initDailyCustomFilter(compConf, cfg)}, reqHoursInv_{initReqHouorsInv(reqHours_)} {};
    ~DailyCustomFilter() {};

    size_t size() const { return reqHours_.size(); };

    std::string name() const { return std::string{"DailyCustomFilter"}; };

    void fillMetadata(size_t idx, message::Metadata& metadata) const {
        // long startStep = metadata.getLong( "startStep" );
        // metadata.setLong( startStep+reqHours[idx] );
    };


    bool match(const message::Message& msg, const StatisticsConfiguration& cfg, size_t& key) const {
        eckit::DateTime now = currentDateTime(msg, cfg);
        int tid = static_cast<int>(now.time().hhmmss() / 10000);
        if (tid < 0 && tid >= 24) {
            std::ostringstream os;
            os << "Invalid time id from message :: DailyCustomFilter" << std::endl;
            throw eckit::UserError(os.str(), Here());
        }
        key = reqHoursInv_[tid];
        return (key < 24);
    };
};


}  // namespace multio::action

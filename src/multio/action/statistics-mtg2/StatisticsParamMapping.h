#pragma once

#include <map>

#include "multio/message/Metadata.h"


namespace multio::action::statistics_mtg2 {


class StatisticsParamMapping {

public:
    StatisticsParamMapping(StatisticsParamMapping& other) = delete;
    void operator=(const StatisticsParamMapping&) = delete;

    static StatisticsParamMapping* instance();

    std::int64_t getMapping(std::int64_t param, std::int64_t typeOfStatisticalProcessing);
    void applyMapping(message::Metadata& metadata, std::int64_t typeOfStatisticalProcessing);
    void applyMapping(message::Metadata& metadata, const std::string& opname);


private:
    StatisticsParamMapping();

    static StatisticsParamMapping* instance_;

    std::map<std::pair<std::int64_t, std::int64_t>, std::int64_t> mapping_;
};

StatisticsParamMapping* paramMapping();


}  // namespace multio::action::statistics_mtg2

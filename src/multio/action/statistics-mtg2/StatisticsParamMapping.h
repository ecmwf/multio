#pragma once

#include "multio/message/Metadata.h"


namespace multio::action::statistics_mtg2 {


namespace {
    struct ParamTypeOfStatisticalProcessingPair {
        std::int64_t param;
        std::int64_t typeOfStatisticalProcessing;

        bool operator==(const ParamTypeOfStatisticalProcessingPair& other) const {
            return param == other.param && typeOfStatisticalProcessing == other.typeOfStatisticalProcessing;
        }
    };

    // Hash function for the unordered_map with custom struct as key, in boost::hash_combine style
    struct ParamTypeOfStatisticalProcessingPairHash {
        std::size_t operator()(const ParamTypeOfStatisticalProcessingPair& p) const {
            std::size_t a = std::hash<std::int64_t>{}(p.param);
            std::size_t b = std::hash<std::int64_t>{}(p.typeOfStatisticalProcessing);
            return a ^ (b + 0x9e3779b9 + (a << 6) + (a >> 2));
        }
    };
}

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

    std::unordered_map<ParamTypeOfStatisticalProcessingPair, std::int64_t, ParamTypeOfStatisticalProcessingPairHash> mapping_;
};

StatisticsParamMapping* paramMapping();


}  // namespace multio::action::statistics_mtg2

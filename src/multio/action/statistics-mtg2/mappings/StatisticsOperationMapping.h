#pragma once

#include <cstdint>

#include <optional>
#include <string>
#include <unordered_map>


namespace multio::action::statistics_mtg2 {

namespace {
    using Param = std::int64_t;
    using TypeOfStatisticalProcessing = std::int64_t;
    using ParamToTypeOfStatisticalProcessingMap
        = std::unordered_map<Param, TypeOfStatisticalProcessing>;
}


class StatisticsOperationMapping {
public:
    static StatisticsOperationMapping makeStatisticsOperationMapping();

    std::optional<TypeOfStatisticalProcessing> getOperation(Param param) const;
    bool hasOperation(Param param, TypeOfStatisticalProcessing operation) const;
    bool hasOperation(Param param, std::string operation) const;

private:
    StatisticsOperationMapping(ParamToTypeOfStatisticalProcessingMap operationMappings);

    const ParamToTypeOfStatisticalProcessingMap operationMappings_;
};


}  // namespace multio::action::statistics_mtg2

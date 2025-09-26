#include "StatisticsOperationMapping.h"

#include "multio/LibMultio.h"

#include "eckit/config/LocalConfiguration.h"
#include "eckit/config/YAMLConfiguration.h"
#include "eckit/filesystem/PathName.h"
#include "eckit/exception/Exceptions.h"


namespace multio::action::statistics_mtg2 {

StatisticsOperationMapping::StatisticsOperationMapping(ParamToTypeOfStatisticalProcessingMap operationMappings) :
    operationMappings_{operationMappings} {}

StatisticsOperationMapping StatisticsOperationMapping::makeStatisticsOperationMapping() {
    eckit::LocalConfiguration mappingConf{eckit::YAMLConfiguration{eckit::PathName{
        multio::LibMultio::instance().libraryHome() + "/share/multio/mappings/statistics_operation_mappings.yaml"
    }}};

    ParamToTypeOfStatisticalProcessingMap operationMappings;
    for (auto& mapping : mappingConf.getSubConfigurations()) {
        auto param = mapping.getInt64("param");
        auto typeOfStatisticalProcessing = mapping.getInt64("typeOfStatisticalProcessing");
        operationMappings[param] = typeOfStatisticalProcessing;
    }
    return StatisticsOperationMapping(operationMappings);
}


std::optional<TypeOfStatisticalProcessing> StatisticsOperationMapping::getOperation(Param param) const {
    if (auto search = operationMappings_.find(param); search != operationMappings_.end()) {
        return search->second;
    }
    else {
        return std::nullopt;
    }
}

bool StatisticsOperationMapping::hasOperation(Param param, TypeOfStatisticalProcessing operation) const {
    auto actualOperation = getOperation(param);
    if (actualOperation) {
        return *actualOperation == operation;
    }
    return false;
}

// TODO: Create an ENUM TypeOfStatisticalProcessing with appropriate conversions to and from strings and integers
bool StatisticsOperationMapping::hasOperation(Param param, std::string operation) const {
    std::cout << "operation=" << operation;
    if (operation == "average")            { return hasOperation(param, 0); }
    if (operation == "accumulate")         { return hasOperation(param, 1); }
    if (operation == "maximum")            { return hasOperation(param, 2); }
    if (operation == "minimum")            { return hasOperation(param, 3); }
    if (operation == "difference")         { return hasOperation(param, 4); }
    if (operation == "stddev")             { return hasOperation(param, 6); }
    if (operation == "inverse-difference") { return hasOperation(param, 8); }

    std::ostringstream os;
    os << "TypeOfStatisticalProcessing of operation " << operation << " is undefined!" << std::endl;
    throw eckit::SeriousBug(os.str(), Here());
}


}  // namespace multio::action::statistics_mtg2

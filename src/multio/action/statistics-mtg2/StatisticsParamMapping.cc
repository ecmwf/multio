#include "StatisticsParamMapping.h"

#include "multio/LibMultio.h"
#include "multio/message/Glossary.h"

#include "eckit/config/LocalConfiguration.h"
#include "eckit/config/YAMLConfiguration.h"
#include "eckit/exception/Exceptions.h"


namespace multio::action::statistics_mtg2 {


using message::glossary;


StatisticsParamMapping::StatisticsParamMapping(ParamTypeOfStatisticalProcessingToParamMap paramMappings) :
    paramMappings_{paramMappings} {}

StatisticsParamMapping StatisticsParamMapping::makeStatisticsParamMapping() {
    // TODO : This path might not always be correct?
    eckit::LocalConfiguration mappingConf{eckit::YAMLConfiguration{eckit::PathName{
        multio::LibMultio::instance().libraryHome() +
        "/multio/share/multio/statistics-mtg2/statistics_param_mappings.yml"
    }}};

    ParamTypeOfStatisticalProcessingToParamMap paramMappings;
    for (auto& mappings : mappingConf.getSubConfigurations()) {
        auto paramIn = mappings.getInt64("paramIn");
        for (auto& mapping : mappings.getSubConfigurations("mappings")) {
            auto typeOfStatisticalProcessing = mapping.getInt64("typeOfStatisticalProcessing");
            auto paramOut = mapping.getInt64("paramOut");
            paramMappings[{paramIn, typeOfStatisticalProcessing}] = paramOut;
        }
    }
    return StatisticsParamMapping(paramMappings);
}

std::int64_t StatisticsParamMapping::getMapping(std::int64_t param, std::int64_t typeOfStatisticalProcessing) const {
    if (auto search = paramMappings_.find({param, typeOfStatisticalProcessing}); search != paramMappings_.end()) {
        return search->second;
    }

    std::ostringstream os;
    os << "Mapping for param=" << param << " and typeOfStatisticalProcessing=" << typeOfStatisticalProcessing << " is undefined!" << std::endl;
    throw eckit::SeriousBug(os.str() , Here());
}

void StatisticsParamMapping::applyMapping(message::Metadata& metadata, std::int64_t typeOfStatisticalProcessing) const {
    if (auto paramOld = metadata.getOpt<std::int64_t>(glossary().param); paramOld) {
        auto paramNew = getMapping(*paramOld, typeOfStatisticalProcessing);
        metadata.set(glossary().param, paramNew);
        return;
    }

    throw eckit::SeriousBug("Metadata does not contain param!", Here());
}

// TODO : Put the typeOfStatisticalProcessing in the operations
namespace {
const std::map<const std::string, const std::int64_t> opname_to_typeOfStatisticalProcessing {
    {"average", 0}, {"accumulate", 1}, {"maximum", 2}, {"minimum", 3}, {"difference", 4}, {"stddev", 6}, {"inverse-difference", 8}};
}

// NOTE : This function will not apply a mapping, nor throw an exception, if the operation is not an official
//        type of statistical processing as defined in table 4.10.
void StatisticsParamMapping::applyMapping(message::Metadata& metadata, const std::string& opname) const {
    if (auto search = opname_to_typeOfStatisticalProcessing.find(opname); search != opname_to_typeOfStatisticalProcessing.end()) {
        applyMapping(metadata, search->second);
    }
}


}  // namespace multio::action::statistics_mtg2

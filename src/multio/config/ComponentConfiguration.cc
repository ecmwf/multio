
#include "multio/config/ComponentConfiguration.h"

#include "multio/util/Environment.h"


using namespace multio::util;

namespace multio::config {

SubComponentIteratorMapper::SubComponentIteratorMapper(const ComponentConfiguration& compConf) : compConf_(compConf) {}
SubComponentIteratorMapper::SubComponentIteratorMapper(ComponentConfiguration&& compConf) :
    compConf_(std::move(compConf)) {}

ComponentConfiguration SubComponentIteratorMapper::operator()(const eckit::LocalConfiguration& config) const {
    return ComponentConfiguration(config, compConf_.multioConfig(), compConf_.planName());
}


//-----------------------------------------------------------------------------

ComponentConfiguration::ComponentConfiguration(const eckit::LocalConfiguration& componentConfig,
                                               const MultioConfiguration& multioConf) :
    ComponentConfiguration(componentConfig, multioConf, std::nullopt) {}

ComponentConfiguration::ComponentConfiguration(const eckit::LocalConfiguration& componentConfig,
                                               const MultioConfiguration& multioConf,
                                               std::optional<std::string> planName) :
    componentConf_(componentConfig), multioConf_(multioConf), planName_(std::move(planName)) {}

//-----------------------------------------------------------------------------

eckit::LocalConfiguration& ComponentConfiguration::parsedConfig() {
    return componentConf_;
};
const eckit::LocalConfiguration& ComponentConfiguration::parsedConfig() const {
    return componentConf_;
};

const MultioConfiguration& ComponentConfiguration::multioConfig() const {
    return multioConf_.get();
};

const std::optional<std::string>& ComponentConfiguration::planName() const {
    return planName_;
};


//-----------------------------------------------------------------------------

ComponentConfiguration ComponentConfiguration::subComponent(const std::string& subConfiguratinKey) const {
    return ComponentConfiguration(componentConf_.getSubConfiguration(subConfiguratinKey), multioConfig(), planName_);
};

ComponentConfiguration::SubComponentConfigurations ComponentConfiguration::subComponents(
    const std::string& subConfiguratinKey) const {
    return SubComponentConfigurations(componentConf_.getSubConfigurations(subConfiguratinKey),
                                      SubComponentIteratorMapper(*this));
};


//-----------------------------------------------------------------------------

}  // namespace multio::config

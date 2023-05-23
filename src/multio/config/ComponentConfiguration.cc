
#include "multio/config/ComponentConfiguration.h"

#include "multio/util/Environment.h"


using namespace multio::util;

namespace multio::config {

SubComponentIteratorMapper::SubComponentIteratorMapper(const ComponentConfiguration& compConf, ComponentTag tag) :
    compConf_(compConf), tag_(tag) {}
SubComponentIteratorMapper::SubComponentIteratorMapper(ComponentConfiguration&& compConf, ComponentTag tag) :
    compConf_(std::move(compConf)), tag_(tag) {}

ComponentConfiguration SubComponentIteratorMapper::operator()(const eckit::LocalConfiguration& config) const {
    return compConf_.recast(config, tag_);
}


//=============================================================================

ComponentConfiguration::ComponentConfiguration(const eckit::LocalConfiguration& componentYAMLConfig,
                                               const MultioConfiguration& multioConf, ComponentTag tag) :
    componentConf_(componentYAMLConfig), multioConf_(multioConf), componentTag_(tag){};

//=============================================================================

eckit::LocalConfiguration& ComponentConfiguration::parsedConfig() {
    return componentConf_;
};
const eckit::LocalConfiguration& ComponentConfiguration::parsedConfig() const {
    return componentConf_;
};

const MultioConfiguration& ComponentConfiguration::multioConfig() const {
    return multioConf_.get();
};


//=============================================================================

ComponentConfiguration ComponentConfiguration::subComponent(const std::string& subConfiguratinKey,
                                                            ComponentTag tag) const {
    return recast(componentConf_.getSubConfiguration(subConfiguratinKey), tag);
};

ComponentConfiguration::SubComponentConfigurations ComponentConfiguration::subComponents(
    const std::string& subConfiguratinKey, ComponentTag tag) const {
    return SubComponentConfigurations(componentConf_.getSubConfigurations(subConfiguratinKey),
                                      SubComponentIteratorMapper(*this, tag));
};


//=============================================================================

ComponentConfiguration ComponentConfiguration::recast(const eckit::LocalConfiguration& componentYAMLConfig,
                                                      ComponentTag tag) const {
    return ComponentConfiguration(componentYAMLConfig, multioConf_, tag);
};

ComponentConfiguration ComponentConfiguration::recast(ComponentTag tag) const {
    return ComponentConfiguration(componentConf_, multioConf_, tag);
};


//=============================================================================

ComponentTag ComponentConfiguration::componentTag() const {
    return componentTag_;
};
void ComponentConfiguration::setComponentTag(ComponentTag tag) {
    componentTag_ = tag;
};


//=============================================================================

bool ComponentConfiguration::isServer() const {
    return multioConfig().isServer();
};
bool ComponentConfiguration::isClient() const {
    return multioConfig().isClient();
};

//=============================================================================

}  // namespace multio::config

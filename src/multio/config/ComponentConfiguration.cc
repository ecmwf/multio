
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
                                               std::shared_ptr<MultioConfiguration> multioConf, ComponentTag tag) :
    componentConf_(componentYAMLConfig), multioConf_(std::move(multioConf)), componentTag_(tag){};


ComponentConfiguration::ComponentConfiguration(const eckit::LocalConfiguration& componentYAMLConfig,
                                               const eckit::LocalConfiguration& globalYAMLConfig,
                                               const eckit::PathName& pathName, const eckit::PathName& fileName,
                                               LocalPeerTag localPeerTag, ComponentTag tag) :
    ComponentConfiguration::ComponentConfiguration(
        componentYAMLConfig, std::make_shared<MultioConfiguration>(globalYAMLConfig, pathName, fileName, localPeerTag),
        tag) {}

ComponentConfiguration::ComponentConfiguration(const eckit::LocalConfiguration& config, const eckit::PathName& pathName,
                                               const eckit::PathName& fileName, LocalPeerTag localPeerTag,
                                               ComponentTag tag) :
    ComponentConfiguration::ComponentConfiguration(config, config, pathName, fileName, localPeerTag, tag) {}

ComponentConfiguration::ComponentConfiguration(const eckit::PathName& pathName, const eckit::PathName& fileName,
                                               LocalPeerTag localPeerTag, ComponentTag tag) :
    ComponentConfiguration::ComponentConfiguration(eckit::LocalConfiguration{eckit::YAMLConfiguration{fileName}},
                                                   pathName, fileName, localPeerTag, tag) {}

ComponentConfiguration::ComponentConfiguration(const eckit::PathName& fileName, LocalPeerTag localPeerTag,
                                               ComponentTag tag) :
    ComponentConfiguration::ComponentConfiguration(eckit::LocalConfiguration{eckit::YAMLConfiguration{fileName}},
                                                   configuration_path_name(fileName), fileName, localPeerTag, tag) {}


//=============================================================================

eckit::LocalConfiguration& ComponentConfiguration::YAML() {
    return componentConf_;
};
const eckit::LocalConfiguration& ComponentConfiguration::YAML() const {
    return componentConf_;
};

MultioConfiguration& ComponentConfiguration::multioConfig() {
    return *multioConf_.get();
};
const MultioConfiguration& ComponentConfiguration::multioConfig() const {
    return *multioConf_.get();
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
ComponentConfiguration& ComponentConfiguration::setComponentTag(ComponentTag tag) {
    componentTag_ = tag;
    return *this;
};


//=============================================================================

bool ComponentConfiguration::isServer() const {
    return multioConfig().isServer();
};
bool ComponentConfiguration::isClient() const {
    return multioConfig().isClient();
};

ComponentConfiguration& ComponentConfiguration::tagServer() {
    multioConfig().setLocalPeerTag(LocalPeerTag::Server);
    return setComponentTag(ComponentTag::Server);
};
ComponentConfiguration& ComponentConfiguration::tagClient() {
    multioConfig().setLocalPeerTag(LocalPeerTag::Client);
    return setComponentTag(ComponentTag::Client);
};


//=============================================================================

}  // namespace multio::config

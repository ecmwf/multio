
#include "ConfigurationContext.h"

namespace multio {
namespace util {

std::string toString(ComponentTag tag) {
    switch (tag) {
        case ComponentTag::Unrelated:
            return "Unrelated";
        case ComponentTag::Client:
            return "Client";
        case ComponentTag::Server:
            return "Server";
        case ComponentTag::Plan:
            return "Plan";
        case ComponentTag::Action:
            return "Action";
        case ComponentTag::Transport:
            return "Transport";
        case ComponentTag::Receiver:
            return "Receiver";
        case ComponentTag::Dispatcher:
            return "Dispatcher";
        default:
            return "Unknown component tag";
    }
}

std::string toString(LocalPeerTag tag) {
    switch (tag) {
        case LocalPeerTag::Client:
            return "Client";
        case LocalPeerTag::Server:
            return "Server";
        default:
            return "Unknown local peer tag";
    }
}


SubContextIteratorMapper::SubContextIteratorMapper(const ConfigurationContext& confCtx, ComponentTag tag) :
    confCtx_(confCtx), tag_(tag) {}
SubContextIteratorMapper::SubContextIteratorMapper(ConfigurationContext&& confCtx, ComponentTag tag) :
    confCtx_(std::move(confCtx)), tag_(tag) {}

ConfigurationContext SubContextIteratorMapper::operator()(
    const eckit::LocalConfiguration& config) const {
    return confCtx_.recast(config, tag_);
}


// GlobalConfCtx
GlobalConfCtx::GlobalConfCtx(const eckit::LocalConfiguration& config,
                             const eckit::PathName& pathName, const eckit::PathName& fileName,
                             LocalPeerTag localPeerTag) :
    globalConfig_(config), pathName_(pathName), fileName_(fileName), localPeerTag_(localPeerTag) {}

GlobalConfCtx::GlobalConfCtx(const eckit::PathName& pathName, const eckit::PathName& fileName,
                             LocalPeerTag localPeerTag) :
    GlobalConfCtx::GlobalConfCtx(eckit::LocalConfiguration{eckit::YAMLConfiguration{fileName}},
                                 pathName, fileName, localPeerTag) {}

GlobalConfCtx::GlobalConfCtx(const eckit::PathName& fileName, LocalPeerTag localPeerTag) :
    GlobalConfCtx::GlobalConfCtx(eckit::LocalConfiguration{eckit::YAMLConfiguration{fileName}},
                                 configuration_path_name(), fileName, localPeerTag) {}

const eckit::LocalConfiguration& GlobalConfCtx::globalConfig() const {
    return globalConfig_;
};
const eckit::PathName& GlobalConfCtx::pathName() const {
    return pathName_;
};
void GlobalConfCtx::setPathName(const eckit::PathName& pathName) {
    pathName_ = pathName;
};
const eckit::PathName& GlobalConfCtx::fileName() const {
    return fileName_;
};

LocalPeerTag GlobalConfCtx::localPeerTag() const {
    return localPeerTag_;
};
void GlobalConfCtx::setLocalPeerTag(LocalPeerTag clientOrServer) {
    localPeerTag_ = clientOrServer;
};

const eckit::Optional<MPIInitInfo>& GlobalConfCtx::getMPIInitInfo() const {
    return mpiInitInfo_;
};
eckit::Optional<MPIInitInfo>& GlobalConfCtx::getMPIInitInfo() {
    return mpiInitInfo_;
};
void GlobalConfCtx::setMPIInitInfo(const eckit::Optional<MPIInitInfo>& val) {
    mpiInitInfo_ = val;
};


// ConfigurationContext
ConfigurationContext::ConfigurationContext(const eckit::LocalConfiguration& config,
                                           std::shared_ptr<GlobalConfCtx> globalConfCtx,
                                           ComponentTag tag) :
    config_(config), globalConfCtx_(std::move(globalConfCtx)), componentTag_(tag){};


ConfigurationContext::ConfigurationContext(const eckit::LocalConfiguration& config,
                                           const eckit::LocalConfiguration& globalConfig,
                                           const eckit::PathName& pathName,
                                           const eckit::PathName& fileName,
                                           LocalPeerTag localPeerTag,
                                           ComponentTag tag) :
    ConfigurationContext::ConfigurationContext(
        config, std::shared_ptr<GlobalConfCtx>{
                    new GlobalConfCtx(globalConfig, pathName, fileName, localPeerTag)}, tag) {}

ConfigurationContext::ConfigurationContext(const eckit::LocalConfiguration& config,
                                           const eckit::PathName& pathName,
                                           const eckit::PathName& fileName,
                                           LocalPeerTag localPeerTag,
                                           ComponentTag tag) :
    ConfigurationContext::ConfigurationContext(config, config, pathName, fileName, localPeerTag, tag) {}

ConfigurationContext::ConfigurationContext(const eckit::PathName& pathName,
                                           const eckit::PathName& fileName,
                                           LocalPeerTag localPeerTag,
                                           ComponentTag tag) :
    ConfigurationContext::ConfigurationContext(
        eckit::LocalConfiguration{eckit::YAMLConfiguration{fileName}}, pathName, fileName,
        localPeerTag, tag) {}

ConfigurationContext::ConfigurationContext(const eckit::PathName& fileName,
                                           LocalPeerTag localPeerTag,
                                           ComponentTag tag) :
    ConfigurationContext::ConfigurationContext(
        eckit::LocalConfiguration{eckit::YAMLConfiguration{fileName}}, configuration_path_name(),
        fileName, localPeerTag, tag) {}

eckit::LocalConfiguration& ConfigurationContext::config() {
    return config_;
};
const eckit::LocalConfiguration& ConfigurationContext::config() const {
    return config_;
};

const eckit::LocalConfiguration& ConfigurationContext::globalConfig() const {
    return globalConfCtx_->globalConfig();
};

const eckit::PathName& ConfigurationContext::pathName() const {
    return globalConfCtx_->pathName();
};

ConfigurationContext& ConfigurationContext::setPathName(const eckit::PathName& pathName) {
    globalConfCtx_->setPathName(pathName);
    return *this;
};

const eckit::PathName& ConfigurationContext::fileName() const {
    return globalConfCtx_->fileName();
};

ConfigurationContext ConfigurationContext::subContext(const std::string& subConfiguratinKey, ComponentTag tag) const {
    return recast(config_.getSubConfiguration(subConfiguratinKey), tag);
};

ConfigurationContext::SubConfigurationContexts ConfigurationContext::subContexts(
    const std::string& subConfiguratinKey, ComponentTag tag) const {
    return SubConfigurationContexts(config_.getSubConfigurations(subConfiguratinKey),
                                    SubContextIteratorMapper(*this, tag));
};

ConfigurationContext ConfigurationContext::recast(const eckit::LocalConfiguration& config, ComponentTag tag) const {
    return ConfigurationContext(config, globalConfCtx_, tag);
};

ConfigurationContext ConfigurationContext::recast(ComponentTag tag) const {
    return ConfigurationContext(config_, globalConfCtx_, tag);
};

// ComponentTag
ComponentTag ConfigurationContext::componentTag() const {
    return componentTag_;
};
ConfigurationContext& ConfigurationContext::setComponentTag(ComponentTag tag) {
    componentTag_ = tag;
    return  *this;
};

// LocalPeerTag
LocalPeerTag ConfigurationContext::localPeerTag() const {
    return globalConfCtx_->localPeerTag();
};
bool ConfigurationContext::isServer() const {
    return localPeerTag() == LocalPeerTag::Server;
};
bool ConfigurationContext::isClient() const {
    return localPeerTag() == LocalPeerTag::Client;
};

ConfigurationContext& ConfigurationContext::setLocalPeerTag(LocalPeerTag clientOrServer) {
    globalConfCtx_->setLocalPeerTag(clientOrServer);
    return *this;
};
ConfigurationContext& ConfigurationContext::tagServer() {
    return setComponentTag(ComponentTag::Server).setLocalPeerTag(LocalPeerTag::Server);
};
ConfigurationContext& ConfigurationContext::tagClient() {
    return setComponentTag(ComponentTag::Client).setLocalPeerTag(LocalPeerTag::Client);
};


// MPIInitInfo
const eckit::Optional<MPIInitInfo>& ConfigurationContext::getMPIInitInfo() const {
    return globalConfCtx_->getMPIInitInfo();
}
eckit::Optional<MPIInitInfo>& ConfigurationContext::getMPIInitInfo() {
    return globalConfCtx_->getMPIInitInfo();
}
ConfigurationContext& ConfigurationContext::setMPIInitInfo(
    const eckit::Optional<MPIInitInfo>& val) {
    globalConfCtx_->setMPIInitInfo(val);
    return *this;
}

}  // namespace util
}  // namespace multio

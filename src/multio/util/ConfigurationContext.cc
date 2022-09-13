
#include "ConfigurationContext.h"

namespace multio {
namespace util {

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


SubContextIteratorMapper::SubContextIteratorMapper(const ConfigurationContext& confCtx) :
    confCtx_(confCtx) {}
SubContextIteratorMapper::SubContextIteratorMapper(ConfigurationContext&& confCtx) :
    confCtx_(std::move(confCtx)) {}

ConfigurationContext SubContextIteratorMapper::operator()(
    const eckit::LocalConfiguration& config) const {
    return confCtx_.recast(config);
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

const eckit::LocalConfiguration& GlobalConfCtx::globalConfig() const {
    return globalConfig_;
};
const eckit::PathName& GlobalConfCtx::pathName() const {
    return pathName_;
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
void GlobalConfCtx::setMPIInitInfo(const eckit::Optional<MPIInitInfo>& val) {
    mpiInitInfo_ = val;
};


// ConfigurationContext
ConfigurationContext::ConfigurationContext(const eckit::LocalConfiguration& config,
                                           std::shared_ptr<GlobalConfCtx> globalConfCtx) :
    config_(config), globalConfCtx_(std::move(globalConfCtx)){};


ConfigurationContext::ConfigurationContext(const eckit::LocalConfiguration& config,
                                           const eckit::LocalConfiguration& globalConfig,
                                           const eckit::PathName& pathName,
                                           const eckit::PathName& fileName,
                                           LocalPeerTag localPeerTag) :
    ConfigurationContext::ConfigurationContext(
        config,
        std::shared_ptr<GlobalConfCtx>{new GlobalConfCtx(globalConfig, pathName, fileName, localPeerTag)}) {
}

ConfigurationContext::ConfigurationContext(const eckit::LocalConfiguration& config,
                                           const eckit::PathName& pathName,
                                           const eckit::PathName& fileName,
                                           LocalPeerTag localPeerTag) :
    ConfigurationContext::ConfigurationContext(config, config, pathName, fileName, localPeerTag) {}

ConfigurationContext::ConfigurationContext(const eckit::PathName& pathName,
                                           const eckit::PathName& fileName,
                                           LocalPeerTag localPeerTag) :
    ConfigurationContext::ConfigurationContext(
        eckit::LocalConfiguration{eckit::YAMLConfiguration{fileName}}, pathName, fileName,
        localPeerTag) {}

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

const eckit::PathName& ConfigurationContext::fileName() const {
    return globalConfCtx_->fileName();
};

ConfigurationContext ConfigurationContext::subContext(const std::string& subConfiguratinKey) const {
    return recast(config_.getSubConfiguration(subConfiguratinKey));
};

ConfigurationContext::SubConfigurationContexts ConfigurationContext::subContexts(
    const std::string& subConfiguratinKey) const {
    return SubConfigurationContexts(config_.getSubConfigurations(subConfiguratinKey),
                                    SubContextIteratorMapper(*this));
};

ConfigurationContext ConfigurationContext::recast(const eckit::LocalConfiguration& config) const {
    return ConfigurationContext(config, globalConfCtx_);
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
    return setLocalPeerTag(LocalPeerTag::Server);
};
ConfigurationContext& ConfigurationContext::tagClient() {
    return setLocalPeerTag(LocalPeerTag::Client);
};


// MPIInitInfo
const eckit::Optional<MPIInitInfo>& ConfigurationContext::getMPIInitInfo() const {
    return globalConfCtx_->getMPIInitInfo();
}
ConfigurationContext& ConfigurationContext::setMPIInitInfo(const eckit::Optional<MPIInitInfo>& val) {
    globalConfCtx_->setMPIInitInfo(val);
    return *this;
}

}  // namespace util
}  // namespace multio

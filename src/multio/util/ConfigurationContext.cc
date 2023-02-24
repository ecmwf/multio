
#include "ConfigurationContext.h"
#include "MetadataMappings.h"

using namespace multio::util;

std::string eckit::Translator<ComponentTag, std::string>::operator()(ComponentTag tag) {
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

std::string eckit::Translator<LocalPeerTag, std::string>::operator()(LocalPeerTag tag) {
    switch (tag) {
        case LocalPeerTag::Client:
            return "Client";
        case LocalPeerTag::Server:
            return "Server";
        default:
            return "Unknown local peer tag";
    }
}

namespace multio {
namespace util {


SubContextIteratorMapper::SubContextIteratorMapper(const ConfigurationContext& confCtx, ComponentTag tag) :
    confCtx_(confCtx), tag_(tag) {}
SubContextIteratorMapper::SubContextIteratorMapper(ConfigurationContext&& confCtx, ComponentTag tag) :
    confCtx_(std::move(confCtx)), tag_(tag) {}

ConfigurationContext SubContextIteratorMapper::operator()(const eckit::LocalConfiguration& config) const {
    return confCtx_.recast(config, tag_);
}


// GlobalConfCtx
GlobalConfCtx::GlobalConfCtx(const eckit::LocalConfiguration& config, const eckit::PathName& pathName,
                             const eckit::PathName& fileName, LocalPeerTag localPeerTag) :
    globalConfig_(config), pathName_(pathName), fileName_(fileName), localPeerTag_(localPeerTag) {}

GlobalConfCtx::GlobalConfCtx(const eckit::PathName& pathName, const eckit::PathName& fileName,
                             LocalPeerTag localPeerTag) :
    GlobalConfCtx::GlobalConfCtx(eckit::LocalConfiguration{eckit::YAMLConfiguration{fileName}}, pathName, fileName,
                                 localPeerTag) {}

GlobalConfCtx::GlobalConfCtx(const eckit::PathName& fileName, LocalPeerTag localPeerTag) :
    GlobalConfCtx::GlobalConfCtx(eckit::LocalConfiguration{eckit::YAMLConfiguration{fileName}},
                                 configuration_path_name(fileName), fileName, localPeerTag) {}

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

const YAMLFile& GlobalConfCtx::getYAMLFile(const char* fname) const {
    return getYAMLFile(std::string(fname));
}
const YAMLFile& GlobalConfCtx::getYAMLFile(const std::string& fname) const {
    return getYAMLFile(eckit::PathName{replaceCurly(fname)});
}
const YAMLFile& GlobalConfCtx::getRelativeYAMLFile(const eckit::PathName& referedFrom, const char* fname) const {
    return getRelativeYAMLFile(referedFrom, std::string(fname));
}
const YAMLFile& GlobalConfCtx::getRelativeYAMLFile(const eckit::PathName& referedFrom, const std::string& fname) const {
    return getYAMLFile(referedFrom / fname);
}
const YAMLFile& GlobalConfCtx::getYAMLFile(const eckit::PathName& fname) const {
    eckit::PathName path = fname.fullName();
    std::string key = path.asString();
    auto config = referencedConfigFiles_.find(key);
    if (config != referencedConfigFiles_.end()) {
        return config->second;
    }

    referencedConfigFiles_.emplace(
        std::piecewise_construct, std::forward_as_tuple(key),
        std::forward_as_tuple(YAMLFile{eckit::LocalConfiguration{eckit::YAMLConfiguration{fname}}, path}));
    return referencedConfigFiles_[key];
}

// TODO:
// Currently we replace {~} with the configured through MULTIO_SERVER_CONFIG_PATH (which might be the basepath of MULTIO_SERVER_CONFIG_FILE).
// All other names are looked up in the environment directly.
//
// Usually we would use eckit::Resource, however we have not adopted the usage of a multio home (i.e. /etc/multio) yet and probably don't want to -
// alot of other users probably don't want to adopt this approach.
// Moreover to allow looking environment variables or cli arguments, for eckit::Resource would enforce us to construct a string like "var;$var;-var" which
// will be reparsed again instead of passing 3 arguments directly...
std::string GlobalConfCtx::replaceCurly(const std::string& s) const {
    return ::replaceCurly(s, [this](std::string_view replace){
        if (replace == "~") {
            return eckit::Optional<std::string>{this->pathName_.asString()};
        }
        std::string lookUpKey{replace};
        char* env = ::getenv(lookUpKey.c_str());
        if (env) {
            return eckit::Optional<std::string>{env};
        }
        else {
            return eckit::Optional<std::string>{};
        }
    });
}

const MetadataMappings& GlobalConfCtx::metadataMappings() const {
    if (!metadataMappings_) {
        metadataMappings_.emplace(*this);
    }
    return *metadataMappings_;
};


// ConfigurationContext
ConfigurationContext::ConfigurationContext(const eckit::LocalConfiguration& config,
                                           std::shared_ptr<GlobalConfCtx> globalConfCtx, ComponentTag tag) :
    config_(config), globalConfCtx_(std::move(globalConfCtx)), componentTag_(tag){};


ConfigurationContext::ConfigurationContext(const eckit::LocalConfiguration& config,
                                           const eckit::LocalConfiguration& globalConfig,
                                           const eckit::PathName& pathName, const eckit::PathName& fileName,
                                           LocalPeerTag localPeerTag, ComponentTag tag) :
    ConfigurationContext::ConfigurationContext(
        config, std::make_shared<GlobalConfCtx>(globalConfig, pathName, fileName, localPeerTag),
        tag) {}

ConfigurationContext::ConfigurationContext(const eckit::LocalConfiguration& config, const eckit::PathName& pathName,
                                           const eckit::PathName& fileName, LocalPeerTag localPeerTag,
                                           ComponentTag tag) :
    ConfigurationContext::ConfigurationContext(config, config, pathName, fileName, localPeerTag, tag) {}

ConfigurationContext::ConfigurationContext(const eckit::PathName& pathName, const eckit::PathName& fileName,
                                           LocalPeerTag localPeerTag, ComponentTag tag) :
    ConfigurationContext::ConfigurationContext(eckit::LocalConfiguration{eckit::YAMLConfiguration{fileName}}, pathName,
                                               fileName, localPeerTag, tag) {}

ConfigurationContext::ConfigurationContext(const eckit::PathName& fileName, LocalPeerTag localPeerTag,
                                           ComponentTag tag) :
    ConfigurationContext::ConfigurationContext(eckit::LocalConfiguration{eckit::YAMLConfiguration{fileName}},
                                               configuration_path_name(fileName), fileName, localPeerTag, tag) {}

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

ConfigurationContext::SubConfigurationContexts ConfigurationContext::subContexts(const std::string& subConfiguratinKey,
                                                                                 ComponentTag tag) const {
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
    return *this;
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
ConfigurationContext& ConfigurationContext::setMPIInitInfo(const eckit::Optional<MPIInitInfo>& val) {
    globalConfCtx_->setMPIInitInfo(val);
    return *this;
}

// Referenced fileds
const YAMLFile& ConfigurationContext::getYAMLFile(const char* fname) const {
    return globalConfCtx_->getYAMLFile(fname);
}
const YAMLFile& ConfigurationContext::getYAMLFile(const std::string& fname) const {
    return globalConfCtx_->getYAMLFile(fname);
}
const YAMLFile& ConfigurationContext::getYAMLFile(const eckit::PathName& fname) const {
    return globalConfCtx_->getYAMLFile(fname);
}
const YAMLFile& ConfigurationContext::getRelativeYAMLFile(const eckit::PathName& referedFrom, const char* fname) const {
    return globalConfCtx_->getRelativeYAMLFile(referedFrom, fname);
}
const YAMLFile& ConfigurationContext::getRelativeYAMLFile(const eckit::PathName& referedFrom,
                                                          const std::string& fname) const {
    return globalConfCtx_->getRelativeYAMLFile(referedFrom, fname);
}
std::string ConfigurationContext::replaceCurly(const std::string& s) const {
    return globalConfCtx_->replaceCurly(s);
}


const MetadataMappings& ConfigurationContext::metadataMappings() const {
    return globalConfCtx_->metadataMappings();
}

}  // namespace util
}  // namespace multio

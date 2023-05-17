
#include "multio/config/MultioConfiguration.h"
#include "multio/config/MetadataMappings.h"

#include "multio/util/Environment.h"
#include "multio/util/Substitution.h"


using namespace multio::config;

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

namespace multio::config {

// MultioConfiguration
MultioConfiguration::MultioConfiguration(const eckit::LocalConfiguration& globalConfig,
                                         const eckit::PathName& configDir, const eckit::PathName& configFile,
                                         LocalPeerTag localPeerTag) :
    parsedConfig_{globalConfig},
    configDir_{configDir},
    configFile_{configFile},
    localPeerTag_{localPeerTag},
    cFailureInteroperator_{std::make_shared<CFailureInteroperator>()} {}

MultioConfiguration::MultioConfiguration(const eckit::PathName& configDir, const eckit::PathName& configFile,
                                         LocalPeerTag localPeerTag) :
    MultioConfiguration::MultioConfiguration(eckit::LocalConfiguration{eckit::YAMLConfiguration{configFile}}, configDir,
                                             configFile, localPeerTag) {}

MultioConfiguration::MultioConfiguration(const eckit::PathName& configFile, LocalPeerTag localPeerTag) :
    MultioConfiguration::MultioConfiguration(eckit::LocalConfiguration{eckit::YAMLConfiguration{configFile}},
                                             configuration_path_name(configFile), configFile, localPeerTag) {}

MultioConfiguration::MultioConfiguration(const eckit::LocalConfiguration& globalConfig, LocalPeerTag localPeerTag) :
    parsedConfig_{globalConfig}, configDir_{}, configFile_{}, localPeerTag_{localPeerTag} {}


eckit::LocalConfiguration& MultioConfiguration::parsedConfig() {
    return parsedConfig_;
};
const eckit::LocalConfiguration& MultioConfiguration::parsedConfig() const {
    return parsedConfig_;
};
const eckit::PathName& MultioConfiguration::configDir() const {
    return configDir_;
};
void MultioConfiguration::setConfigDir(const eckit::PathName& configDir) {
    configDir_ = configDir;
};
const eckit::PathName& MultioConfiguration::configFile() const {
    return configFile_;
};

LocalPeerTag MultioConfiguration::localPeerTag() const {
    return localPeerTag_;
};

void MultioConfiguration::setLocalPeerTag(LocalPeerTag clientOrServer) {
    localPeerTag_ = clientOrServer;
};


const std::optional<MPIInitInfo>& MultioConfiguration::getMPIInitInfo() const {
    return mpiInitInfo_;
};
std::optional<MPIInitInfo>& MultioConfiguration::getMPIInitInfo() {
    return mpiInitInfo_;
};
void MultioConfiguration::setMPIInitInfo(const std::optional<MPIInitInfo>& val) {
    mpiInitInfo_ = val;
};

const ConfigFile& MultioConfiguration::getConfigFile(const std::string& fname) const {
    return getConfigFile(eckit::PathName{replaceCurly(fname)});
}
const ConfigFile& MultioConfiguration::getConfigFile(const eckit::PathName& fname) const {
    eckit::PathName path = fname.fullName();
    std::string key = path.asString();
    auto config = referencedConfigFiles_.find(key);
    if (config != referencedConfigFiles_.end()) {
        return config->second;
    }

    referencedConfigFiles_.emplace(
        std::piecewise_construct, std::forward_as_tuple(key),
        std::forward_as_tuple(ConfigFile{eckit::LocalConfiguration{eckit::YAMLConfiguration{fname}}, path}));
    return referencedConfigFiles_[key];
}

// TODO:
// Currently we replace {~} with the configured through MULTIO_SERVER_CONFIG_PATH (which might be the basepath of
// MULTIO_SERVER_CONFIG_FILE). All other names are looked up in the environment directly.
//
// Usually we would use eckit::Resource, however we have not adopted the usage of a multio home (i.e. /etc/multio) yet
// and probably don't want to - alot of other users probably don't want to adopt this approach. Moreover to allow
// looking environment variables or cli arguments, for eckit::Resource would enforce us to construct a string like
// "var;$var;-var" which will be reparsed again instead of passing 3 arguments directly...
std::string MultioConfiguration::replaceCurly(const std::string& s) const {
    return util::replaceCurly(s, [this](std::string_view replace) {
        if (replace == "~") {
            return std::optional<std::string>{this->configDir_.asString()};
        }
        std::string lookUpKey{replace};
        auto env = util::getEnv(lookUpKey);
        if (env) {
            return std::optional<std::string>{*env};
        }
        else {
            return std::optional<std::string>{};
        }
    });
}

const std::vector<message::MetadataMapping>& MultioConfiguration::getMetadataMappings(
    const std::string& mappings) const {
    return metadataMappings_.getMappings(*this, mappings);
};


std::weak_ptr<CFailureInteroperator> MultioConfiguration::getCFailureInteroperator() const {
    return cFailureInteroperator_;
}


//=============================================================================

MultioConfigurationHolder::MultioConfigurationHolder(MultioConfiguration&& multioConf) :
    multioConf_(std::move(multioConf)) {}

MultioConfigurationHolder::MultioConfigurationHolder(MultioConfiguration&& multioConf, LocalPeerTag peerTag) :
    multioConf_(std::move(multioConf)) {
    multioConf_.setLocalPeerTag(peerTag);
}

MultioConfiguration& MultioConfigurationHolder::multioConfig() noexcept {
    return multioConf_;
};


//=============================================================================

}  // namespace multio::config

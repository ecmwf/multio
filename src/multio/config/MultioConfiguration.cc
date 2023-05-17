
#include "multio/config/MultioConfiguration.h"
#include "multio/config/MetadataMappings.h"

#include "multio/util/Environment.h"
#include "multio/util/Substitution.h"


using namespace multio::config;

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

namespace multio::config {

// MultioConfiguration
MultioConfiguration::MultioConfiguration(const eckit::LocalConfiguration& globalYAMLConfig, const eckit::PathName& pathName,
                             const eckit::PathName& fileName, LocalPeerTag localPeerTag) :
    globalYAML_(globalYAMLConfig), pathName_(pathName), fileName_(fileName), localPeerTag_(localPeerTag) {}

MultioConfiguration::MultioConfiguration(const eckit::PathName& pathName, const eckit::PathName& fileName,
                             LocalPeerTag localPeerTag) :
    MultioConfiguration::MultioConfiguration(eckit::LocalConfiguration{eckit::YAMLConfiguration{fileName}}, pathName, fileName,
                                 localPeerTag) {}

MultioConfiguration::MultioConfiguration(const eckit::PathName& fileName, LocalPeerTag localPeerTag) :
    MultioConfiguration::MultioConfiguration(eckit::LocalConfiguration{eckit::YAMLConfiguration{fileName}},
                                 configuration_path_name(fileName), fileName, localPeerTag) {}

const eckit::LocalConfiguration& MultioConfiguration::YAML() const {
    return globalYAML_;
};
const eckit::PathName& MultioConfiguration::pathName() const {
    return pathName_;
};
MultioConfiguration& MultioConfiguration::setPathName(const eckit::PathName& pathName) {
    pathName_ = pathName;
    return *this;
};
const eckit::PathName& MultioConfiguration::fileName() const {
    return fileName_;
};

LocalPeerTag MultioConfiguration::localPeerTag() const {
    return localPeerTag_;
};
bool MultioConfiguration::isServer() const {
    return localPeerTag() == LocalPeerTag::Server;
};
bool MultioConfiguration::isClient() const {
    return localPeerTag() == LocalPeerTag::Client;
};

MultioConfiguration& MultioConfiguration::setLocalPeerTag(LocalPeerTag clientOrServer) {
    localPeerTag_ = clientOrServer;
    return *this;
};
MultioConfiguration& MultioConfiguration::tagServer() {
    return setLocalPeerTag(LocalPeerTag::Server);
};
MultioConfiguration& MultioConfiguration::tagClient() {
    return setLocalPeerTag(LocalPeerTag::Client);
};



const std::optional<MPIInitInfo>& MultioConfiguration::getMPIInitInfo() const {
    return mpiInitInfo_;
};
std::optional<MPIInitInfo>& MultioConfiguration::getMPIInitInfo() {
    return mpiInitInfo_;
};
MultioConfiguration& MultioConfiguration::setMPIInitInfo(const std::optional<MPIInitInfo>& val) {
    mpiInitInfo_ = val;
    return *this; 
};

const YAMLFile& MultioConfiguration::getYAMLFile(const char* fname) const {
    return getYAMLFile(std::string(fname));
}
const YAMLFile& MultioConfiguration::getYAMLFile(const std::string& fname) const {
    return getYAMLFile(eckit::PathName{replaceCurly(fname)});
}
const YAMLFile& MultioConfiguration::getRelativeYAMLFile(const eckit::PathName& referedFrom, const char* fname) const {
    return getRelativeYAMLFile(referedFrom, std::string(fname));
}
const YAMLFile& MultioConfiguration::getRelativeYAMLFile(const eckit::PathName& referedFrom, const std::string& fname) const {
    return getYAMLFile(referedFrom / fname);
}
const YAMLFile& MultioConfiguration::getYAMLFile(const eckit::PathName& fname) const {
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
            return std::optional<std::string>{this->pathName_.asString()};
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

const MetadataMappings& MultioConfiguration::metadataMappings() const {
    if (!metadataMappings_) {
        metadataMappings_.emplace(*this);
    }
    return *metadataMappings_;
};

}  // namespace multio::config

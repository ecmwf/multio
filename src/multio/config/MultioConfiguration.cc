
#include "eckit/utils/Tokenizer.h"


#include "multio/config/MetadataMappings.h"
#include "multio/config/MultioConfiguration.h"

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


namespace {

eckit::LocalConfiguration configureFromSinks(const eckit::LocalConfiguration& sinksConf) {
    std::vector<eckit::LocalConfiguration> actions;
    actions.push_back(sinksConf);
    actions[0].set("type", "sink");

    std::vector<eckit::LocalConfiguration> plans;
    plans.push_back(eckit::LocalConfiguration{});
    plans[0].set("actions", actions);

    eckit::LocalConfiguration cfg;
    cfg.set("plans", plans);

    return cfg;
}


ConfigAndPaths configureFromEnv(config::LocalPeerTag tag) {
    ConfigPaths paths = defaultConfigPaths();

    // Servers only use default environment variables for default
    if (tag == LocalPeerTag::Server) {
        return ConfigAndPaths{paths, eckit::LocalConfiguration{eckit::YAMLConfiguration{paths.configFile}}};
    }

    // Clients can be used in legacy mode to be constructed from sinks or plans configurations with a range of different
    // envs to check If no default config file is given, all these different options are checked
    if (paths.configFile.exists()) {
        return ConfigAndPaths{paths, eckit::LocalConfiguration{eckit::YAMLConfiguration{paths.configFile}}};
    }


    if (::getenv("MULTIO_PLANS")) {
        std::string cfg(::getenv("MULTIO_PLANS"));
        std::cout << "MultIO initialising with plans " << cfg << std::endl;
        paths.configDir = "";
        return ConfigAndPaths{paths, eckit::LocalConfiguration{eckit::YAMLConfiguration(cfg)}};
    }

    if (::getenv("MULTIO_PLANS_FILE")) {
        eckit::PathName filePath(::getenv("MULTIO_PLANS_FILE"));
        std::cout << "MultIO initialising with plans file " << filePath << std::endl;

        auto paths2 = defaultConfigPaths(filePath);
        return ConfigAndPaths{paths2, eckit::LocalConfiguration{eckit::YAMLConfiguration{paths2.configDir}}};
    }

    // IFS Legacy
    if (::getenv("MULTIO_CONFIG")) {
        std::string cfg(::getenv("MULTIO_CONFIG"));
        std::cout << "MultIO initialising with config " << cfg << std::endl;
        paths.configDir = "";
        return ConfigAndPaths{paths, configureFromSinks(eckit::LocalConfiguration{eckit::YAMLConfiguration(cfg)})};
    }

    if (::getenv("MULTIO_CONFIG_FILE")) {
        eckit::PathName filePath(::getenv("MULTIO_CONFIG_FILE"));
        std::cout << "MultIO initialising with config file " << filePath << std::endl;

        auto paths2 = defaultConfigPaths(filePath);
        return ConfigAndPaths{
            paths2, configureFromSinks(eckit::LocalConfiguration{eckit::YAMLConfiguration{paths2.configDir}})};
    }

    eckit::Tokenizer parse(":");

    eckit::StringList sinks;
    parse(::getenv("MULTIO_SINKS") ? ::getenv("MULTIO_SINKS") : "fdb5", sinks);

    ASSERT(sinks.size());

    std::ostringstream oss;

    oss << "{ \"sinks\" : [";

    const char* sep = "";
    for (eckit::StringList::iterator i = sinks.begin(); i != sinks.end(); ++i) {
        oss << sep << "{ \"type\" : \"" << *i << "\"";
        oss << "}";
        sep = ",";
    }
    oss << "] }";

    std::cout << "MultIO initialising with $MULTIO_SINKS " << oss.str() << std::endl;

    std::istringstream iss(oss.str());
    paths.configDir = "";
    return ConfigAndPaths{paths, configureFromSinks(eckit::LocalConfiguration{eckit::YAMLConfiguration(iss)})};
}
}  // namespace

// MultioConfiguration
MultioConfiguration::MultioConfiguration(const eckit::LocalConfiguration& globalConfig,
                                         const eckit::PathName& configDir, const eckit::PathName& configFile,
                                         LocalPeerTag localPeerTag) :
    configDir_{configDir}, configFile_{configFile}, localPeerTag_{localPeerTag} {
        parsedConfig_ = replaceAllCurly(globalConfig);
    }

MultioConfiguration::MultioConfiguration(const eckit::PathName& configDir, const eckit::PathName& configFile,
                                         LocalPeerTag localPeerTag) :
    MultioConfiguration::MultioConfiguration(eckit::LocalConfiguration{eckit::YAMLConfiguration{configFile}}, configDir,
                                             configFile, localPeerTag) {}

MultioConfiguration::MultioConfiguration(const eckit::PathName& configFile, LocalPeerTag localPeerTag) :
    MultioConfiguration::MultioConfiguration(eckit::LocalConfiguration{eckit::YAMLConfiguration{configFile}},
                                             configuration_path_name(configFile), configFile, localPeerTag) {}

MultioConfiguration::MultioConfiguration(const eckit::LocalConfiguration& globalConfig, LocalPeerTag localPeerTag) :
    configDir_{}, configFile_{}, localPeerTag_{localPeerTag} {
        parsedConfig_ = replaceAllCurly(globalConfig);
    }


MultioConfiguration::MultioConfiguration(ConfigAndPaths c, LocalPeerTag localPeerTag) :
    configDir_{c.paths.configDir}, configFile_{c.paths.configFile}, localPeerTag_{localPeerTag} {
        const auto& cfg = c.parsedConfig;
        parsedConfig_ = replaceAllCurly(cfg);
    }

MultioConfiguration::MultioConfiguration(LocalPeerTag localPeerTag) :
    MultioConfiguration(configureFromEnv(localPeerTag), localPeerTag) {}

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

void MultioConfiguration::replaceAllCurly(eckit::LocalConfiguration& cfg) const {
    for (auto& key : cfg.keys()) {
        if (cfg.isString(key)) {
            cfg.set(key, replaceCurly(cfg.getString(key)));
        }
        if (cfg.isStringList(key)) {
            std::vector<std::string> tmp;
            for (const auto& str : cfg.getStringVector(key)) {
                tmp.push_back(replaceCurly(str));
            }
            cfg.set(key, tmp);
        }
        if (cfg.isSubConfiguration(key)) {
            const auto& sub = cfg.getSubConfiguration(key);
            cfg.set(key, replaceAllCurly(sub));
        }
        if (cfg.isSubConfigurationList(key)) {
            std::vector<eckit::LocalConfiguration> tmp;
            for (const auto& sub : cfg.getSubConfigurations(key)) {
                tmp.push_back(replaceAllCurly(sub));
            }
            cfg.set(key, tmp);
        }
    }
}

eckit::LocalConfiguration MultioConfiguration::replaceAllCurly(const eckit::LocalConfiguration& cfg) const {
    eckit::LocalConfiguration tmp = cfg;
    replaceAllCurly(tmp);
    return tmp;
}

const std::vector<message::MetadataMapping>& MultioConfiguration::getMetadataMappings(
    const std::string& mappings) const {
    return metadataMappings_.getMappings(*this, mappings);
};


//-----------------------------------------------------------------------------

std::queue<message::Message>& MultioConfiguration::debugSink() const {
    return debugSink_;
};

//-----------------------------------------------------------------------------

MultioConfigurationHolder::MultioConfigurationHolder(MultioConfiguration&& multioConf) :
    multioConf_(std::move(multioConf)) {}

MultioConfigurationHolder::MultioConfigurationHolder(MultioConfiguration&& multioConf, LocalPeerTag peerTag) :
    multioConf_(std::move(multioConf)) {
    multioConf_.setLocalPeerTag(peerTag);
}

MultioConfiguration& MultioConfigurationHolder::multioConfig() noexcept {
    return multioConf_;
};


//-----------------------------------------------------------------------------

}  // namespace multio::config

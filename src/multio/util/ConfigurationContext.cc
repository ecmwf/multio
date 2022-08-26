
#include "ConfigurationContext.h"

namespace multio {
namespace util {


SubContextIteratorMapper::SubContextIteratorMapper(const ConfigurationContext& confCtx) : confCtx_(confCtx) {}
SubContextIteratorMapper::SubContextIteratorMapper(ConfigurationContext&& confCtx) : confCtx_(std::move(confCtx)) {}

ConfigurationContext SubContextIteratorMapper::operator()(const eckit::LocalConfiguration& config) const {
        return confCtx_.recast(config);
}


ConfigurationContext::ConfigurationContext(const eckit::LocalConfiguration& config,
                                           const eckit::LocalConfiguration& globalConfig,
                                           const eckit::PathName& pathName,
                                           const eckit::PathName& fileName) :
    config_(config), globalConfig_(globalConfig), pathName_(pathName), fileName_(fileName) {}

ConfigurationContext::ConfigurationContext(const eckit::LocalConfiguration& config,
                                           const eckit::PathName& pathName,
                                           const eckit::PathName& fileName) :
    ConfigurationContext::ConfigurationContext(config, config, pathName, fileName) {}

ConfigurationContext::ConfigurationContext(const eckit::PathName& pathName,
                                           const eckit::PathName& fileName) :
    ConfigurationContext::ConfigurationContext(
        eckit::LocalConfiguration{eckit::YAMLConfiguration{fileName}}, pathName, fileName) {}

eckit::LocalConfiguration& ConfigurationContext::config() {
    return config_;
};
const eckit::LocalConfiguration& ConfigurationContext::config() const {
    return config_;
};

const eckit::LocalConfiguration& ConfigurationContext::globalConfig() const {
    return globalConfig_;
};

const eckit::PathName& ConfigurationContext::pathName() const {
    return pathName_;
};

const eckit::PathName& ConfigurationContext::fileName() const {
    return fileName_;
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
    return ConfigurationContext(config, globalConfig_, pathName_, fileName_);
};


}  // namespace util
}  // namespace multio

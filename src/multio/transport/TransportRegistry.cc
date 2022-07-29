
#include "TransportRegistry.h"

#include "eckit/config/YAMLConfiguration.h"
#include "multio/util/ConfigurationPath.h"

namespace multio {
namespace transport {

using util::configuration_file;

TransportRegistry& TransportRegistry::instance() {
    static TransportRegistry singleton;
    return singleton;
}

std::shared_ptr<transport::Transport> TransportRegistry::get(const eckit::Configuration& config) {

    auto serverName = config.getString("target");
    add(serverName);

    return transports_.at(serverName);
}

void TransportRegistry::openConnections() {
    std::lock_guard<std::mutex> lock{mutex_};

    for (const auto& tr : transports_) {
        tr.second->openConnections();
    }
}

void TransportRegistry::closeConnections() {
    std::lock_guard<std::mutex> lock{mutex_};

    for (const auto& tr : transports_) {
        tr.second->closeConnections();
    }
}

void TransportRegistry::abort(const std::string& serverName) {
    std::lock_guard<std::mutex> lock{mutex_};
    transports_.at(serverName)->abort();
}

void TransportRegistry::abortAll() {
    for (const auto& tr : transports_) {
            abort(tr.first);
    }
}

void TransportRegistry::add(const std::string& serverName) {
    std::lock_guard<std::mutex> lock{mutex_};

    if (transports_.find(serverName) != std::end(transports_)) {
        return;
    }

    eckit::LocalConfiguration fullConfig{eckit::YAMLConfiguration{configuration_file()}};
    auto serverConfig = fullConfig.getSubConfiguration(serverName);
    transports_.insert({serverName, std::shared_ptr<transport::Transport>{
                                        transport::TransportFactory::instance().build(
                                            serverConfig.getString("transport"), serverConfig)}});
}

}  // namespace action
}  // namespace multio

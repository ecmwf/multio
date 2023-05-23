
#include "TransportRegistry.h"

namespace multio::transport {

TransportRegistry& TransportRegistry::instance() {
    static TransportRegistry singleton;
    return singleton;
}

std::shared_ptr<transport::Transport> TransportRegistry::get(const ComponentConfiguration& compConf) {

    auto serverName = compConf.parsedConfig().getString("target");
    add(serverName, compConf);

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

void TransportRegistry::add(const std::string& serverName, const ComponentConfiguration& compConf) {
    std::lock_guard<std::mutex> lock{mutex_};

    if (transports_.find(serverName) != std::end(transports_)) {
        return;
    }

    if (!compConf.multioConfig().parsedConfig().has(serverName)) {
        std::ostringstream oss;
        oss << "No config for server \"" << serverName << "\" found in configuration " << compConf.multioConfig().configFile()
            << std::endl;
        throw TransportException(oss.str(), Here());
    }
    auto serverConfigCtx = compConf.recast(compConf.multioConfig().parsedConfig().getSubConfiguration(serverName),
                                           config::ComponentTag::Transport);
    if (!serverConfigCtx.parsedConfig().has("transport")) {
        std::ostringstream oss;
        oss << "No key \"transport\" in server config for server \"" << serverName
            << "\" found (Configuration filename:  " << compConf.multioConfig().configFile() << ")" << std::endl;
        throw TransportException(oss.str(), Here());
    }
    transports_.insert({serverName, std::shared_ptr<transport::Transport>{transport::TransportFactory::instance().build(
                                        serverConfigCtx.parsedConfig().getString("transport"), serverConfigCtx)}});
}


}  // namespace multio::transport

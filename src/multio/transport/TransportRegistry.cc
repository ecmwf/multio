
#include "TransportRegistry.h"
#include "eckit/exception/Exceptions.h"

namespace multio {
namespace transport {

TransportRegistry& TransportRegistry::instance() {
    static TransportRegistry singleton;
    return singleton;
}

std::shared_ptr<transport::Transport> TransportRegistry::get(const ConfigurationContext& confCtx) {

    auto serverName = confCtx.config().getString("target");
    add(serverName, confCtx);

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

void TransportRegistry::add(const std::string& serverName, const ConfigurationContext& confCtx) {
    std::lock_guard<std::mutex> lock{mutex_};

    if (transports_.find(serverName) != std::end(transports_)) {
        return;
    }

    if (!confCtx.globalConfig().has(serverName)) {
        std::ostringstream oss;
        oss << "No config for server \"" << serverName << "\" found in configuration " << confCtx.fileName() << std::endl;
        throw eckit::Exception(oss.str());
    }
    auto serverConfigCtx = confCtx.recast(confCtx.globalConfig().getSubConfiguration(serverName));    
    if (!serverConfigCtx.config().has("transport")) {
        std::ostringstream oss;
        oss << "No key \"transport\" in server config for server \"" << serverName << "\" found (Configuration filename:  " << confCtx.fileName() << ")" << std::endl;
        throw eckit::Exception(oss.str());
    }
    transports_.insert({serverName, std::shared_ptr<transport::Transport>{
                                        transport::TransportFactory::instance().build(
                                            serverConfigCtx.config().getString("transport"), serverConfigCtx)}});
}


}  // namespace action
}  // namespace multio

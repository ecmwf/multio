/// @author Domokos Sarmany

/// @date Jan 2022

#pragma once

#include "multio/config/ComponentConfiguration.h"
#include "multio/transport/Transport.h"  // This means circular dependency at the minute


namespace eckit {
class Configuration;
}

namespace multio::transport {

using message::Message;
using config::ComponentConfiguration;


class TransportRegistry : public eckit::NonCopyable {
public:
    static TransportRegistry& instance();

    std::shared_ptr<transport::Transport> get(const ComponentConfiguration& config);

    void openConnections();
    void closeConnections();

    void abort(const std::string& serverName);
    void abortAll();

private:
    void add(const std::string& serverName, const ComponentConfiguration& fullConfig);

    std::map<std::string, std::shared_ptr<transport::Transport>> transports_;
    std::mutex mutex_;
};

}  // namespace multio::transport

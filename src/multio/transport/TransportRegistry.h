/// @author Domokos Sarmany

/// @date Jan 2022

#pragma once

#include "multio/config/ComponentConfiguration.h"
#include "multio/transport/Transport.h"  // This means circular dependency at the minute


namespace eckit {
class Configuration;
}

namespace multio::transport {

using config::ComponentConfiguration;
using message::Message;


class TransportRegistry : public eckit::NonCopyable {
public:
    static TransportRegistry& instance();

    std::shared_ptr<transport::Transport> get(const ComponentConfiguration& config);

    void openConnections();
    void closeConnections();

    void abort(const std::string& serverName, std::exception_ptr);
    void abortAll(std::exception_ptr);

    void synchronize();

    void print() {
        std::lock_guard<std::mutex> lock{mutex_};
        std::cout << " + TransportRegistry::TranposrtSize" << transports_.size() << std::endl;
        for ( const auto& [k,v] : transports_ ){
            std::cout << " + TransportRegistry::TransportPrint :: " << k << std::endl;
        }

    };


    void print( const std::string& prefix ) {
        std::lock_guard<std::mutex> lock{mutex_};
        std::cerr << prefix << "::TransportRegistry::TranposrtSize" << transports_.size() << std::endl;
        for ( const auto& [k,v] : transports_ ){
            std::cerr << prefix << "::TransportRegistry::TransportPrint :: " << k << std::endl;
        }

    };

private:
    void add(const std::string& serverName, const ComponentConfiguration& fullConfig);

    std::map<std::string, std::shared_ptr<transport::Transport>> transports_;
    std::mutex mutex_;
};

}  // namespace multio::transport

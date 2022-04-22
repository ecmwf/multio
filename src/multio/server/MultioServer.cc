
#include "MultioServer.h"

#include "eckit/config/YAMLConfiguration.h"
#include "eckit/filesystem/PathName.h"

#include "multio/LibMultio.h"
#include "multio/transport/Transport.h"

namespace multio {
namespace server {

using transport::TransportFactory;

MultioServer::MultioServer(const eckit::Configuration& config) :
    transport_{TransportFactory::instance().build(config.getString("transport"), config)},
    listener_{config, *transport_} {
    eckit::Log::info() << "Server config: " << config << std::endl;
    //LOG_DEBUG_LIB(multio::LibMultio) << "Server config: " << config << std::endl;
    listener_.start();
    eckit::Log::info() << "Listening loop has stopped" << std::endl;
}

MultioServer::~MultioServer() = default;

}  // namespace server
}  // namespace multio

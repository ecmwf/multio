
#include "MultioServer.h"

#include "eckit/config/YAMLConfiguration.h"
#include "eckit/filesystem/PathName.h"

#include "multio/LibMultio.h"
#include "multio/server/Transport.h"

namespace {
eckit::PathName configuration_path() {
    eckit::PathName base = (::getenv("MULTIO_SERVER_PATH"))
                               ? eckit::PathName{::getenv("MULTIO_SERVER_PATH")}
                               : eckit::PathName{""};

    return base + "/configs/multio-server.yaml";
}
}  // namespace

namespace multio {
namespace server {

MultioServer::MultioServer() : MultioServer{eckit::YAMLConfiguration{configuration_path()}} {}

MultioServer::MultioServer(const eckit::Configuration& config) :
    transport_{TransportFactory::instance().build(config.getString("transport"), config)},
    listener_{config, *transport_} {
    eckit::Log::debug<multio::LibMultio>() << config << std::endl;
    listener_.listen();
}

MultioServer::~MultioServer() = default;

}  // namespace server
}  // namespace multio

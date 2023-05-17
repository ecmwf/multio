
#include "MultioServer.h"

#include <fstream>
#include <iomanip>

#include "eckit/config/YAMLConfiguration.h"
#include "eckit/filesystem/PathName.h"
#include "eckit/log/Statistics.h"
#include "eckit/types/DateTime.h"

#include "multio/LibMultio.h"
#include "multio/transport/Transport.h"
#include "multio/util/logfile_name.h"

namespace multio::server {

using transport::TransportFactory;

MultioServer::MultioServer(const ServerConfiguration& compConf) :
    FailureAware(compConf),
    transport_{TransportFactory::instance().build(compConf.YAML().getString("transport"),
                                                  compConf.recast(config::ComponentTag::Transport))},
    listener_{compConf.recast(config::ComponentTag::Receiver), *transport_} {
    ASSERT(compConf.componentTag() == config::ComponentTag::Server);
    LOG_DEBUG_LIB(multio::LibMultio) << "Server config: " << compConf.YAML() << std::endl;
    eckit::Log::info() << "*** Server -- constructor " << compConf.YAML() << std::endl;

    std::ofstream logFile{util::logfile_name(), std::ios_base::app};

    struct ::timeval tstamp;
    ::gettimeofday(&tstamp, 0);
    auto mSecs = tstamp.tv_usec;

    logFile << "MultioServer starts at " << eckit::DateTime{static_cast<double>(tstamp.tv_sec)}.time().now() << ":"
            << std::setw(6) << std::setfill('0') << mSecs << " -- ";


    eckit::Log::info() << "Server start listening..." << std::endl;
    withFailureHandling([&]() { listener_.start(); });
    eckit::Log::info() << "Listening loop has stopped" << std::endl;
}

util::FailureHandlerResponse MultioServer::handleFailure(util::OnServerError t, const util::FailureContext& c,
                                                         util::DefaultFailureState&) const {
    // Last cascading instace - print nested contexts
    eckit::Log::error() << c;

    if (t == util::OnServerError::AbortTransport) {
        transport_->abort();
    }
    return util::FailureHandlerResponse::Rethrow;
};

MultioServer::~MultioServer() {
    std::ofstream logFile{util::logfile_name(), std::ios_base::app};

    struct ::timeval tstamp;
    ::gettimeofday(&tstamp, 0);
    auto mSecs = tstamp.tv_usec;

    logFile << "MultioServer stops at " << eckit::DateTime{static_cast<double>(tstamp.tv_sec)}.time().now() << ":"
            << std::setw(6) << std::setfill('0') << mSecs << std::endl;
}

}  // namespace multio::server

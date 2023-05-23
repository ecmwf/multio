
#include "MultioServer.h"

#include <fstream>
#include <iomanip>

#include "eckit/config/LocalConfiguration.h"
#include "eckit/filesystem/PathName.h"
#include "eckit/log/Statistics.h"
#include "eckit/types/DateTime.h"

#include "multio/LibMultio.h"
#include "multio/transport/Transport.h"
#include "multio/util/logfile_name.h"

namespace multio::server {

using config::ComponentConfiguration;
using transport::TransportFactory;


namespace {

eckit::LocalConfiguration getServerConf(const MultioConfiguration& multioConf) {
    if (multioConf.YAML().has("server")) {
        return multioConf.YAML().getSubConfiguration("server");
    }

    std::ostringstream oss;
    oss << "Configuration 'server' not found in configuration file " << multioConf.fileName();
    throw eckit::UserError(oss.str());
}

}  // namespace

MultioServer::MultioServer(const eckit::LocalConfiguration& conf, MultioConfiguration&& multioConf) :
    MultioConfigurationHolder(std::move(multioConf), config::LocalPeerTag::Server),
    FailureAware(config::ComponentConfiguration(conf, multioConfig(), config::ComponentTag::Server)),
    transport_{TransportFactory::instance().build(
        conf.getString("transport"), ComponentConfiguration(conf, multioConfig(), config::ComponentTag::Transport))},
    listener_{ComponentConfiguration(conf, multioConfig(), config::ComponentTag::Receiver), *transport_} {
    LOG_DEBUG_LIB(multio::LibMultio) << "Server config: " << conf << std::endl;
    eckit::Log::info() << "*** Server -- constructor " << conf << std::endl;

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

MultioServer::MultioServer(MultioConfiguration&& multioConf) :
    MultioServer(getServerConf(multioConf), std::move(multioConf)) {}

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

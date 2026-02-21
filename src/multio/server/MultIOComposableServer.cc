#include "MultIOComposableServer.h"

#include "eckit/exception/Exceptions.h"
#include "eckit/log/Log.h"
#include "eckit/config/Resource.h"

#include "multio/LibMultio.h"
#include "multio/transport/Transport.h"
#include "multio/transport/TransportRegistry.h"

namespace multio::server {

using config::ComponentConfiguration;
using transport::TransportFactory;

namespace impl {

std::size_t queueSize()
{
    return eckit::Resource<std::size_t>(
        "multioMessageQueueSize;$MULTIO_MESSAGE_QUEUE_SIZE",
        1024 * 1024);
}

std::size_t numDispatchers(const eckit::LocalConfiguration& c)
{
    return c.has("num-dispatchers") ?
           static_cast<std::size_t>(c.getLong("num-dispatchers")) :
           1;
}

bool enableProfiler(const eckit::LocalConfiguration& c)
{
    return c.has("enable-queue-profiler") && c.getBool("enable-queue-profiler");
}

std::chrono::milliseconds profilerPeriod(const eckit::LocalConfiguration& c)
{
    return std::chrono::milliseconds(
        c.has("queue-profiler-period-ms") ? c.getLong("queue-profiler-period-ms") : 1000);
}

std::size_t profilerBuckets(const eckit::LocalConfiguration& c)
{
    return c.has("queue-profiler-buckets") ?
           static_cast<std::size_t>(c.getLong("queue-profiler-buckets")) :
           10;
}

std::vector<std::unique_ptr<MultIODispatcherRunner>>
makeDispatchers(const config::ComponentConfiguration& compConf,
                multio::transport::Transport& transport,
                MultIOQueue& queue,
                MultIOProfilerState& profiler,
                std::size_t count)
{
    std::vector<std::unique_ptr<MultIODispatcherRunner>> dispatchers;
    dispatchers.reserve(count);

    for (std::size_t i = 0; i < count; ++i) {
        dispatchers.emplace_back(
            std::make_unique<MultIODispatcherRunner>(
                compConf,
                transport,
                queue,
                profiler,
                i));
    }

    return dispatchers;
}

MultIOProfiler makeProfiler(const MultIOProfilerState& profiler,
                            std::size_t queueCapacity,
                            const eckit::LocalConfiguration& c)
{
    return MultIOProfiler(
        profiler,
        queueCapacity,
        profilerPeriod(c),
        profilerBuckets(c));
}

eckit::LocalConfiguration
getServerConf(const config::MultioConfiguration& multioConf)
{
    if (multioConf.parsedConfig().has("server")) {
        return multioConf.parsedConfig().getSubConfiguration("server");
    }

    throw eckit::UserError("Missing 'server' configuration");
}

}  // namespace impl





MultIOComposableServer::MultIOComposableServer(config::MultioConfiguration&& multioConf) :

    MultioConfigurationHolder(std::move(multioConf),
                              config::LocalPeerTag::Server),

    serverConf_(impl::getServerConf(multioConfig())),
    compConf_(serverConf_, multioConfig()),

    FailureAware(compConf_),

    hasProfiler_(impl::enableProfiler(serverConf_)),
    dispatcherCount_(impl::numDispatchers(serverConf_)),

    profilerState_(dispatcherCount_),
    queue_(impl::queueSize(), profilerState_),

    transportProgress_(compConf_, queue_, profilerState_),
    receiver_(compConf_,
          transportProgress_.transport(),
          queue_,
          profilerState_),
    dispatchers_(impl::makeDispatchers(compConf_,
                                 transportProgress_.transport(),
                                 queue_,
                                 profilerState_,
                                 dispatcherCount_)),
    profilerRunner_(impl::makeProfiler(profilerState_, queue_.capacity(), serverConf_))
{}


MultIOComposableServer::~MultIOComposableServer()
{
    profilerRunner_.stop();

    queue_.close();

    for (auto& t : threads_) {
        if (t.joinable()) {
            t.join();
        }
    }
}


void MultIOComposableServer::run()
{
    eckit::Log::info() << "ComposableServer starting..." << std::endl;

    threads_.emplace_back([&] { transportProgress_.run(); });

    threads_.emplace_back([&] { receiver_.run(); });

    for (auto& d : dispatchers_) {
        threads_.emplace_back([&] { d->run(); });
    }

    if (hasProfiler_) {
        threads_.emplace_back([&] { profilerRunner_.run(); });
    }

    for (auto& t : threads_) {
        t.join();
    }

    eckit::Log::info() << "ComposableServer stopped." << std::endl;
}


util::FailureHandlerResponse
MultIOComposableServer::handleFailure(util::OnServerError t,
                                       const util::FailureContext& c,
                                       util::DefaultFailureState&) const
{
    eckit::Log::error() << c << std::endl;
    eckit::Log::flush();

    if (t == util::OnServerError::AbortTransport) {
        transportProgress_.transport().abort(c.eptr);
    }
    queue_.interrupt(c.eptr);

    return util::FailureHandlerResponse::Rethrow;
}

}  // namespace multio::server